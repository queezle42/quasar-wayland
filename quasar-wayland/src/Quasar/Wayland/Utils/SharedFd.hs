module Quasar.Wayland.Utils.SharedFd (
  SharedFd,
  newSharedFd,
  duplicateSharedFd,

  withSharedFd,

  disposeSharedFd,
  unshareSharedFd,
) where

import Control.Exception (finally, mask_)
import Quasar.Prelude hiding (dup)
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd)

newtype FdRc = FdRc (TVar (Maybe Fd, Word32))

-- | Shared reference to a file descriptor.
--
-- This is a reference counting container that points a file descriptor,
-- allowing it to be duplicated from `STM` (see `duplicateSharedFd`).
--
-- It is possible to retrieve a file descriptor that refers to the underlying
-- open file description using `unshareSharedFd`, and to temporarily receive the
-- file descriptor by using `withSharedFd`.
--
-- The underlying file descriptor is closed when all `SharedFd`s created by
-- duplication are disposed (see `disposeSharedFd` and `unshareSharedFd`).
data SharedFd = SharedFd (TVar (Maybe FdRc)) String

instance Show SharedFd where
  show (SharedFd var showData) = showData

newFdRc :: Fd -> IO FdRc
newFdRc fd = do
  var <- newTVarIO (Just fd, 1)
  let rc = FdRc var
  void $ mkWeakTVar var (finalizeFdRcStore rc)
  pure rc

finalizeFdRcStore :: FdRc -> IO ()
finalizeFdRcStore (FdRc var) =
  mapM_ finalizer . fst =<< atomically (swapTVar var (Nothing, 0))
  where
    finalizer :: Fd -> IO ()
    finalizer fd = do
      traceIO $ mconcat ["Leaked SharedFd, closing Fd@", show fd]
      closeFd fd

incRc :: FdRc -> STM ()
incRc (FdRc var) = modifyTVar var \(fd, active) -> (fd, active + 1)

getFdRc :: SharedFd -> STM FdRc
getFdRc (SharedFd var _) =
  readTVar var >>= \case
    Nothing -> throwSTM (userError "SharedFd has already been disposed")
    Just x -> pure x

-- | Create a new `SharedFd`, passing ownership of a file descriptor to a
-- SharedFd.
--
-- The caller is responsible for disposing the SharedFd (see `disposeSharedFd`
-- and `unshareSharedFd`).
newSharedFd :: Fd -> IO SharedFd
newSharedFd fd = do
  rc <- newFdRc fd
  var <- newTVarIO (Just rc)
  pure (SharedFd var ("Fd@" <> show fd))

-- | @withSharedFd fn sfd@ executes the computation @fn@, passing the underlying
-- file descriptor of @sfd@ to @fn@.
--
-- The file descriptor will not be closed while @fn@ is running.
--
-- @fn@ must not close the file descriptor.
withSharedFd :: SharedFd -> (Fd -> IO ()) -> IO ()
withSharedFd (SharedFd var _) fn = mask_ do
  -- This function will:
  --
  -- - Increment the active refcount, so disposing the `SharedFd`-object during
  --   `withSharedFd` does not result in a memory error.
  --
  -- - Call the provided `fn` function, passing the file descriptor.
  --
  -- - Decrement the active refcount again, disposing the fd if necessary (if
  --   no `SharedFd`-object point to the file descriptor).
  join $ atomically do
    readTVar var >>= \case
      Nothing -> throwSTM (userError "Failed to lock SharedFd because it is already disposed")
      Just rc@(FdRc rcVar) -> do
        readTVar rcVar >>= \case
          (_, (< 1) -> True) -> unreachableCodePathM -- Refcount invariant violated
          (Nothing, _) -> unreachableCodePathM -- Reference invariant violated
          (Just fd, n) -> go rc fd <$ writeTVar rcVar (Just fd, n + 1)
  where
    go :: FdRc -> Fd -> IO ()
    go rc fd = fn fd `finally` decRc closeFd (const (pure ())) rc

-- | Create a new `SharedFd` that references the same file descriptor as another
-- SharedFd.
duplicateSharedFd :: SharedFd -> STM SharedFd
duplicateSharedFd fd@(SharedFd _ showData) = do
  rc <- getFdRc fd
  incRc rc
  var <- newTVar (Just rc)
  pure (SharedFd var showData)

-- | Releases the reference to the underlying file descriptor. If this was the
-- last reference, the file descriptor is closed.
--
-- Invalidates the `SharedFd` object (i.e. future calls to `withSharedFd` or
-- `duplicateSharedFd` will fail).
--
-- Idempotent.
disposeSharedFd :: SharedFd -> IO ()
disposeSharedFd (SharedFd var _) = mask_ do
  atomically (swapTVar var Nothing) >>= \case
    Just rc -> decRc closeFd (const (pure ())) rc
    Nothing -> pure ()

-- | Releases the reference to the underlying file descriptor. If this was the
-- last reference, the file descriptor is returned. Otherwise a duplicated fd
-- is created (see @dup(2)@).
--
-- This function passes ownership of the returned fd to the caller, who becomes
-- responsible for closing it.
--
-- Invalidates the `SharedFd` object (i.e. future calls to `withSharedFd` or
-- `duplicateSharedFd` will fail).
--
-- Will throw an exception if `disposeSharedFd` or `unshareSharedFd` have been
-- called on this `SharedFd` before.
unshareSharedFd :: SharedFd -> IO Fd
unshareSharedFd (SharedFd var _) = do
  atomically (swapTVar var Nothing) >>= \case
    -- TODO use @dup3@ with FD_CLOEXEC?
    Just rc -> decRc pure dup rc
    Nothing -> throwIO (userError "Failed to unshare SharedFd because it is already disposed")

-- Decrements the refcount and passes the fd to either @lastFn@ (if the refcount
-- was 1) or @decFn@ (if the refcount was larger).
decRc :: (Fd -> IO a) -> (Fd -> IO a) -> FdRc -> IO a
decRc lastFn decFn (FdRc rcVar) =
  join $ atomically do
    readTVar rcVar >>= \case
      (_, (< 1) -> True) -> unreachableCodePathM -- Refcount invariant violated
      (Nothing, _) -> unreachableCodePathM -- Reference invariant violated
      (Just fd, 1) -> writeTVar rcVar (Nothing, 0) >> pure (lastFn fd)
      (m@(Just fd), n) -> writeTVar rcVar (m, n - 1) >> pure (decFn fd)
