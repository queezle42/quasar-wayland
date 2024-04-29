module Quasar.Wayland.Utils.SharedFd (
  SharedFd,
  newSharedFd,
  duplicateSharedFd,

  withSharedFd,
  withSharedFds,

  disposeSharedFd,
  unshareSharedFd,
  dupSharedFdRaw,
) where

import Control.Exception (finally, mask_)
import GHC.Stack (callStack, prettyCallStack)
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Prelude hiding (dup)
import Quasar.Resources (Disposable (getDisposer), dispose)
import Quasar.Resources.DisposableVar
import System.Posix.IO (closeFd, dup)
import System.Posix.Types (Fd)

-- A ref-counted file descriptor container.
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
data SharedFd = SharedFd (DisposableVar FdRc) String

instance Disposable SharedFd where
  getDisposer (SharedFd var _) = getDisposer var

instance Show SharedFd where
  show (SharedFd _ showData) = showData

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
      traceIO $ mconcat ["SharedFd was garbage collected, closing Fd@", show fd]
      closeFd fd

incRc :: FdRc -> STMc NoRetry '[SomeException] ()
incRc (FdRc var) = modifyTVar var \(fd, active) -> (fd, active + 1)

getFdRc :: HasCallStack => SharedFd -> STMc NoRetry '[SomeException] FdRc
getFdRc (SharedFd var _) =
  tryReadDisposableVar var >>= \case
    Nothing -> throwSTM (userError (unlines [ "SharedFd has already been disposed", prettyCallStack callStack]))
    Just x -> pure x

-- | Create a new `SharedFd`, passing ownership of a file descriptor to a
-- SharedFd.
--
-- The caller is responsible for disposing the SharedFd (see `disposeSharedFd`
-- and `unshareSharedFd`).
newSharedFd :: Fd -> IO SharedFd
newSharedFd fd = do
  rc <- newFdRc fd
  -- TODO
  var <- newFnDisposableVarIO loggingExceptionSink (decRc closeFd (const (pure ()))) rc
  pure (SharedFd var ("Fd@" <> show fd))

-- | @withSharedFd fn sfd@ executes the computation @fn@, passing the underlying
-- file descriptor of @sfd@ to @fn@.
--
-- The file descriptor will not be closed while @fn@ is running.
--
-- @fn@ must not close the file descriptor.
withSharedFd :: forall a. HasCallStack => SharedFd -> (Fd -> IO a) -> IO a
withSharedFd (SharedFd var _) fn = mask_ do
  -- This function will:
  -- > Increment the active refcount, so disposing the `SharedFd`-object during
  --   `withSharedFd` does not result in a memory error.
  -- > Call the provided `fn` function, passing the file descriptor.
  -- > Decrement the active refcount again, disposing the fd if necessary (if
  --   no `SharedFd`-object point to the file descriptor).
  join $ atomically do
    tryReadDisposableVar var >>= \case
      Nothing -> throwSTM (userError (unlines ["Failed to lock SharedFd because it is already disposed", prettyCallStack callStack]))
      Just rc@(FdRc rcVar) -> do
        readTVar rcVar >>= \case
          (_, (< 1) -> True) -> unreachableCodePathM -- Refcount invariant violated
          (Nothing, _) -> unreachableCodePathM -- Reference invariant violated
          (Just fd, n) -> go rc fd <$ writeTVar rcVar (Just fd, n + 1)
  where
    go :: FdRc -> Fd -> IO a
    go rc fd = fn fd `finally` decRc closeFd (const (pure ())) rc

withSharedFds :: HasCallStack => [SharedFd] -> ([Fd] -> IO a) -> IO a
withSharedFds [] fn = fn []
withSharedFds (x:xs) fn = withSharedFd x \fd -> withSharedFds xs (fn . (fd :))

-- | Create a new `SharedFd` that references the same file descriptor as another
-- SharedFd.
duplicateSharedFd :: SharedFd -> STMc NoRetry '[SomeException] SharedFd
duplicateSharedFd fd@(SharedFd _ showData) = do
  rc <- getFdRc fd
  incRc rc
  var <- newFnDisposableVar loggingExceptionSink (decRc closeFd (const (pure ()))) rc
  pure (SharedFd var ("Fd@" <> show fd))

-- | Releases the reference to the underlying file descriptor. If this was the
-- last reference, the file descriptor is closed.
--
-- Invalidates the `SharedFd` object (i.e. future calls to `withSharedFd` or
-- `duplicateSharedFd` will fail).
--
-- Idempotent.
disposeSharedFd :: SharedFd -> IO ()
disposeSharedFd (SharedFd var _) = dispose var

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
unshareSharedFd :: HasCallStack => SharedFd -> IO Fd
unshareSharedFd s@(SharedFd var _) = do
  rc <- atomicallyC do
    rc <- getFdRc s
    incRc rc
    pure rc

  dispose s

  decRcAndGetFd rc

-- | Aquires a copy of the underlying file descriptor (see @dup(2)@).
--
-- This function passes ownership of the returned fd to the caller, who becomes
-- responsible for closing it.
--
-- Will throw an exception if `disposeSharedFd` or `unshareSharedFd` have been
-- called on this `SharedFd` before.
dupSharedFdRaw :: HasCallStack => SharedFd -> IO Fd
dupSharedFdRaw (SharedFd var _) = do
  join $ atomicallyC $ tryReadDisposableVar var >>= \case
    Just rc -> do
      -- Increase refcount as part of the STM transaction to prevent fd from
      -- being closed from another thread before extracting/duplicating the fd.
      incRc rc
      pure $ decRcAndGetFd rc
    Nothing -> do
      pure $ throwIO (userError (unlines ["Failed to export SharedFd because it is already disposed", prettyCallStack callStack]))


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

-- Decrements the refcount and gets an fd. The caller becomes responsible for
-- closing the fd.
decRcAndGetFd :: FdRc -> IO Fd
decRcAndGetFd =
  -- TODO use @dup3@ with FD_CLOEXEC?
  decRc pure dup
