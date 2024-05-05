module Quasar.Wayland.Shm (
  IsShmBufferBackend,
  ShmBufferBackend(..),
  ShmPool,
  ShmBuffer,
  newShmPool,
  resizeShmPool,
  destroyShmPool,
  newShmBuffer,
) where

import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Set (Set)
import Data.Set qualified as Set
import Quasar.Future
import Quasar.Prelude
import Quasar.Resources
import Quasar.Resources.DisposableVar
import Quasar.Resources.Rc
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Utils.Resources

-- | Simple buffer backend that only supports shared memory buffers.
data ShmBufferBackend = ShmBufferBackend

data ShmBufferFrame = ShmBufferFrame TDisposer (Rc (Borrowed ShmBuffer))

instance Disposable ShmBufferFrame where
  getDisposer (ShmBufferFrame tdisposer _) = getDisposer tdisposer

instance RenderBackend ShmBufferBackend where
  type Frame ShmBufferBackend = ShmBufferFrame

type IsShmBufferBackend b = IsBufferBackend ShmBuffer b

instance IsBufferBackend ShmBuffer ShmBufferBackend where
  type ExternalBuffer ShmBuffer ShmBufferBackend = Borrowed ShmBuffer
  newExternalBuffer :: ShmBufferBackend -> Borrowed ShmBuffer -> STMc NoRetry '[] (Borrowed ShmBuffer)
  newExternalBuffer ShmBufferBackend borrowed = pure borrowed
  createExternalBufferFrame :: ShmBufferBackend -> TDisposer -> Rc (Borrowed ShmBuffer) -> STMc NoRetry '[] ShmBufferFrame
  createExternalBufferFrame ShmBufferBackend frameRelease rc =
    pure (ShmBufferFrame frameRelease rc)

-- | Wrapper for an externally managed shm pool
data ShmPool = ShmPool {
  key :: Unique,
  fd :: TVar (Maybe SharedFd),
  size :: TVar Int32,
  bufferCount :: TVar Word32,
  destroyRequested :: TVar Bool,
  destroyed :: TVar Bool,
  downstreams :: TVar [DownstreamShmPool]
}

instance Eq ShmPool where
  x == y = x.key == y.key

instance Hashable ShmPool where
  hash pool = hash pool.key
  hashWithSalt salt pool = hashWithSalt salt pool.key

data DownstreamShmPool = DownstreamShmPool {
  disposer :: TDisposer,
  resize :: Int32 -> STMc NoRetry '[SomeException] ()
}


newtype ShmBuffer = ShmBuffer (TDisposableVar ShmBufferState)
  deriving (Eq, Hashable, Disposable, TDisposable)

data ShmBufferState = ShmBufferState {
  pool :: ShmPool,
  offset :: Int32,
  width :: Int32,
  height :: Int32,
  stride :: Int32,
  format :: Word32
}

-- | Create an `ShmPool` for externally managed memory. Takes ownership of the
-- passed file descriptor. Needs to be destroyed with `destroyShmPool` when no
-- longer required.
newShmPool :: SharedFd -> Int32 -> STMc NoRetry '[] ShmPool
newShmPool fd size = do
  key <- newUniqueSTM
  fdVar <- newTVar (Just fd)
  sizeVar <- newTVar size
  bufferCount <- newTVar 0
  destroyRequested <- newTVar False
  destroyed <- newTVar False
  downstreams <- newTVar mempty
  pure ShmPool {
    key,
    fd = fdVar,
    size = sizeVar,
    bufferCount,
    destroyRequested,
    destroyed,
    downstreams
  }

-- | Resize an externally managed shm pool.
resizeShmPool :: ShmPool -> Int32 -> STMc NoRetry '[SomeException] ()
resizeShmPool pool size = do
  oldSize <- readTVar pool.size
  when (oldSize > size) $ throwM $ ProtocolUsageError (mconcat ["wl_shm: Invalid resize from ", show oldSize, " to ", show size])
  writeTVar pool.size size
  downstreams <- readTVar pool.downstreams
  mapM_ (\downstream -> downstream.resize size) downstreams

-- | Request destruction of an an externally managed shm pool. Memory shared
-- with this pool will be deallocated after all buffer is released.
destroyShmPool :: ShmPool -> STMc NoRetry '[] ()
destroyShmPool pool = do
  writeTVar pool.destroyRequested True
  tryFinalizeShmPool pool

tryFinalizeShmPool :: ShmPool -> STMc NoRetry '[] ()
tryFinalizeShmPool pool = do
  destroyRequested <- readTVar pool.destroyRequested
  bufferCount <- readTVar pool.bufferCount
  when (destroyRequested && bufferCount == 0) do
    writeTVar pool.destroyed True
    fd <- swapTVar pool.fd Nothing
    downstreams <- swapTVar pool.downstreams mempty
    mapM_ (disposeTDisposer . (.disposer)) downstreams
    traceM "Finalized ShmPool"
    -- TODO close fd
    forM_ fd \fd' -> traceM $ "leaking fd fd@" <> show fd' <> " (needs to be deferred to IO)"

connectDownstreamShmPool :: ShmPool -> DownstreamShmPool -> STMc NoRetry '[SomeException] ()
connectDownstreamShmPool pool downstream = do
  whenM (readTVar pool.destroyed) $ throwM $ userError "ShmPool: Cannot attach downstream since the pool has been destroyed"
  modifyTVar pool.downstreams (downstream:)


-- | Create a new buffer for an externally managed pool
newShmBuffer ::
  ShmPool -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> STMc NoRetry '[] ShmBuffer
newShmBuffer pool offset width height stride format = do
  -- TODO check arguments
  modifyTVar pool.bufferCount succ
  ShmBuffer <$> newTDisposableVar (ShmBufferState pool offset width height stride format) releaseShmBuffer
  where
    releaseShmBuffer :: ShmBufferState -> STMc NoRetry '[] ()
    releaseShmBuffer buffer = do
      modifyTVar buffer.pool.bufferCount pred
      traceM "Finalized ShmBuffer"
      tryFinalizeShmPool buffer.pool


-- * Wayland client

instance ClientBufferBackend ShmBufferBackend where

  type ClientBufferManager ShmBufferBackend = ClientShmManager
  type RenderedFrame ShmBufferBackend = ShmBufferFrame
  type ExportBufferId ShmBufferBackend = Unique

  newClientBufferManager = newClientShmManager

  renderFrame :: Rc ShmBufferFrame -> IO (Rc ShmBufferFrame)
  renderFrame = pure

  getExportBufferId :: ShmBufferFrame -> STMc NoRetry '[] Unique
  getExportBufferId (ShmBufferFrame _ rc) =
    tryReadRc rc >>= \case
      Nothing -> undefined -- "ShmBufferBackend: Trying to get export id for a disposed frame"
      Just (Borrowed _ (ShmBuffer var)) -> pure (disposerElementKey var)

  exportWlBuffer :: ClientShmManager -> ShmBufferFrame -> IO (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer client (ShmBufferFrame _ rc) = atomicallyC do
    tryReadRc rc >>= \case
      Nothing -> throwM (userError "ShmBufferBackend: Trying to export already disposed frame")
      Just (Borrowed _ (ShmBuffer var)) -> do
        tryReadTDisposableVar var >>= \case
          Nothing -> throwM (userError "ShmBufferBackend: Trying to export already disposed buffer")
          Just state -> do
            wlShmPool <- getClientShmPool client state.pool
            -- NOTE no event handlers are attached here, since the caller (usually `Quasar.Wayland.Surface`) has that responsibility.
            wlShmPool.create_buffer state.offset state.width state.height state.stride state.format

  syncExportBuffer :: ShmBufferFrame -> IO ()
  syncExportBuffer _ = pure ()

  getExportBufferDestroyedFuture :: ShmBufferFrame -> STMc NoRetry '[] (Future '[] ())
  getExportBufferDestroyedFuture (ShmBufferFrame _ shmBuffer) = pure $ isDisposed shmBuffer

data ClientShmManager = ClientShmManager {
  key :: Unique,
  wlShm :: Object 'Client Interface_wl_shm,
  wlShmPools :: TVar (HashMap ShmPool (Object 'Client Interface_wl_shm_pool)),
  formats :: FutureEx '[SomeException] (Set Word32)
}

instance Eq ClientShmManager where
  x == y = x.key == y.key

instance Hashable ClientShmManager where
  hash x = hash x.key
  hashWithSalt salt x = hashWithSalt salt x.key


newClientShmManager :: WaylandClient -> STMc NoRetry '[SomeException] ClientShmManager
newClientShmManager client = do
  key <- newUniqueSTM
  wlShm <- bindSingleton client.registry maxVersion
  wlShmPools <- newTVar mempty
  formatsVar <- newTVar mempty
  formats <- newPromise
  setEventHandler wlShm $ EventHandler_wl_shm {
    format = \fmt -> modifyTVar formatsVar (Set.insert fmt)
  }
  -- Formats are emittet all at once; sync ensures the list is complete
  formatListComplete <- liftSTMc client.sync
  callOnceCompleted_ formatListComplete \case
    Right () -> tryFulfillPromise_ formats . Right =<< readTVar formatsVar
    Left ex -> tryFulfillPromise_ formats (Left ex)

  pure ClientShmManager {
    key,
    wlShm,
    wlShmPools,
    formats = toFutureEx formats
  }

getClientShmPool :: ClientShmManager -> ShmPool -> STMc NoRetry '[SomeException] (Object 'Client Interface_wl_shm_pool)
getClientShmPool client pool = do
  readTVar client.wlShmPools >>= \pools -> case HM.lookup pool pools of
    Just wlShmPool -> pure wlShmPool
    Nothing -> do
      wlShmPool <- exportClientShmPool client pool
      modifyTVar client.wlShmPools (HM.insert pool wlShmPool)
      pure wlShmPool

exportClientShmPool :: ClientShmManager -> ShmPool -> STMc NoRetry '[SomeException] (Object 'Client Interface_wl_shm_pool)
exportClientShmPool client pool = do
  readTVar pool.fd >>= \case
    Nothing -> throwM $ userError "Cannot export finalized ShmPool"
    Just fd -> do
      size <- readTVar pool.size
      -- TODO attach downstream to propagate size changes and pool destruction
      -- TODO (then: remove downstream when client is closed)
      wlShmPool <- client.wlShm.create_pool fd size
      disposer <- newUnmanagedTDisposer (tryCall wlShmPool.destroy)
      connectDownstreamShmPool pool DownstreamShmPool {
        disposer,
        resize = wlShmPool.resize
      }
      pure wlShmPool
