module Quasar.Wayland.Shm (
  IsShmBufferBackend,
  ShmPool(..),
  ShmBuffer(..),
  newShmPool,
  newShmBuffer,

  -- ** Client
  newClientShmManager,

  -- ** Simple shm rendering backend. Sufficient for a minimal example client,
  -- for other use-cases please consider @quasar-wayland-skia@.
  ShmBufferBackend(..),
) where

import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Set (Set)
import Data.Set qualified as Set
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Exceptions
import Quasar.Future
import Quasar.Observable.Core
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.Surface

-- | Simple buffer backend that only supports shared memory buffers.
data ShmBufferBackend = ShmBufferBackend

newtype ShmBufferFrame = ShmBufferFrame (ExternalFrame ShmBuffer ShmBufferBackend)

instance Disposable ShmBufferFrame where
  getDisposer (ShmBufferFrame external) = getDisposer external

instance RenderBackend ShmBufferBackend where
  type Frame ShmBufferBackend = ShmBufferFrame

type IsShmBufferBackend b = IsBufferBackend ShmBuffer b

instance IsBufferBackend ShmBuffer ShmBufferBackend where
  type ExternalBuffer ShmBuffer ShmBufferBackend = ShmBuffer

  newExternalBuffer :: ShmBufferBackend -> Owned ShmBuffer -> STMc NoRetry '[] (Owned ShmBuffer)
  newExternalBuffer ShmBufferBackend shmBuffer = pure shmBuffer

  wrapExternalFrame :: ExternalFrame ShmBuffer ShmBufferBackend -> STMc NoRetry '[DisposedException] (Owned (Frame ShmBufferBackend))
  wrapExternalFrame external = pure (ownedDisposable (ShmBufferFrame external))

-- | Wrapper for an externally managed shm pool
data ShmPool = ShmPool {
  key :: Unique,
  fd :: SharedFd,
  size :: Observable NoLoad '[] Int32
}

instance Disposable ShmPool where
  getDisposer pool = getDisposer pool.fd

instance Eq ShmPool where
  x == y = x.key == y.key

instance Hashable ShmPool where
  hash pool = hash pool.key
  hashWithSalt salt pool = hashWithSalt salt pool.key


data ShmBuffer = ShmBuffer {
  key :: Unique,
  pool :: ShmPool,
  offset :: Int32,
  width :: Int32,
  height :: Int32,
  stride :: Int32,
  format :: Word32
}

instance Eq ShmBuffer where
  x == y = x.key == y.key

instance Hashable ShmBuffer where
  hash x = hash x.key
  hashWithSalt salt x = hashWithSalt salt x.key

instance ToFuture '[] () ShmBuffer where
  toFuture x = toFuture (getDisposer x.pool)

-- | Create an `ShmPool` for externally managed memory. Takes ownership of the
-- passed file descriptor. Needs to be disposed when it is no longer required.
newShmPool :: SharedFd -> Observable NoLoad '[] Int32 -> STMc NoRetry '[] (Owned ShmPool)
newShmPool fd size = do
  key <- newUniqueSTM
  pure $ ownedDisposable ShmPool {
    key,
    fd,
    size
  }


-- | Create a new buffer for an externally managed pool
--
-- Takes ownership of the @Rc ShmPool@.
newShmBuffer ::
  Owned ShmPool -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> STMc NoRetry '[] (Owned ShmBuffer)
newShmBuffer ownedPool offset width height stride format = do
  key <- newUniqueSTM
  pure $ (\pool -> ShmBuffer key pool offset width height stride format) <$> ownedPool


-- * Wayland client

instance ClientBufferBackend ShmBufferBackend where

  type ClientBufferManager ShmBufferBackend = ClientShmManager
  type RenderedFrame ShmBufferBackend = ShmBufferFrame
  type ExportBufferId ShmBufferBackend = Unique

  newClientBufferManager = newClientShmManager

  renderFrame :: Rc ShmBufferFrame -> IO (Rc ShmBufferFrame)
  renderFrame = pure

  getExportBufferId :: ShmBufferFrame -> STMc NoRetry '[DisposedException] Unique
  getExportBufferId (ShmBufferFrame (ExternalFrame _ rc)) = do
    buffer <- readRc rc
    pure buffer.key

  exportWlBuffer :: ClientShmManager -> ShmBufferFrame -> IO (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer client (ShmBufferFrame (ExternalFrame _ rc)) = atomicallyC do
    buffer <- readRc rc
    wlShmPool <- getClientShmPool client buffer.pool
    -- NOTE no event handlers are attached here, since the caller (usually `Quasar.Wayland.Surface`) has that responsibility.
    wlShmPool.create_buffer buffer.offset buffer.width buffer.height buffer.stride buffer.format

  syncExportBuffer :: ShmBufferFrame -> IO ()
  syncExportBuffer _ = pure ()

  getExportBufferDestroyedFuture :: ShmBufferFrame -> STMc NoRetry '[] (Future '[] ())
  getExportBufferDestroyedFuture (ShmBufferFrame (ExternalFrame _ externalBuffer)) = pure $ isDisposed externalBuffer

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

-- | Gets the wayland client object for an `ShmPool`.
--
-- The `ShmPool` is borrowed for the duration of the call.
getClientShmPool :: ClientShmManager -> ShmPool -> STMc NoRetry '[SomeException] (Object 'Client Interface_wl_shm_pool)
getClientShmPool client pool = do
  readTVar client.wlShmPools >>= \pools -> case HM.lookup pool pools of
    Just wlShmPool -> pure wlShmPool
    Nothing -> do
      wlShmPool <- exportClientShmPool
      modifyTVar client.wlShmPools (HM.insert pool wlShmPool)
      pure wlShmPool

  where
    exportClientShmPool :: STMc NoRetry '[SomeException] (Object 'Client Interface_wl_shm_pool)
    exportClientShmPool =
      mfix \wlShmPoolFix -> do
        (disposer, size) <- liftSTMc $ attachSimpleObserver pool.size \newSize -> do
          tryCall (wlShmPoolFix.resize newSize)
        wlShmPool <- client.wlShm.create_pool pool.fd size
        attachFinalizer wlShmPool do
          modifyTVar client.wlShmPools (HM.delete pool)
          disposeEventually_ disposer
        callOnceCompleted_ (getDisposer pool) \_ -> tryCall wlShmPool.destroy
        pure wlShmPool
