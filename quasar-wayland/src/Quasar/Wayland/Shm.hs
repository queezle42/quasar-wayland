module Quasar.Wayland.Shm (
  ShmBufferBackend,
  ShmPool,
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
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Surface
import Quasar.Wayland.Client.Surface
import System.Posix (Fd)

data ShmBufferBackend

instance BufferBackend ShmBufferBackend where
  type BufferStorage ShmBufferBackend = ShmBuffer

releaseShmBuffer :: ShmBuffer -> STM ()
releaseShmBuffer buffer = do
  modifyTVar buffer.pool.bufferCount pred
  traceM "Finalized ShmBuffer"
  tryFinalizeShmPool buffer.pool

-- | Wrapper for an externally managed shm pool
data ShmPool = ShmPool {
  key :: Unique,
  fd :: TVar (Maybe Fd),
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
  destroy :: STM (),
  resize :: Int32 -> STM ()
}


data ShmBuffer = ShmBuffer {
  pool :: ShmPool,
  offset :: Int32,
  width :: Int32,
  height :: Int32,
  stride :: Int32,
  format :: Word32
}

-- | Create an `ShmPool` for externally managed memory. Takes ownership of the passed file descriptor.
newShmPool :: Fd -> Int32 -> STM ShmPool
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
resizeShmPool :: ShmPool -> Int32 -> STM ()
resizeShmPool pool size = do
  oldSize <- readTVar pool.size
  when (oldSize > size) $ throwM $ ProtocolUsageError (mconcat ["wl_shm: Invalid resize from ", show oldSize, " to ", show size])
  writeTVar pool.size size
  downstreams <- readTVar pool.downstreams
  mapM_ (\downstream -> downstream.resize size) downstreams

-- | Request destruction of an an externally managed shm pool. Memory shared with this pool will be deallocated after the last buffer is released.
destroyShmPool :: ShmPool -> STM ()
destroyShmPool pool = do
  writeTVar pool.destroyRequested True
  tryFinalizeShmPool pool

tryFinalizeShmPool :: ShmPool -> STM ()
tryFinalizeShmPool pool = do
  destroyRequested <- readTVar pool.destroyRequested
  bufferCount <- readTVar pool.bufferCount
  when (destroyRequested && bufferCount == 0) do
    writeTVar pool.destroyed True
    fd <- swapTVar pool.fd Nothing
    downstreams <- swapTVar pool.downstreams mempty
    mapM_ (.destroy) downstreams
    traceM "Finalized ShmPool"
    -- TODO close fd
    forM_ fd \fd' -> traceM $ "leaking fd fd@" <> show fd' <> " (needs to be deferred to IO)"

connectDownstreamShmPool :: ShmPool -> DownstreamShmPool -> STM ()
connectDownstreamShmPool pool downstream = do
  whenM (readTVar pool.destroyed) $ throwM $ userError "ShmPool: Cannot attach downstream since the pool has been destroyed"
  modifyTVar pool.downstreams (downstream:)


-- | Create a new buffer for an externally managed pool
newShmBuffer :: ShmPool -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> STM (Buffer ShmBufferBackend)
newShmBuffer pool offset width height stride format = do
  -- TODO check arguments
  modifyTVar pool.bufferCount succ
  let shmBuffer = ShmBuffer pool offset width height stride format
  newBuffer @ShmBufferBackend shmBuffer (releaseShmBuffer shmBuffer)


-- * Wayland client

instance ClientBufferBackend ShmBufferBackend where

  type ClientBufferManager ShmBufferBackend = ClientShmManager

  newClientBufferManager = newClientShmManager

  exportWlBuffer :: ClientShmManager -> Buffer ShmBufferBackend -> STM (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer client buffer = do
    let shmBuffer = buffer.storage
    wlShmPool <- getClientShmPool client shmBuffer.pool
    -- NOTE no event handlers are attached here, since the caller (usually `Quasar.Wayland.Surface`) has that responsibility.
    wlShmPool.create_buffer shmBuffer.offset shmBuffer.width shmBuffer.height shmBuffer.stride shmBuffer.format

data ClientShmManager = ClientShmManager {
  key :: Unique,
  wlShm :: Object 'Client Interface_wl_shm,
  wlShmPools :: TVar (HashMap ShmPool (Object 'Client Interface_wl_shm_pool)),
  formats :: Future (Set Word32)
}

instance Eq ClientShmManager where
  x == y = x.key == y.key

instance Hashable ClientShmManager where
  hash x = hash x.key
  hashWithSalt salt x = hashWithSalt salt x.key


newClientShmManager :: WaylandClient -> STM ClientShmManager
newClientShmManager client = do
  key <- newUniqueSTM
  wlShm <- bindSingleton client.registry maxVersion
  wlShmPools <- newTVar mempty
  formatsVar <- newTVar mempty
  setEventHandler wlShm $ EventHandler_wl_shm {
    format = \fmt -> modifyTVar formatsVar (Set.insert fmt)
  }
  -- Formats are emittet all at once; sync ensures the list is complete
  formatListComplete <- client.sync
  -- Create awaitable from formats
  let formats = formatListComplete >> unsafeAwaitSTM (readTVar formatsVar)

  pure ClientShmManager {
    key,
    wlShm,
    wlShmPools,
    formats
  }

getClientShmPool :: ClientShmManager -> ShmPool -> STM (Object 'Client Interface_wl_shm_pool)
getClientShmPool client pool = do
  HM.lookup pool <$> readTVar client.wlShmPools >>= \case
    Just wlShmPool -> pure wlShmPool
    Nothing -> do
      wlShmPool <- exportClientShmPool client pool
      modifyTVar client.wlShmPools (HM.insert pool wlShmPool)
      pure wlShmPool

exportClientShmPool :: ClientShmManager -> ShmPool -> STM (Object 'Client Interface_wl_shm_pool)
exportClientShmPool client pool = do
  readTVar pool.fd >>= \case
    Nothing -> throwM $ userError "Cannot export finalized ShmPool"
    Just fd -> do
      size <- readTVar pool.size
      -- TODO attach downstream to propagate size changes and pool destruction
      -- TODO (then: remove downstream when client is closed)
      wlShmPool <- client.wlShm.create_pool fd size
      connectDownstreamShmPool pool DownstreamShmPool {
        destroy = wlShmPool.destroy,
        resize = wlShmPool.resize
      }
      pure wlShmPool
