module Quasar.Wayland.Shm (
  ShmBufferBackend,
  ShmPool,
  newShmPool,
  resizeShmPool,
  destroyShmPool,
  newShmBuffer,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Surface
import Quasar.Wayland.Client.Surface
import System.Posix (Fd)

data ShmBufferBackend

instance BufferBackend ShmBufferBackend where
  type BufferContent ShmBufferBackend = ShmBuffer
  releaseBufferStorage buffer = do
    modifyTVar buffer.pool.bufferCount pred
    traceM "Finalized ShmBuffer"
    tryFinalizeShmPool buffer.pool

instance ClientBufferBackend ShmBufferBackend where
  type ClientBufferManager ShmBufferBackend = ShmClient
  exportWlBuffer = undefined

data ShmClient = ShmClient {
}




-- | Wrapper for an externally managed shm pool
data ShmPool = ShmPool {
  fd :: TVar (Maybe Fd),
  size :: TVar Int32,
  bufferCount :: TVar Word32,
  destroyed :: TVar Bool,
  downstreams :: TVar [DownstreamShmPool]
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
  fdVar <- newTVar (Just fd)
  sizeVar <- newTVar size
  bufferCount <- newTVar 0
  destroyed <- newTVar False
  downstreams <- newTVar mempty
  pure ShmPool {
    fd = fdVar,
    size = sizeVar,
    bufferCount,
    destroyed,
    downstreams
  }

-- | Resize an externally managed shm pool.
resizeShmPool :: ShmPool -> Int32 -> STM ()
resizeShmPool pool size = do
  oldSize <- readTVar pool.size
  when (oldSize > size) $ throwM $ ProtocolUsageError (mconcat ["wl_shm: Invalid resize from ", show oldSize, " to ", show size])
  writeTVar pool.size size

-- | Destroy an externally managed shm pool. Memory shared to this pool will be deallocated after the last buffer is released.
destroyShmPool :: ShmPool -> STM ()
destroyShmPool pool = do
  alreadyDestroyed <- swapTVar pool.destroyed True
  unless alreadyDestroyed do
    tryFinalizeShmPool pool

tryFinalizeShmPool :: ShmPool -> STM ()
tryFinalizeShmPool pool = do
  destroyed <- readTVar pool.destroyed
  bufferCount <- readTVar pool.bufferCount
  when (destroyed && bufferCount == 0) do
    fd <- swapTVar pool.fd Nothing
    traceM "Finalized ShmPool"
    -- TODO close fd
    traceM $ "leaking fd " <> show fd <> " (closing fd is not implemented yet)"


-- | Create a new buffer for an externally managed pool
newShmBuffer :: ShmPool -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> STM (Buffer ShmBufferBackend)
newShmBuffer pool offset width height stride format = do
  -- TODO check arguments
  modifyTVar pool.bufferCount succ
  let shmBuffer = ShmBuffer pool offset width height stride format
  newBuffer @ShmBufferBackend shmBuffer

data DownstreamShmPool = DownstreamShmPool

connectDownstreamShmPool :: ShmPool -> DownstreamShmPool -> STM ()
connectDownstreamShmPool pool downstream = undefined
