module Quasar.Wayland.Client.Buffer (
  -- * wl_shm
  ShmBufferManager(formats),
  newShmBufferManager,
  newShmPool,
  newShmBuffer,
) where

import Control.Monad.Catch
import Data.Set qualified as Set
import Foreign
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Utils.SharedMemory
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)


data ShmBufferManager = ShmBufferManager {
  wlShm :: Object 'Client Interface_wl_shm,
  formats :: Future (Set.Set Word32)
}

newShmBufferManager :: WaylandClient -> STM ShmBufferManager
newShmBufferManager client = do
  formatsVar <- newTVar mempty

  wlShm <- bindSingleton @Interface_wl_shm client.registry
  setEventHandler wlShm $ EventHandler_wl_shm {
    format = modifyTVar formatsVar . Set.insert
  }

  -- Formats are emittet all at once; sync ensures the list is complete
  formatListComplete <- client.sync
  let formats = formatListComplete >> unsafeAwaitSTM (readTVar formatsVar)

  pure ShmBufferManager {
    wlShm,
    formats
  }

data ShmPool = ShmPool {
  wlShmPool :: Object 'Client Interface_wl_shm_pool,
  shmPtr :: TVar (Maybe (ForeignPtr Word8))
}

newShmPool :: ShmBufferManager -> Int32 -> IO ShmPool
newShmPool shm size = do
  (wlShmPool, ptr) <- trySendShm size (\fd -> shm.wlShm.create_pool fd size)

  shmPtr <- newTVarIO (Just ptr)

  pure ShmPool {
    wlShmPool,
    shmPtr
  }


newShmBuffer
  :: ShmBufferManager
  -> Int32
  -> Int32
  -> IO (Object 'Client Interface_wl_buffer, ForeignPtr Word32)
newShmBuffer shm width height = do
  (wlShmPool, ptr) <- trySendShm size (\fd -> shm.wlShm.create_pool fd size)

  wlBuffer <- liftIO $ atomically do
    wlBuffer <- wlShmPool.create_buffer offset width height stride pixelFormat
    setEventHandler wlBuffer EventHandler_wl_buffer {
      -- TODO
      release = pure () -- wlBuffer.destroy
    }
    pure wlBuffer

  atomically wlShmPool.destroy

  pure (wlBuffer, castForeignPtr ptr)

  where
    bytePerPixel = 4
    offset = 0
    stride = width * bytePerPixel
    size = width * height * bytePerPixel
    pixelFormat = 0 -- argb8888


trySendShm :: Int32 -> (Fd -> STM a) -> IO (a, ForeignPtr Word8)
trySendShm size sendAction = do
  fd <- memfdCreate $ fromIntegral size

  -- Has to be created before sending since the fd will be closed when sent
  ptr <- mmap fd $ fromIntegral size

  -- Fd ownership is transferred to the outbox (it will be closed after it has
  -- been sent)
  result <- atomically (sendAction fd)
    `onException`
      (closeFd fd >> finalizeForeignPtr ptr)

  pure (result, ptr)
