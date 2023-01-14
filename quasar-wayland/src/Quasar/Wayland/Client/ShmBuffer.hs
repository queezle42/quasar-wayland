module Quasar.Wayland.Client.ShmBuffer (
  -- * wl_shm
  newLocalShmPool,
  newLocalShmBuffer,
) where

import Control.Monad.Catch
import Foreign
import Quasar.Prelude
import Quasar.Wayland.Shm
import Quasar.Wayland.Surface
import Quasar.Wayland.Utils.SharedFd
import Quasar.Wayland.Utils.SharedMemory


newLocalShmPool :: Int32 -> IO (ShmPool, ForeignPtr Word8)
newLocalShmPool size = do
  fd <- memfdCreate (fromIntegral size)

  ptr <- mmap MmapReadWrite fd (fromIntegral size)

  -- Passes ownership of the fd to the pool
  pool <- atomically (newShmPool fd size)
    `onException`
      (disposeSharedFd fd >> finalizeForeignPtr ptr)

  pure (pool, ptr)


newLocalShmBuffer
  :: Int32
  -> Int32
  -> IO (Buffer ShmBufferBackend, ForeignPtr Word32)
newLocalShmBuffer width height = do
  (pool, ptr) <- newLocalShmPool size

  atomically do
    buffer <- newShmBuffer pool offset width height stride pixelFormat

    -- Pool won't be reused
    destroyShmPool pool

    pure (buffer, castForeignPtr ptr)

  where
    bytePerPixel = 4
    offset = 0
    stride = width * bytePerPixel
    size = width * height * bytePerPixel
    pixelFormat = 0 -- argb8888

