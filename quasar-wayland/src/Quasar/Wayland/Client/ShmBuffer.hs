module Quasar.Wayland.Client.ShmBuffer (
  -- * wl_shm
  newLocalShmPool,
  newLocalShmBuffer,
) where

import Control.Monad.Catch
import Foreign
import Quasar.Disposer.Rc
import Quasar.Prelude
import Quasar.Wayland.Shm
import Quasar.Wayland.Utils.SharedFd
import Quasar.Wayland.Utils.SharedMemory


newLocalShmPool :: Int32 -> IO (Rc ShmPool, ForeignPtr Word8)
newLocalShmPool size = do
  fd <- memfdCreate (fromIntegral size)

  ptr <- mmap MmapReadWrite fd (fromIntegral size)

  -- Passes ownership of the fd to the pool
  pool <- atomicallyC (newShmPool fd (pure size))
    `onException`
      (disposeSharedFd fd >> finalizeForeignPtr ptr)

  pure (pool, ptr)


newLocalShmBuffer ::
  Int32 -> Int32 -> IO (ShmBuffer, ForeignPtr Word32)
newLocalShmBuffer width height = do
  (pool, ptr) <- newLocalShmPool size

  atomicallyC do
    -- Buffer takes ownership of the pool
    buffer <- newShmBuffer pool offset width height stride pixelFormat

    pure (buffer, castForeignPtr ptr)

  where
    bytePerPixel = 4 :: Int32
    offset = 0 :: Int32
    stride = width * bytePerPixel
    size = width * height * bytePerPixel
    pixelFormat = 0 :: Word32 -- argb8888

