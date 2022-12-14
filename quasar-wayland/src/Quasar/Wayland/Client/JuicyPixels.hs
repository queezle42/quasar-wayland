module Quasar.Wayland.Client.JuicyPixels (
  loadImageBuffer,
  toImageBuffer,
  pixelRgba8ToWlARGB,
) where

import Codec.Picture
import Foreign
import Quasar.Prelude
import Quasar.Wayland.Client.ShmBuffer
import Quasar.Wayland.Shm
import Quasar.Wayland.Surface

loadImageBuffer :: FilePath -> IO (Buffer ShmBufferBackend)
loadImageBuffer path = do
  image <- either fail (pure . convertRGBA8) =<< readImage path
  toImageBuffer image

toImageBuffer :: Image PixelRGBA8 -> IO (Buffer ShmBufferBackend)
toImageBuffer image = do
  (buffer, ptr) <- newLocalShmBuffer (fromIntegral (imageWidth image)) (fromIntegral (imageHeight image))
  let
    width = imageWidth image
    height = imageHeight image

  withForeignPtr ptr \ptr' -> forM_ [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]] \(x, y) -> do
    pokeByteOff ptr' ((x + (y * width)) * 4) (pixelRgba8ToWlARGB (pixelAt image x y))

  pure buffer


pixelRgba8ToWlARGB :: PixelRGBA8 -> Word32
{-# INLINE pixelRgba8ToWlARGB #-}
pixelRgba8ToWlARGB (PixelRGBA8 r g b a) =
    (fi b `unsafeShiftL` (0 * bitCount)) .|.
    (fi g `unsafeShiftL` (1 * bitCount)) .|.
    (fi r `unsafeShiftL` (2 * bitCount)) .|.
    (fi a `unsafeShiftL` (3 * bitCount))
  where
    fi :: Pixel8 -> Word32
    fi = fromIntegral
    bitCount :: Int
    bitCount = 8
