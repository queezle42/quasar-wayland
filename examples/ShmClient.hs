module Main (main) where

import Codec.Picture
import Data.Bits
import Foreign
import Quasar
import Quasar.Disposer.Rc
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Backend
import Quasar.Wayland.Client
import Quasar.Wayland.Client.ShmBuffer
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi
import Quasar.Wayland.Shm


main :: IO ()
main = do
  _ <- runQuasarAndExit do
    backend <- swallowDisposerIO $ newRcIO (pure ShmBufferBackend)

    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    configurationVar <- newEmptyTMVarIO
    closeRequestedVar <- newTVarIO False

    let properties = defaultWindowProperties {
      title = "quasar-wayland-example-client"
    }

    window <- atomicallyC do
      windowManager <- getClientWindowManager @ShmBufferBackend client
      newWindow windowManager properties (writeTMVar configurationVar) \case
        WindowRequestClose -> writeTVar closeRequestedVar True

    forM_ [gradient, red, green, blue, alpha, transparent, gradient, gradient2, gradient3, gradient4] \img -> do
      -- Blocks until first configure event
      configuration <- atomically $ readTMVar configurationVar
      let width = max configuration.width 512
      let height = max configuration.height 512
      rawBuffer <- liftIO $ renderImage backend (mkImage width height img)
      buffer <- newRcIO rawBuffer
      commit <- atomicallyC do
        commitWindow window configuration.configureSerial defaultWindowCommit $
          defaultSurfaceCommit @ShmBufferBackend buffer

      delay <- newDelay 1000000
      await commit
      await delay

    traceIO "Closing"
  traceIO "Closed"


-- * Buffer content generation (using JuicyPixels)

data Dimensions = Dimensions {
  width :: Int,
  height :: Int,
  aspect :: Double
}

data Position = Position {
  dimensions :: Dimensions,
  pixelX :: Int,
  pixelY :: Int,
  u :: Double,
  v :: Double,
  x :: Double,
  y :: Double
}

mkDimensions :: Integral a => a -> a -> Dimensions
mkDimensions width height = Dimensions { width = fromIntegral width, height = fromIntegral height, aspect }
  where
    aspect :: Double
    aspect = fromIntegral width / fromIntegral height

mkPosition :: Dimensions -> Int -> Int -> Position
mkPosition dimensions pixelX pixelY = Position { dimensions, pixelX, pixelY, u, v, x, y }
  where
    width' = dimensions.width
    height' = dimensions.height
    u :: Double
    u = fromIntegral pixelX / fromIntegral width'
    v :: Double
    v = fromIntegral pixelY / fromIntegral height'
    innerRadius :: Int
    innerRadius = div (min width' height') 2
    x :: Double
    x = fromIntegral (pixelX - (width' `div` 2)) / fromIntegral innerRadius
    y :: Double
    y = fromIntegral (pixelY - (height' `div` 2)) / fromIntegral innerRadius

rgb :: Double -> Double -> Double -> PixelRGBA8
rgb r g b = PixelRGBA8 (toWord r) (toWord g) (toWord b) (toWord 1)

toWord :: Double -> Word8
toWord = truncate . (* 255) . max 0 . min 1

rgba :: Double -> Double -> Double -> Double -> PixelRGBA8
rgba r g b a = PixelRGBA8 (toWord (r * a)) (toWord (g * a)) (toWord (b * a)) (toWord a)

gradient :: Position -> PixelRGBA8
gradient p = rgb (u p) 0 (v p)

gradient2 :: Position -> PixelRGBA8
gradient2 p = rgb (v p) 0 (1 - u p)

gradient3 :: Position -> PixelRGBA8
gradient3 p = rgb (1 - u p) 0 (1 - v p)

gradient4 :: Position -> PixelRGBA8
gradient4 p = rgb (1 - v p) 0 (u p)

red :: Position -> PixelRGBA8
red = const (rgb 1 0 0)

green :: Position -> PixelRGBA8
green = const (rgb 0 1 0)

blue :: Position -> PixelRGBA8
blue = const (rgb 0 0 1)

alpha :: Position -> PixelRGBA8
alpha p = rgba 1 0 1 (v p)

transparent :: Position -> PixelRGBA8
transparent p = rgba 0 0 0 0

mkImage :: Int32 -> Int32 -> (Position -> PixelRGBA8) -> Image PixelRGBA8
mkImage width height fn = generateImage pixel (fromIntegral width) (fromIntegral height)
  where
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGBA8
    pixel x y = fn $ mkPosition dimensions x y

renderImage ::
  IsShmBufferBackend b =>
  Rc b -> Image PixelRGBA8 -> IO (Owned (Frame b))
renderImage backend image = do
  (buffer, ptr) <- newLocalShmBuffer (fromIntegral (imageWidth image)) (fromIntegral (imageHeight image))
  let
    width = imageWidth image
    height = imageHeight image

  withForeignPtr ptr \ptr' -> forM_ [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]] \(x, y) -> do
    pokeByteOff ptr' ((x + (y * width)) * 4) (pixelRgba8ToWlARGB (pixelAt image x y))

  atomicallyC (newFrameConsumeBuffer backend buffer)


pixelRgba8ToWlARGB :: PixelRGBA8 -> Word32
{-# INLINE pixelRgba8ToWlARGB #-}
pixelRgba8ToWlARGB (PixelRGBA8 r g b a) =
    (fi a `unsafeShiftL` (3 * bitCount)) .|.
    (fi r `unsafeShiftL` (2 * bitCount)) .|.
    (fi g `unsafeShiftL` (1 * bitCount)) .|.
    (fi b `unsafeShiftL` (0 * bitCount))
  where
    fi :: Pixel8 -> Word32
    fi = fromIntegral
    bitCount :: Int
    bitCount = 8
