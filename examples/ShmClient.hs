module Main (main) where

import Codec.Picture as JuicyPixels
import Quasar
import Quasar.Disposer.Rc (newRcIO)
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.JuicyPixels
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi
import Quasar.Wayland.Shm


main :: IO ()
main = do
  _ <- runQuasarAndExit do
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

    forM_ [solidColor, gradient, gradient2, gradient3, gradient4] \img -> do
      -- Blocks until first configure event
      configuration <- atomically $ readTMVar configurationVar
      let width = max configuration.width 512
      let height = max configuration.height 512
      rawBuffer <- liftIO $ toImage ShmBufferBackend (mkImage width height img)
      buffer <- newRcIO rawBuffer
      commit <- atomicallyC do
        commitWindowContent window configuration.configureSerial ((defaultSurfaceCommit @ShmBufferBackend buffer) {bufferDamage = Just DamageAll})

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

color :: forall a. RealFrac a => a -> a -> a -> PixelRGBA8
color r g b = PixelRGBA8 (toWord r) (toWord g) (toWord b) 255
  where
    toWord :: a -> Word8
    toWord = truncate . (* 255) . max 0 . min 1

gradient :: Position -> PixelRGBA8
gradient p = color (u p) 0 (v p)

gradient2 :: Position -> PixelRGBA8
gradient2 p = color (v p) 0 (1 - u p)

gradient3 :: Position -> PixelRGBA8
gradient3 p = color (1 - u p) 0 (1 - v p)

gradient4 :: Position -> PixelRGBA8
gradient4 p = color (1 - v p) 0 (u p)

solidColor :: Position -> PixelRGBA8
solidColor _p = color (255 :: Double) 0 0

mkImage :: Int32 -> Int32 -> (Position -> PixelRGBA8) -> JuicyPixels.Image PixelRGBA8
mkImage width height fn = generateImage pixel (fromIntegral width) (fromIntegral height)
  where
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGBA8
    pixel x y = fn $ mkPosition dimensions x y
