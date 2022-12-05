module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.JuicyPixels
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shm
import Quasar.Wayland.Surface

import Codec.Picture


main :: IO ()
main = do
  _ <- runQuasarAndExit (stderrLogger LogLevelWarning) do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    configurationVar <- newEmptyTMVarIO

    tl <- atomically do
      clientWindowManager <- getClientWindowManager @ShmBufferBackend client
      tl <- newWindow clientWindowManager (writeTMVar configurationVar)
      setTitle tl "quasar-wayland-example-client"
      pure tl

    forM_ [gradient, gradient2, gradient3, gradient4] \img -> do
      -- Blocks until first configure event
      configuration <- (atomically $ readTMVar configurationVar)
      let width = max configuration.width 512
      let height = max configuration.height 512
      buffer <- liftIO $ toImageBuffer (mkImage width height img)
      atomically do
        commitWindowContent tl configuration.configureSerial (defaultSurfaceCommit DamageAll) {
          buffer = Just buffer
        }
        destroyBuffer buffer

      await =<< newDelay 1000000

    --join $ atomically do

    --  (surface, wlSurface) <- newClientSurface @ShmBufferBackend client
    --  (surface2, wlSurface2) <- newClientSurface @ShmBufferBackend client

    --  wlrLayerShell <- bindSingleton @Interface_zwlr_layer_shell_v1 client.registry

    --  configuredVar <- newTVar False
    --  unmappedVar <- newTVar False
    --  configuredVar2 <- newTVar False
    --  unmappedVar2 <- newTVar False

    --  wlrLayerSurface <- wlrLayerShell.get_layer_surface wlSurface Nothing 2 "demo"
    --  setMessageHandler wlrLayerSurface EventHandler_zwlr_layer_surface_v1 {
    --    configure = \serial _width _height -> do
    --        whenM (not <$> readTVar unmappedVar) do
    --          wlrLayerSurface.ack_configure serial
    --        writeTVar configuredVar True,
    --    closed = pure ()
    --  }
    --  wlrLayerSurface.set_size 512 512
    --  wlrLayerSurface.set_anchor 1

    --  wlrLayerSurface2 <- wlrLayerShell.get_layer_surface wlSurface2 Nothing 2 "demo"
    --  setMessageHandler wlrLayerSurface2 EventHandler_zwlr_layer_surface_v1 {
    --    configure = \serial _width _height -> do
    --        whenM (not <$> readTVar unmappedVar2) do
    --          wlrLayerSurface2.ack_configure serial
    --        writeTVar configuredVar2 True,
    --    closed = pure ()
    --  }
    --  wlrLayerSurface2.set_size 512 512
    --  wlrLayerSurface2.set_anchor 2

    --  -- Commit role
    --  wlSurface.commit
    --  wlSurface2.commit
    --  -- Should await first `configure` event

    --  pure do
    --    buffer <- liftIO $ toImageBuffer (mkImage gradient)
    --    buffer2 <- liftIO $ toImageBuffer (mkImage solidColor)

    --    atomically do
    --      check =<< readTVar configuredVar
    --      check =<< readTVar configuredVar2
    --      commitSurface surface SurfaceCommit {
    --        buffer = Just buffer,
    --        offset = (0, 0),
    --        bufferDamage = DamageAll
    --      }
    --      commitSurface surface2 SurfaceCommit {
    --        buffer = Just buffer2,
    --        offset = (0, 0),
    --        bufferDamage = DamageList [Rectangle 0 0 42 42]
    --      }
    --      destroyBuffer buffer
    --      destroyBuffer buffer2

    --    await =<< newDelay 100000
    --    traceIO "Waiting 2s"
    --    await =<< newDelay 2000000

    --    atomically do
    --      commitSurface surface SurfaceCommit {
    --        buffer = Nothing,
    --        offset = (0, 0),
    --        bufferDamage = DamageList []
    --      }
    --      writeTVar unmappedVar True

    --    -- await =<< newDelay 100000
    --    atomically do
    --      commitSurface surface2 SurfaceCommit {
    --        buffer = Nothing,
    --        offset = (0, 0),
    --        bufferDamage = DamageList []
    --      }
    --      writeTVar unmappedVar2 True

    await =<< newDelay 1000000
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
    aspect = (fromIntegral width) / (fromIntegral height)

mkPosition :: Dimensions -> Int -> Int -> Position
mkPosition dimensions pixelX pixelY = Position { dimensions, pixelX, pixelY, u, v, x, y }
  where
    width' = dimensions.width
    height' = dimensions.height
    u :: Double
    u = (fromIntegral pixelX) / (fromIntegral width')
    v :: Double
    v = (fromIntegral pixelY) / (fromIntegral height')
    innerRadius :: Int
    innerRadius = div (min width' height') 2
    x :: Double
    x = (fromIntegral $ pixelX - (div width' 2)) / (fromIntegral innerRadius)
    y :: Double
    y = (fromIntegral $ pixelY - (div height' 2)) / (fromIntegral innerRadius)

color :: forall a. RealFrac a => a -> a -> a -> PixelRGBA8
color r g b = PixelRGBA8 (toWord r) (toWord g) (toWord b) 255
  where
    toWord :: a -> Word8
    toWord = truncate . (* 255) . (max 0) . (min 1)

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

mkImage :: Int32 -> Int32 -> (Position -> PixelRGBA8) -> Image PixelRGBA8
mkImage width height fn = generateImage pixel (fromIntegral width) (fromIntegral height)
  where
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGBA8
    pixel x y = fn $ mkPosition dimensions x y

mkImage' :: (Position -> PixelRGBA8) -> Image PixelRGBA8
mkImage' fn = generateImage pixel width height
  where
    width :: Int
    width = 512
    height :: Int
    height = 512
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGBA8
    pixel x y = fn $ mkPosition dimensions x y
