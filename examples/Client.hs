module Main (main) where

import Data.List (intersperse)
import Data.Foldable (toList)
import Quasar
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.JuicyPixels
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shm
import Quasar.Wayland.Surface

import Codec.Picture


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

mkDimensions :: Int -> Int -> Dimensions
mkDimensions width height = Dimensions { width, height, aspect }
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

solidColor :: Position -> PixelRGBA8
solidColor p = color 255 0 0

mkImage :: (Position -> PixelRGBA8) -> Image PixelRGBA8
mkImage fn = generateImage pixel width height
  where
    width :: Int
    width = 512
    height :: Int
    height = 512
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGBA8
    pixel x y = fn $ mkPosition dimensions x y


main :: IO ()
main = do
  _ <- runQuasarAndExit (stderrLogger LogLevelWarning) do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    join $ liftIO $ atomically do

      --xdgWmBase <- bindSingleton @Interface_xdg_wm_base client.registry
      --setMessageHandler xdgWmBase EventHandler_xdg_wm_base {
      --  ping = \serial -> xdgWmBase.pong serial
      --}

      --xdgToplevel <- xdgSurface.get_toplevel
      --setMessageHandler xdgToplevel EventHandler_xdg_toplevel {
      --  configure = \_ _ _ -> pure (),
      --  close = pure ()
      --}

      --xdgToplevel.set_title "foobar"


      (surface, wlSurface) <- newClientSurface @ShmBufferBackend client
      (surface2, wlSurface2) <- newClientSurface @ShmBufferBackend client


      wlrLayerShell <- bindSingleton @Interface_zwlr_layer_shell_v1 client.registry

      configuredVar <- newTVar False
      configuredVar2 <- newTVar False

      wlrLayerSurface <- wlrLayerShell.get_layer_surface wlSurface Nothing 2 "demo"
      setMessageHandler wlrLayerSurface EventHandler_zwlr_layer_surface_v1 {
        configure = \serial _width _height -> do
            wlrLayerSurface.ack_configure serial
            writeTVar configuredVar True,
        closed = pure ()
      }
      wlrLayerSurface.set_size 512 512
      wlrLayerSurface.set_anchor 1

      wlrLayerSurface2 <- wlrLayerShell.get_layer_surface wlSurface2 Nothing 2 "demo"
      setMessageHandler wlrLayerSurface2 EventHandler_zwlr_layer_surface_v1 {
        configure = \serial _width _height -> do
            wlrLayerSurface2.ack_configure serial
            writeTVar configuredVar2 True,
        closed = pure ()
      }
      wlrLayerSurface2.set_size 512 512
      wlrLayerSurface2.set_anchor 2

      -- Commit role
      wlSurface.commit
      wlSurface2.commit
      -- Should await first `configure` event

      pure do
        buffer <- liftIO $ toImageBuffer (mkImage gradient)
        buffer2 <- liftIO $ toImageBuffer (mkImage solidColor)

        liftIO $ atomically do
          check =<< readTVar configuredVar
          check =<< readTVar configuredVar2
          commitSurface surface SurfaceCommit {
            buffer = Just buffer,
            offset = (0, 0),
            bufferDamage = DamageAll
          }
          commitSurface surface2 SurfaceCommit {
            buffer = Just buffer2,
            offset = (0, 0),
            bufferDamage = DamageList [Rectangle 0 0 42 42]
          }
          --destroyBuffer buffer
          --destroyBuffer buffer2

        await =<< newDelay 100000
        traceIO "Waiting 2s"
        await =<< newDelay 2000000

        liftIO $ atomically do
          commitSurface surface SurfaceCommit {
            buffer = Nothing,
            offset = (0, 0),
            bufferDamage = DamageList []
          }
          commitSurface surface2 SurfaceCommit {
            buffer = Nothing,
            offset = (0, 0),
            bufferDamage = DamageList []
          }

        -- traceIO . ("shm buffer formats: " <>) . mconcat . intersperse ", " . fmap show . toList =<< await shm.formats

        pure ()

    await =<< newDelay 1000000
    traceIO "Closing"
  traceIO "Closed"
