module Main (main) where

import Data.List (intersperse)
import Data.Foldable (toList)
import Quasar
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Buffer
import Quasar.Wayland.Client.JuicyPixels
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

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
    width' = width dimensions
    height' = height dimensions
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

color :: RealFrac a => a -> a -> a -> PixelRGBA8
color r g b = PixelRGBA8 (toWord r) (toWord g) (toWord b) 255
  where
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
  runQuasarAndExit (stderrLogger LogLevelWarning) do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    join $ liftIO $ atomically do

      wlCompositor <- bindSingleton @Interface_wl_compositor client.registry

      shm <- newShmBufferManager client

      wlSurface <- wlCompositor.create_surface
      setMessageHandler wlSurface EventHandler_wl_surface {
        enter = \_ -> pure (),
        leave = \_ -> pure ()
      }

      --xdgWmBase <- bindSingleton @Interface_xdg_wm_base client.registry
      --setMessageHandler xdgWmBase EventHandler_xdg_wm_base {
      --  ping = \serial -> xdgWmBase.pong serial
      --}

      --xdgSurface <- xdgWmBase.get_xdg_surface wlSurface
      --setMessageHandler xdgSurface EventHandler_xdg_surface {
      --  configure = \serial -> xdgSurface.ack_configure serial
      --}
      --xdgToplevel <- xdgSurface.get_toplevel
      --setMessageHandler xdgToplevel EventHandler_xdg_toplevel {
      --  configure = \_ _ _ -> pure (),
      --  close = pure ()
      --}

      --xdgToplevel.set_title "foobar"


      wlrLayerShell <- bindSingleton @Interface_zwlr_layer_shell_v1 client.registry

      configuredVar <- newTVar False

      wlrLayerSurface <- wlrLayerShell.get_layer_surface wlSurface Nothing 2 "demo"
      setMessageHandler wlrLayerSurface EventHandler_zwlr_layer_surface_v1 {
        configure = \serial width height -> do
            wlrLayerSurface.ack_configure serial
            writeTVar configuredVar True,
        closed = pure ()
      }
      wlrLayerSurface.set_size 512 512

      wlSurface.commit
      -- Should await first `configure` event

      pure do
        buffer <- liftIO $ toImageBuffer shm (mkImage gradient)

        liftIO $ atomically do
          check =<< readTVar configuredVar
          wlSurface.attach (Just buffer) 0 0
          wlSurface.commit

        -- buffer2 <- liftIO $ toImageBuffer shm (wallpaperImage wallpaper)

        --liftIO $ atomically do
        --  wlSurface.attach (Nothing) 0 0
        --  --wlSurface.damage 0 0 100 100
        --  wlSurface.commit

        traceIO . ("shm buffer formats: " <>) . mconcat . intersperse ", " . fmap show . toList =<< await shm.formats

        pure ()

    traceIO "Waiting 2s"
    await =<< newDelay 2000000
    traceIO "Closing"
  traceIO "Closed"
