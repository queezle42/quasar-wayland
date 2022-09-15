module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Server
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Shm
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shm
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

main :: IO ()
main = runQuasarAndExit (stderrLogger LogLevelWarning) do
  wm <- atomically newServerWindowManager
  let
    layerShellGlobal = createGlobal @Interface_zwlr_layer_shell_v1 maxVersion (\x -> setRequestHandler x layerShellHandler)
    wmGlobal = xdgShellGlobal @ShmBufferBackend wm
  registry <- newRegistry [compositorGlobal @ShmBufferBackend, shmGlobal, layerShellGlobal, wmGlobal]
  server <- newWaylandServer registry
  listenAt "example.socket" server
  sleepForever

layerShellHandler :: RequestHandler_zwlr_layer_shell_v1
layerShellHandler =
  RequestHandler_zwlr_layer_shell_v1 {
    get_layer_surface = \wlLayerSurface _ _ _ _ -> do
      setRequestHandler wlLayerSurface layerSurfaceHandler
      -- Just send a "correct" configure event for the demo client to get things rolling
      wlLayerSurface.configure 0 512 512,
    destroy = pure ()
  }

layerSurfaceHandler :: RequestHandler_zwlr_layer_surface_v1
layerSurfaceHandler =
  RequestHandler_zwlr_layer_surface_v1 {
    set_size = \_ _ -> pure (),
    set_anchor = \_ -> pure (),
    set_exclusive_zone = \_ -> pure (),
    set_margin = \_ _ _ _ -> pure (),
    set_keyboard_interactivity = \_ -> pure (),
    get_popup = \_ -> pure (),
    ack_configure = \_ -> pure (),
    destroy = pure (),
    set_layer = \_ -> pure ()
  }
