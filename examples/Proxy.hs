module Main (main) where

import Control.Concurrent
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Server
import Quasar.Wayland.Server.DataTransfer
import Quasar.Wayland.Server.DummyOutput
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shared.FnWindowManager
import Quasar.Wayland.Shared.WindowApi (WindowProperties(..), toWindowFactory)
import Quasar.Wayland.Shared.WindowMultiplexer
import Quasar.Wayland.Skia
import Quasar.Wayland.Skia.GL

main :: IO ()
main = runQuasarAndExit do
  client <- connectWaylandClient
  traceIO "Connected"

  quasar <- askQuasar
  liftIO $ runInBoundThread do

    skia <- liftIO $ initializeSkia @GL

    --clientDmabuf <- atomically $ getClientDmabufSingleton client
    --(dmabufFormats, dmabufModifiers) <- awaitSupportedFormats clientDmabuf

    wlClientWM <- atomicallyC $ getClientWindowManager @(Skia GL) client

    let muxWM = WindowMultiplexerFactory [toWindowFactory (mapWindowManager wlClientWM), toWindowFactory wlClientWM]

    let globals =
          skiaGlobals skia <> [
            compositorGlobal @(Skia GL),
            subcompositorGlobal @(Skia GL),
            dummyOutputGlobal,
            dataDeviceManagerGlobal,
            xdgShellGlobal (mapWindowManager wlClientWM) -- muxWM
          ]

    registry <- newRegistry globals
    server <- newWaylandServer registry
    runQuasarIO quasar do
      listenAt "example.socket" server

    sleepForever

mapWindowManager :: IsWindowManager (Skia GL) w a => a -> FnWindowManager (Skia GL)
mapWindowManager upstream = fnWindowManager {
  newWindowFn = \props cfg req -> fnWindowManager.newWindowFn (mapProperties props) cfg req
}
  where
    fnWindowManager = toFnWindowManager upstream

mapProperties :: WindowProperties -> WindowProperties
mapProperties properties = properties {
  title = properties.title <> " (proxy)"
}
