module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Server
import Quasar.Wayland.Server.DummyOutput
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.XdgShell

import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

main :: IO ()
main = runQuasarAndExit (stderrLogger LogLevelWarning) do
  client <- connectWaylandClient
  traceIO "Connected"

  clientDmabuf <- atomically $ getClientDmabufSingleton client
  (dmabufFormats, dmabufModifiers) <- liftIO $ awaitSupportedFormats clientDmabuf

  atomically do
    zwpLinuxDmabuf <- bindSingleton @Interface_zwp_linux_dmabuf_v1 client.registry 3

    zwpLinuxDmabuf `setEventHandler` EventHandler_zwp_linux_dmabuf_v1 {
      format = \_ -> pure (),
      modifier = \_ _ _ -> pure ()
    }

  windowManager <- atomically $ getClientWindowManager @GlesBackend client

  registry <- newRegistry [
    compositorGlobal @GlesBackend,
    dummyOutputGlobal,
    dmabufGlobal dmabufFormats dmabufModifiers,
    xdgShellGlobal windowManager
    ]
  server <- newWaylandServer registry
  listenAt "example.socket" server
  sleepForever
