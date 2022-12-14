module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Server
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Shm
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shm

main :: IO ()
main = runQuasarAndExit (stderrLogger LogLevelWarning) do
  client <- connectWaylandClient
  traceIO "Connected"

  windowManager <- atomically $ getClientWindowManager @ShmBufferBackend client

  registry <- newRegistry [
    compositorGlobal @ShmBufferBackend,
    shmGlobal,
    xdgShellGlobal windowManager
    ]
  server <- newWaylandServer registry
  listenAt "example.socket" server
  sleepForever
