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
main = runQuasarAndExit do
  client <- connectWaylandClient
  traceIO "Connected"

  windowManager <- atomicallyC $ getClientWindowManager @ShmBufferBackend client

  registry <- newRegistry [
    compositorGlobal @ShmBufferBackend,
    shmGlobal ShmBufferBackend,
    xdgShellGlobal windowManager
    ]
  server <- newWaylandServer registry
  listenAt "example.socket" server
  sleepForever
