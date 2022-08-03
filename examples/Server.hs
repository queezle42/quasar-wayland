module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Server
import Quasar.Wayland.Server.Registry

main :: IO ()
main = runQuasarAndExit (stderrLogger LogLevelWarning) do
  registry <- newRegistry
  server <- newWaylandServer registry
  listenAt "example.socket" server
  sleepForever
