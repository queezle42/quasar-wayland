module Main (main) where

import Quasar.Prelude
import Quasar.ResourceManager
import Quasar.Wayland.Client

main :: IO ()
main = withResourceManagerM do
  traceIO "Connecting"
  client <- connectWaylandClient
  traceIO "Connected"
