module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client

main :: IO ()
main = withResourceManagerM do
  traceIO "Connecting"
  client <- connectWaylandClient
  traceIO "Connected"
