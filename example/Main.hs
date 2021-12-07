module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client

main :: IO ()
main = do
  withRootResourceManager do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"
    await =<< newDelay 1000000
    traceIO "Closing"
  traceIO "Closed"
