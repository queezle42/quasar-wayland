module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shm
import Quasar.Wayland.Surface


main :: IO ()
main = do
  _ <- runQuasarAndExit do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    egl <- liftIO initializeGles
    demo <- liftIO $ setupRenderDemo egl

    configurationVar <- newEmptyTMVarIO

    tl <- atomicallyC do
      windowManager <- getClientWindowManager @GlesBackend client
      tl <- newWindow windowManager (writeTMVar configurationVar)
      setTitle tl "quasar-wayland-example-client"
      pure tl

    forM_ [0,1..480] \i -> do
      -- Blocks until first configure event
      configuration <- atomically $ readTMVar configurationVar

      let width = max configuration.width 512
      let height = max configuration.height 512
      buffer <- liftIO $ renderDemo demo width height (i / 60)
      atomicallyC do
        commitWindowContent tl configuration.configureSerial defaultSurfaceCommit {
          buffer = Just buffer,
          bufferDamage = Just DamageAll
        }
        liftSTMc $ destroyBuffer buffer

      await =<< newDelay 16000
    traceIO "Closing"
  traceIO "Closed"
