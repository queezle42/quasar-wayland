module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Resources.DisposableVar
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi


main :: IO ()
main = do
  _ <- runQuasarAndExit do
    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    egl <- liftIO initializeGles
    demo <- liftIO $ setupRenderDemo egl

    configurationVar <- newEmptyTMVarIO

    shouldClose <- newTVarIO False

    let properties = defaultWindowProperties {
      title = "quasar-wayland-example-client-gles"
    }

    window <- atomicallyC do
      windowManager <- getClientWindowManager @GlesBackend client
      newWindow windowManager properties (writeTMVar configurationVar) (\WindowRequestClose -> writeTVar shouldClose True)

    frameId <- newTVarIO 0

    whileM (not <$> readTVarIO shouldClose) do
      i <- atomically $ stateTVar frameId (\i -> (i, i + 1))

      -- Blocks until first configure event
      configuration <- atomically $ readTMVar configurationVar

      let width = max configuration.width 512
      let height = max configuration.height 512
      frame <- liftIO $ renderDemo demo width height (i / 60)
      commit <- atomicallyC do
        commitWindowContent window configuration.configureSerial ((defaultSurfaceCommit frame) {
          bufferDamage = Just DamageAll
        })

      delay <- newDelay 16000
      await commit
      await delay

    traceIO "Closing"
  traceIO "Closed"

whileM :: Monad m => m Bool -> m () -> m ()
whileM p action = whenM p (action >> whileM p action)
