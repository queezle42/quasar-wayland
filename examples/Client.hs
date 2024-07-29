module Main (main) where

--import Quasar.Resources.TRcVar
import Quasar
import Quasar.Disposer.Rc
import Quasar.Prelude
import Quasar.Timer
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi
import Quasar.Wayland.Skia
import Quasar.Wayland.Skia.GL

main :: IO ()
main = do
  _ <- runQuasarAndExit do

    skia <- swallowDisposerIO $ liftIO $ initializeSkia @GL

    traceIO "Connecting"
    client <- connectWaylandClient
    traceIO "Connected"

    configurationVar <- newEmptyTMVarIO

    shouldClose <- newTVarIO False

    let properties = defaultWindowProperties {
      title = "quasar-wayland-example-client-gles"
    }

    window <- atomicallyC do
      windowManager <- getClientWindowManager @(Skia GL) client
      newWindow windowManager properties (writeTMVar configurationVar) (\WindowRequestClose -> writeTVar shouldClose True)

    frameId <- newTVarIO 0

    whileM (not <$> readTVarIO shouldClose) do
      -- Blocks until first configure event
      configuration <- atomically $ readTMVar configurationVar

      let width = max configuration.width 512
      let height = max configuration.height 512
      surface <- liftIO $ newSkiaSurface skia width height
      liftIO $ clearSkiaSurface (fromOwned surface)

      frame <- newRcIO =<< liftIO (newFrameConsumeSurface surface)

      commit <- atomicallyC do
        commitWindowContent window configuration.configureSerial
          (defaultSurfaceCommit @(Skia GL) frame <&> \c -> c {
            bufferDamage = Just DamageAll
          })

      delay <- newDelay 16000
      await commit

      traceIO "#################################################"

      await delay

    traceIO "Closing"
  traceIO "Closed"

whileM :: Monad m => m Bool -> m () -> m ()
whileM p action = whenM p (action >> whileM p action)
