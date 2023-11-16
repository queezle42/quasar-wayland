module Main (main) where

import Control.Concurrent
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Server
import Quasar.Wayland.Server.DummyOutput
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shared.FnWindowManager
import Quasar.Wayland.Shared.WindowApi (WindowProperties(..))
import Quasar.Wayland.Surface

main :: IO ()
main = runQuasarAndExit do
  client <- connectWaylandClient
  traceIO "Connected"

  quasar <- askQuasar
  liftIO $ runInBoundThread do

    egl <- initializeGles
    (dmabufFormats, dmabufModifiers) <- eglQueryDmabufFormats egl
    feedback <- getDmabufFeedback egl
    demo <- setupProxyDemo egl

    --clientDmabuf <- atomically $ getClientDmabufSingleton client
    --(dmabufFormats, dmabufModifiers) <- awaitSupportedFormats clientDmabuf

    jobQueue <- newTQueueIO
    windowManager <- atomicallyC $ mapWindowManager demo jobQueue <$> getClientWindowManager @GlesBackend client

    registry <- newRegistry [
      compositorGlobal @GlesBackend,
      dummyOutputGlobal,
      dmabufGlobal dmabufFormats dmabufModifiers feedback,
      xdgShellGlobal windowManager
      ]
    server <- newWaylandServer registry
    runQuasarIO quasar do
      listenAt "example.socket" server

    forever do
      join $ atomically (readTQueue jobQueue)

mapWindowManager :: IsWindowManager GlesBackend w a => ProxyDemo -> TQueue (IO ()) -> a -> FnWindowManager GlesBackend
mapWindowManager demo jobQueue upstream = fnWindowManager {
  newWindowFn = \props cfg req -> mapWindow demo jobQueue <$> fnWindowManager.newWindowFn (mapProperties props) cfg req
}
  where
    fnWindowManager = toFnWindowManager upstream

mapProperties :: WindowProperties -> WindowProperties
mapProperties properties = properties {
  title = properties.title <> " (proxy)"
}

mapWindow :: ProxyDemo -> TQueue (IO ()) -> FnWindow GlesBackend -> FnWindow GlesBackend
mapWindow demo jobQueue window = window {
  commitWindowContentFn = onWindowContentCommit demo jobQueue window
}

onWindowContentCommit :: ProxyDemo -> TQueue (IO ()) -> FnWindow GlesBackend -> ConfigureSerial -> SurfaceCommit GlesBackend -> STMc NoRetry '[SomeException] ()
onWindowContentCommit demo jobQueue window serial commit = do
  traceM "commit"
  case commit.buffer of
    Nothing -> commitWindowContent window serial commit
    Just buffer -> do
      disposer <- liftSTMc $ lockBuffer buffer
      writeTQueue jobQueue do
        b <- proxyDemo demo $ getDmabuf buffer.storage
        atomicallyC do
          disposeTSimpleDisposer disposer
          commitWindowContent window serial commit {
            buffer = Just b
          }
          liftSTMc $ destroyBuffer b
