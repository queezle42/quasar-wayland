module Main (main) where

import Control.Concurrent
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles
import Quasar.Wayland.Server
import Quasar.Wayland.Server.DummyOutput
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shared.FnWindowManager
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi (WindowProperties(..), toWindowFactory)
import Quasar.Wayland.Shared.WindowMultiplexer

main :: IO ()
main = runQuasarAndExit do
  client <- connectWaylandClient
  traceIO "Connected"

  quasar <- askQuasar
  liftIO $ runInBoundThread do

    backend <- initializeGlesBackend
    let egl = backend.egl

    demo <- setupProxyDemo egl

    --clientDmabuf <- atomically $ getClientDmabufSingleton client
    --(dmabufFormats, dmabufModifiers) <- awaitSupportedFormats clientDmabuf

    wlClientWM <- atomicallyC $ getClientWindowManager @GlesBackend client

    jobQueue <- newTQueueIO
    let shaderWM = mapWindowManager demo jobQueue wlClientWM

    let muxWM = WindowMultiplexerFactory [toWindowFactory shaderWM, toWindowFactory wlClientWM]

    registry <- newRegistry [
      compositorGlobal @GlesBackend,
      dummyOutputGlobal,
      xdgShellGlobal muxWM,
      glesDmabufGlobal backend
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

onWindowContentCommit :: ProxyDemo -> TQueue (IO ()) -> FnWindow GlesBackend -> ConfigureSerial -> SurfaceCommit GlesBackend -> STMc NoRetry '[SomeException] (Future ())
onWindowContentCommit demo jobQueue window serial commit = do
  traceM "commit"
  disposer <- liftSTMc $ lockBuffer commit.buffer
  writeTQueue jobQueue do
    b <- proxyDemo demo $ getDmabuf commit.buffer.storage
    commitFuture <- atomicallyC do
      disposeTDisposer disposer
      commitFuture <- commitWindowContent window serial commit {
        buffer = b
      }
      liftSTMc $ destroyBuffer b
      pure commitFuture
    await commitFuture
  pure (pure ())
