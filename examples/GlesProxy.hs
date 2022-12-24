module Main (main) where

import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.XdgShell
import Quasar.Wayland.Gles
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Server
import Quasar.Wayland.Server.DummyOutput
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.XdgShell
import Quasar.Wayland.Shared.FnWindowManager
import Quasar.Wayland.Surface
import Control.Concurrent

main :: IO ()
main = runQuasarAndExit (stderrLogger LogLevelWarning) do
  client <- connectWaylandClient
  traceIO "Connected"

  quasar <- askQuasar
  liftIO $ runInBoundThread do

    egl <- initializeGles
    (dmabufFormats, dmabufModifiers) <- eglQueryDmabufFormats egl
    demo <- setupDemo egl

    --clientDmabuf <- atomically $ getClientDmabufSingleton client
    --(dmabufFormats, dmabufModifiers) <- awaitSupportedFormats clientDmabuf

    jobVar <- newEmptyTMVarIO
    windowManager <- atomically $ mapWindowManager demo jobVar <$> getClientWindowManager @GlesBackend client

    registry <- newRegistry [
      compositorGlobal @GlesBackend,
      dummyOutputGlobal,
      dmabufGlobal dmabufFormats dmabufModifiers,
      xdgShellGlobal windowManager
      ]
    server <- newWaylandServer registry
    runQuasarIO quasar do
      listenAt "example.socket" server

    forever do
      join $ atomically (takeTMVar jobVar)

mapWindowManager :: IsWindowManager GlesBackend a => Demo -> TMVar (IO ()) -> a -> FnWindowManager GlesBackend
mapWindowManager demo jobVar upstream = fnWindowManager {
  newWindowFn = \cfg -> mapWindow demo jobVar <$> fnWindowManager.newWindowFn cfg
}
  where
    fnWindowManager = toFnWindowManager upstream

mapWindow :: Demo -> TMVar (IO ()) -> FnWindow GlesBackend -> FnWindow GlesBackend
mapWindow demo jobVar window = window {
  setTitleFn = \title -> setTitle window (title <> " (proxy)"),
  commitWindowContentFn = onWindowContentCommit demo jobVar window
}

onWindowContentCommit :: Demo -> TMVar (IO ()) -> FnWindow GlesBackend -> ConfigureSerial -> SurfaceCommit GlesBackend -> STM ()
onWindowContentCommit demo jobVar window serial commit = do
  traceM "commit"
  putTMVar jobVar do
    buffer <- mapM (textureDemo demo . getDmabuf . (.storage)) commit.buffer
    atomically $ commitWindowContent window serial commit {
      buffer
    }
