module Quasar.Wayland.Client.XdgShell (
  -- * Window manager
  IsWindowManager(..),
  ClientWindowManager,
  getClientWindowManager,

  -- * Window (xdg_toplevel)
  IsWindow(..),
  ClientXdgToplevel,

  -- ** Window configuration
  WindowConfiguration(..),
  WindowConfigurationCallback,
  ConfigureSerial,
  commitClientXdgToplevel,

  -- ** Window request
  WindowRequest(..),
  WindowRequestCallback,
) where

import Quasar.Prelude
import Quasar.Resources (Disposable (getDisposer))
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.WindowManagerApi
import Quasar.Wayland.Surface
import Quasar.Resources.DisposableTVar


type ClientWindowManager :: Type -> Type
data ClientWindowManager b = ClientWindowManager {
  client :: WaylandClient,
  wlXdgWmBase :: Object 'Client Interface_xdg_wm_base
}

instance ClientBufferBackend b => IsWindowManager b (ClientWindowManager b) where
  type Window b (ClientWindowManager b) = ClientXdgToplevel b
  newWindow = newClientXdgToplevel

data ClientXdgToplevelState b = ClientXdgToplevelState {
  clientSurface :: ClientSurface b,
  xdgSurface :: Object 'Client Interface_xdg_surface,
  xdgToplevel :: Object 'Client Interface_xdg_toplevel,
  nextConfigureSerial :: TVar (Maybe Word32),
  configurationAccumulator :: TVar WindowConfiguration
}

newtype ClientXdgToplevel b = ClientXdgToplevel (DisposableTVar (ClientXdgToplevelState b))

instance ClientBufferBackend b => IsWindow b (ClientXdgToplevel b) where
  setTitle w title =
    withState w \state -> state.xdgToplevel.set_title title
  setAppId w appId =
    withState w \state -> state.xdgToplevel.set_app_id appId
  setFullscreen w fullscreen =
    withState w \state ->
      if fullscreen
        then state.xdgToplevel.set_fullscreen Nothing
        else state.xdgToplevel.unset_fullscreen
  commitWindowContent = commitClientXdgToplevel
  ackWindowConfigure = ackToplevelConfigure

instance Disposable (ClientXdgToplevel b) where
  getDisposer (ClientXdgToplevel disposableVar) = getDisposer disposableVar

disposeClientXdgToplevel :: ClientXdgToplevelState b -> STMc NoRetry '[] ()
disposeClientXdgToplevel state = do
  tryCall state.xdgToplevel.destroy
  tryCall state.xdgSurface.destroy
  -- TODO do we have to release a buffer?

withState :: MonadSTMc NoRetry '[] m => ClientXdgToplevel b -> (ClientXdgToplevelState b -> m ()) -> m ()
withState (ClientXdgToplevel var) action = tryReadDisposableTVar var >>= mapM_ action



newClientWindowManager :: WaylandClient -> STMc NoRetry '[SomeException] (ClientWindowManager b)
newClientWindowManager client = do
  wlXdgWmBase <- bindSingleton client.registry maxVersion
  setEventHandler wlXdgWmBase EventHandler_xdg_wm_base {
    ping = wlXdgWmBase.pong
  }
  pure ClientWindowManager { client, wlXdgWmBase }

getClientWindowManager :: forall b. ClientBufferBackend b => WaylandClient -> STMc NoRetry '[SomeException] (ClientWindowManager b)
getClientWindowManager client = getClientComponent (newClientWindowManager @b client) client


newClientXdgToplevel ::
  forall b.
  ClientBufferBackend b =>
  ClientWindowManager b ->
  WindowConfigurationCallback ->
  WindowRequestCallback ->
  STMc NoRetry '[SomeException] (ClientXdgToplevel b)
newClientXdgToplevel ClientWindowManager{client, wlXdgWmBase} configureCallback requestCallback = do
  nextConfigureSerial <- newTVar Nothing
  configurationAccumulator <- newTVar defaultWindowConfiguration

  (clientSurface, (xdgSurface, xdgToplevel)) <- newClientSurface @b client \wlSurface -> do

    xdgSurface <- wlXdgWmBase.get_xdg_surface wlSurface
    setEventHandler xdgSurface EventHandler_xdg_surface {
      configure = \serial -> do
        traceM ("configure: " <> show serial)
        writeTVar nextConfigureSerial (Just serial)
        configuration <- readTVar configurationAccumulator
        configureCallback (configuration { configureSerial = unsafeConfigureSerial })
    }

    xdgToplevel <- xdgSurface.get_toplevel
    setEventHandler xdgToplevel EventHandler_xdg_toplevel {
      configure = \width height states -> modifyTVar configurationAccumulator \x -> x { width = width, height = height, states = states },
      close = liftSTMc $ requestCallback WindowRequestClose
    }

    pure (xdgSurface, xdgToplevel)

  let state = ClientXdgToplevelState {
    clientSurface,
    xdgSurface,
    xdgToplevel,
    nextConfigureSerial,
    configurationAccumulator
  }

  ClientXdgToplevel <$> newDisposableTVar state disposeClientXdgToplevel

commitClientXdgToplevel :: forall b. ClientBufferBackend b => ClientXdgToplevel b -> ConfigureSerial -> SurfaceCommit b -> STMc NoRetry '[SomeException] ()
commitClientXdgToplevel toplevel configureSerial surfaceCommit = do
  ackWindowConfigure @b toplevel configureSerial
  withState toplevel \state ->
    commitSurfaceDownstream state.clientSurface surfaceCommit

ackToplevelConfigure :: ClientXdgToplevel b -> ConfigureSerial -> STMc NoRetry '[SomeException] ()
ackToplevelConfigure toplevel _configureSerial = do
  withState toplevel \state ->
    -- NOTE Dummy implementation to encourage correct api design without actually implementing configure serials.
    swapTVar state.nextConfigureSerial Nothing >>= \case
      Nothing -> pure ()
      Just serial -> state.xdgSurface.ack_configure serial
