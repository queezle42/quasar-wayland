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

import Quasar.Disposer (Disposable (getDisposer), TDisposer)
import Quasar.Disposer.DisposableVar
import Quasar.Future (Future)
import Quasar.Observable.Core (attachSimpleObserver)
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi


type ClientWindowManager :: Type -> Type
data ClientWindowManager b = ClientWindowManager {
  client :: WaylandClient b,
  wlXdgWmBase :: Object 'Client Interface_xdg_wm_base
}

instance ClientBufferBackend b => IsWindowManager b (ClientXdgToplevel b) (ClientWindowManager b) where
  newWindow = newClientXdgToplevel

data ClientXdgToplevelState b = ClientXdgToplevelState {
  propertiesDisposer :: TDisposer,
  clientSurface :: ClientSurface b,
  xdgSurface :: Object 'Client Interface_xdg_surface,
  xdgToplevel :: Object 'Client Interface_xdg_toplevel,
  nextConfigureSerial :: TVar (Maybe Word32),
  configurationAccumulator :: TVar WindowConfiguration
}

newtype ClientXdgToplevel b = ClientXdgToplevel (TDisposableVar (ClientXdgToplevelState b))

instance ClientBufferBackend b => IsWindow b (ClientXdgToplevel b) where
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
withState (ClientXdgToplevel var) action = tryReadTDisposableVar var >>= mapM_ action



newClientWindowManager :: WaylandClient b -> STMc NoRetry '[SomeException] (ClientWindowManager b)
newClientWindowManager client = do
  wlXdgWmBase <- bindSingleton client.registry maxVersion
  setEventHandler wlXdgWmBase EventHandler_xdg_wm_base {
    ping = wlXdgWmBase.pong
  }
  pure ClientWindowManager { client, wlXdgWmBase }

getClientWindowManager :: forall b. ClientBufferBackend b => WaylandClient b -> STMc NoRetry '[SomeException] (ClientWindowManager b)
getClientWindowManager client = getClientComponent (newClientWindowManager @b client) client


newClientXdgToplevel ::
  forall b.
  ClientBufferBackend b =>
  ClientWindowManager b ->
  WindowProperties ->
  WindowConfigurationCallback ->
  WindowRequestCallback ->
  STMc NoRetry '[SomeException] (ClientXdgToplevel b)
newClientXdgToplevel ClientWindowManager{client, wlXdgWmBase} properties configureCallback requestCallback = do
  nextConfigureSerial <- newTVar Nothing
  configurationAccumulator <- newTVar defaultWindowConfiguration

  (clientSurface, (xdgSurface, xdgToplevel, propertiesDisposer)) <- newClientSurface @b client \wlSurface -> do

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
      close = liftSTMc $ requestCallback WindowRequestClose,
      configure_bounds = undefined,
      wm_capabilities = undefined
    }

    (d1, initialTitle) <- liftSTMc $ attachSimpleObserver properties.title do
      -- TODO ensure this logs/normalizes invalid titles but does not crash the downstream connection
      tryCall . xdgToplevel.set_title

    unless (nullWlString initialTitle) do
      xdgToplevel.set_title initialTitle

    (d2, initialAppId) <- liftSTMc $ attachSimpleObserver properties.appId do
      -- TODO ensure this logs/normalizes invalid app_ids but does not crash the downstream connection
      tryCall . xdgToplevel.set_app_id

    unless (nullWlString initialAppId) do
      xdgToplevel.set_app_id initialAppId

    let propertiesDisposer = d1 <> d2

    pure (xdgSurface, xdgToplevel, propertiesDisposer)

  let state = ClientXdgToplevelState {
    clientSurface,
    xdgSurface,
    xdgToplevel,
    nextConfigureSerial,
    configurationAccumulator,
    propertiesDisposer
  }

  ClientXdgToplevel <$> newTDisposableVar state disposeClientXdgToplevel

commitClientXdgToplevel :: forall b. ClientBufferBackend b => ClientXdgToplevel b -> ConfigureSerial -> Owned (SurfaceCommit b) -> STMc NoRetry '[SomeException] (Future '[] ())
commitClientXdgToplevel toplevel@(ClientXdgToplevel var) configureSerial surfaceCommit = do
  ackWindowConfigure @b toplevel configureSerial
  tryReadTDisposableVar var >>= \case
    Nothing -> pure (pure ())
    Just state -> commitSurfaceDownstream state.clientSurface surfaceCommit

ackToplevelConfigure :: ClientXdgToplevel b -> ConfigureSerial -> STMc NoRetry '[SomeException] ()
ackToplevelConfigure toplevel _configureSerial = do
  withState toplevel \state ->
    -- NOTE Dummy implementation to encourage correct api design without actually implementing configure serials.
    swapTVar state.nextConfigureSerial Nothing >>= \case
      Nothing -> pure ()
      Just serial -> state.xdgSurface.ack_configure serial
