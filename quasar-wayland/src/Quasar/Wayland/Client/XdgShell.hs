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
  ConfigureSerial,
  commitXdgToplevel,
) where

import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.WindowManagerApi
import Quasar.Wayland.Surface


type ClientWindowManager :: Type -> Type
data ClientWindowManager b = ClientWindowManager {
  client :: WaylandClient,
  wlXdgWmBase :: Object 'Client Interface_xdg_wm_base
}

instance ClientBufferBackend b => IsWindowManager b (ClientWindowManager b) where
  type Window b (ClientWindowManager b) = ClientXdgToplevel b
  newWindow = newClientXdgToplevel

instance ClientBufferBackend b => IsWindow b (ClientXdgToplevel b) where
  setTitle w = w.xdgToplevel.set_title
  setAppId w = w.xdgToplevel.set_app_id
  setFullscreen w fullscreen =
    if fullscreen
      then w.xdgToplevel.set_fullscreen Nothing
      else w.xdgToplevel.unset_fullscreen
  commitWindowContent = commitXdgToplevel
  ackWindowConfigure = ackToplevelConfigure


data ClientXdgToplevel b = ClientXdgToplevel {
  clientSurface :: ClientSurface b,
  xdgSurface :: Object 'Client Interface_xdg_surface,
  xdgToplevel :: Object 'Client Interface_xdg_toplevel,
  nextConfigureSerial :: TVar (Maybe Word32),
  configurationAccumulator :: TVar WindowConfiguration
}


newClientWindowManager :: WaylandClient -> STM (ClientWindowManager b)
newClientWindowManager client = do
  wlXdgWmBase <- bindSingleton client.registry maxVersion
  setEventHandler wlXdgWmBase EventHandler_xdg_wm_base {
    ping = wlXdgWmBase.pong
  }
  pure ClientWindowManager { client, wlXdgWmBase }

getClientWindowManager :: forall b. ClientBufferBackend b => WaylandClient -> STM (ClientWindowManager b)
getClientWindowManager client = getClientComponent (newClientWindowManager @b client) client


newClientXdgToplevel :: forall b. ClientBufferBackend b => ClientWindowManager b -> (WindowConfiguration -> STM ()) -> STM (ClientXdgToplevel b)
newClientXdgToplevel ClientWindowManager{client, wlXdgWmBase} configureCallback = do
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
      close = pure () -- TODO
    }

    pure (xdgSurface, xdgToplevel)

  pure ClientXdgToplevel {
    clientSurface,
    xdgSurface,
    xdgToplevel,
    nextConfigureSerial,
    configurationAccumulator
  }

commitXdgToplevel :: forall b. ClientBufferBackend b => ClientXdgToplevel b -> ConfigureSerial -> SurfaceCommit b -> STM ()
commitXdgToplevel toplevel configureSerial surfaceCommit = do
  ackWindowConfigure @b toplevel configureSerial
  commitSurfaceDownstream toplevel.clientSurface surfaceCommit

ackToplevelConfigure :: ClientXdgToplevel b -> ConfigureSerial -> STM ()
ackToplevelConfigure toplevel _configureSerial = do
  -- NOTE Dummy implementation to encourage correct api design without actually implementing configure serials.
  swapTVar toplevel.nextConfigureSerial Nothing >>= \case
    Nothing -> pure ()
    Just serial -> toplevel.xdgSurface.ack_configure serial
