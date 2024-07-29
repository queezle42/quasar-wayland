module Quasar.Wayland.Server (
  WaylandServer,
  WaylandServerConnection,
  newWaylandServer,
  newWaylandServerConnection,
  listenAt,
  compositorGlobal,
  subcompositorGlobal,
) where

import Control.Monad.Catch
import Network.Socket (Socket)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Socket
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Shared.Surface


newtype WaylandServer b = WaylandServer {
  registry :: Registry b
}

newWaylandServer :: Monad m => Registry b -> m (WaylandServer b)
newWaylandServer registry = pure WaylandServer { registry }

data WaylandServerConnection b = WaylandConnection {
  wlDisplay :: Object 'Server Interface_wl_display,
  server :: WaylandServer b,
  connection :: WaylandConnection 'Server
}


newWaylandServerConnection ::
  (MonadIO m, MonadQuasar m) =>
  WaylandServer b -> Socket -> m (WaylandServerConnection b)
newWaylandServerConnection server socket = do
  (wlDisplay, connection) <- newWaylandConnection newServerDisplay socket
  pure WaylandConnection {
    wlDisplay,
    server,
    connection
  }
  where
    newServerDisplay :: STM (Object 'Server Interface_wl_display, ProtocolHandle 'Server)
    newServerDisplay = initializeProtocol wlDisplayRequestHandler (.delete_id) pure

    wlDisplayRequestHandler :: ProtocolHandle 'Server -> RequestHandler_wl_display
    wlDisplayRequestHandler _protocol =
      RequestHandler_wl_display {
        sync = (\wlCallback -> wlCallback.done 0),
        get_registry = (\wlRegistry -> addRegistryConnection server.registry wlRegistry)
      }


listenAt :: (MonadIO m, MonadMask m, MonadQuasar m) => FilePath -> WaylandServer b -> m ()
listenAt socketPath server = disposeOnError do
  var <- liftIO newEmptyTMVarIO
  async_ $ liftIO $ listenUnixPath socketPath (putTMVar var)
  asyncWithUnmask_ \_ -> forever do
    socket <- atomically $ takeTMVar var
    newWaylandServerConnection server socket


compositorGlobal :: forall b. RenderBackend b => Global b
compositorGlobal = createGlobal @Interface_wl_compositor maxVersion bindCompositor
  where
    bindCompositor :: Object 'Server Interface_wl_compositor -> STMc NoRetry '[SomeException] ()
    bindCompositor wlCompositor = setMessageHandler wlCompositor handler

    handler :: RequestHandler_wl_compositor
    handler = RequestHandler_wl_compositor {
      create_surface = \wlSurface -> liftSTMc $ initializeServerSurface @b wlSurface,
      create_region = \wlRegion -> liftSTMc $ initializeServerRegion wlRegion
    }

subcompositorGlobal :: forall b. RenderBackend b => Global b
subcompositorGlobal = createGlobal @Interface_wl_subcompositor maxVersion bindCompositor
  where
    bindCompositor :: Object 'Server Interface_wl_subcompositor -> STMc NoRetry '[SomeException] ()
    bindCompositor wlCompositor = setMessageHandler wlCompositor handler

    handler :: RequestHandler_wl_subcompositor
    handler = RequestHandler_wl_subcompositor {
      destroy = pure (), -- Destroy has no effect, as specified.
      get_subsurface = initializeServerSubsurface @b
    }
