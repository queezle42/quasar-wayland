module Quasar.Wayland.Client (
  WaylandClient(registry),
  connectWaylandClient,
  newWaylandClient,

  -- * wl_registry
  Registry,
  bindSingleton,
  tryBindSingleton,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import GHC.Records
import Network.Socket (Socket)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client.Sync
import Quasar.Wayland.Client.Registry
import Quasar.Wayland.Client.Socket
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated


data WaylandClient = WaylandClient {
  connection :: WaylandConnection 'Client,
  wlDisplay :: Object 'Client Interface_wl_display,
  registry :: Registry
}

instance Resource WaylandClient where
  toDisposer client = toDisposer client.connection

connectWaylandClient :: (MonadIO m, MonadQuasar m) => m WaylandClient
connectWaylandClient = liftQuasarIO $ mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket

newWaylandClient :: (MonadIO m, MonadQuasar m) => Socket -> m WaylandClient
newWaylandClient socket = do
  ((wlDisplay, registry), connection) <- newWaylandConnection newClientDisplay socket

  pure WaylandClient {
    connection,
    wlDisplay,
    registry
  }
  where
    newClientDisplay :: STM ((Object 'Client Interface_wl_display, Registry), ProtocolHandle 'Client)
    newClientDisplay = initializeProtocol wlDisplayEventHandler init

    init :: Object 'Client Interface_wl_display -> STM (Object 'Client Interface_wl_display, Registry)
    init wlDisplay = do
      registry <- createRegistry wlDisplay
      pure (wlDisplay, registry)

    wlDisplayEventHandler :: ProtocolHandle 'Client -> EventHandler_wl_display
    wlDisplayEventHandler protocol =
      EventHandler_wl_display {
        error = handleWlDisplayError protocol,
        delete_id = handleWlDisplayDeleteId protocol
      }


instance HasField "sync" WaylandClient (STM (Future ())) where
  getField client = do
    var <- newPromiseSTM
    lowLevelSync client.wlDisplay \_ -> fulfillPromiseSTM var ()
    pure $ toFuture var
