module Quasar.Wayland.Client (
  WaylandClient(registry),
  connectWaylandClient,
  newWaylandClient,

  -- * wl_registry
  Registry,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import GHC.Records
import Network.Socket (Socket)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client.Registry
import Quasar.Wayland.Client.Socket
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Display
import Quasar.Wayland.Protocol.Generated


data WaylandClient = WaylandClient {
  connection :: WaylandConnection 'Client,
  wlDisplay :: Object 'Client Interface_wl_display,
  registry :: Registry
}

instance IsResourceManager WaylandClient where
  toResourceManager client = toResourceManager client.connection

instance IsDisposable WaylandClient where
  toDisposable client = toDisposable client.connection

connectWaylandClient :: MonadResourceManager m => m WaylandClient
connectWaylandClient = mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = do
  ((wlDisplay, registry), connection) <- newWaylandConnection newClientDisplay socket
  pure WaylandClient {
    connection,
    wlDisplay,
    registry
  }
  where
    newClientDisplay :: STM ((Object 'Client Interface_wl_display, Registry), ProtocolHandle 'Client)
    newClientDisplay =
      initializeProtocol wlDisplayEventHandler \wlDisplay -> do
        registry <- createRegistry wlDisplay
        pure (wlDisplay, registry)



instance HasField "sync" WaylandClient (STM (Awaitable ())) where
  getField client = do
    var <- newAsyncVarSTM
    lowLevelSync client.wlDisplay \_ -> putAsyncVarSTM_ var ()
    pure $ toAwaitable var

