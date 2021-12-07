module Quasar.Wayland.Connection (
  WaylandConnection,
  newWaylandConnection,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Socket.ByteString.Lazy qualified as SocketL
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Protocol


data WaylandConnection s = WaylandConnection {
  protocolHandle :: ProtocolHandle s,
  socket :: Socket,
  resourceManager :: ResourceManager
}

instance IsResourceManager (WaylandConnection s) where
  toResourceManager connection = connection.resourceManager

instance IsDisposable (WaylandConnection s) where
  toDisposable connection = toDisposable connection.resourceManager

data SocketClosed = SocketClosed
  deriving stock Show
  deriving anyclass Exception

newWaylandConnection
  :: forall s m a. (IsSide s, MonadResourceManager m)
  => STM (a, ProtocolHandle s)
  -> Socket
  -> m (a, WaylandConnection s)
newWaylandConnection initializeProtocolAction socket = do
  (result, protocolHandle) <- liftIO $ atomically $ initializeProtocolAction

  resourceManager <- newResourceManager

  onResourceManager resourceManager do
    let connection = WaylandConnection {
      protocolHandle,
      socket,
      resourceManager
    }

    registerDisposeAction $ closeConnection connection

    connectionThread connection $ sendThread connection
    connectionThread connection $ receiveThread connection

    pure (result, connection)

connectionThread :: MonadResourceManager m => WaylandConnection s -> IO () -> m ()
connectionThread connection work = asyncWithHandler_ traceAndDisposeConnection $ liftIO $ work
  where
    traceAndDisposeConnection :: SomeException -> IO ()
    traceAndDisposeConnection ex = traceIO (displayException ex) >> void (dispose connection)

sendThread :: WaylandConnection s -> IO ()
sendThread connection = forever do
  bytes <- takeOutbox connection.protocolHandle

  traceIO $ "Sending " <> show (BSL.length bytes) <> " bytes"
  SocketL.sendAll connection.socket bytes


receiveThread :: IsSide s => WaylandConnection s -> IO ()
receiveThread connection = forever do
  bytes <- Socket.recv connection.socket 4096

  when (BS.length bytes == 0) do
    throwM SocketClosed

  traceIO $ "Received " <> show (BS.length bytes) <> " bytes"

  feedInput connection.protocolHandle bytes

closeConnection :: WaylandConnection s -> IO ()
closeConnection connection = Socket.close connection.socket
