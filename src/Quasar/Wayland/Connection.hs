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
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated


data WaylandConnection s = WaylandConnection {
  protocolStateVar :: TVar (ProtocolState s STM),
  outboxVar :: TMVar BSL.ByteString,
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
  :: forall wl_display wl_registry s m. (IsInterfaceSide s wl_display, IsInterfaceSide s wl_registry, MonadResourceManager m)
  => SimpleCallback s STM wl_display
  -> SimpleCallback s STM wl_registry
  -> Socket
  -> m (WaylandConnection s)
newWaylandConnection wlDisplayCallback wlRegistryCallback socket = do
  protocolStateVar <- liftIO $ newTVarIO $ initialProtocolState wlDisplayCallback wlRegistryCallback
  outboxVar <- liftIO newEmptyTMVarIO

  resourceManager <- newResourceManager

  onResourceManager resourceManager do
    let connection = WaylandConnection {
      protocolStateVar,
      outboxVar,
      socket,
      resourceManager
    }

    registerDisposeAction $ closeConnection connection

    runUnlimitedAsync do
      connectionThread connection $ sendThread connection
      connectionThread connection $ receiveThread connection

    -- HACK to send first message (queued internally)
    stepProtocol connection $ feedInput ""

    pure connection

stepProtocol :: forall s m a. MonadIO m => WaylandConnection s -> ProtocolStep s STM a -> m a
stepProtocol connection step = liftIO do
  result <- atomically do
    oldState <- readTVar connection.protocolStateVar
    (result, outBytes, newState) <- step oldState
    writeTVar connection.protocolStateVar newState
    mapM_ (putTMVar connection.outboxVar) outBytes
    pure result
  case result of
    Left ex -> throwM (ex :: SomeException)
    Right result -> pure result

connectionThread :: MonadAsync m => WaylandConnection s -> IO () -> m ()
connectionThread connection work = async_ $ liftIO $ work `catches` [ignoreCancelTask, handleAll]
  where
    ignoreCancelTask = Handler (throwM :: CancelTask -> IO a)
    handleAll = Handler (\(ex :: SomeException) -> traceIO (displayException ex) >> void (dispose connection))

sendThread :: WaylandConnection s -> IO ()
sendThread connection = forever do
  bytes <- atomically $ takeTMVar connection.outboxVar

  traceIO $ "Sending " <> show (BSL.length bytes) <> " bytes"
  SocketL.sendAll connection.socket bytes


receiveThread :: IsSide s => WaylandConnection s -> IO ()
receiveThread connection = forever do
  bytes <- Socket.recv connection.socket 4096

  when (BS.length bytes == 0) do
    throwM SocketClosed

  traceIO $ "Received " <> show (BS.length bytes) <> " bytes"

  stepProtocol connection $ feedInput bytes

closeConnection :: WaylandConnection s -> IO (Awaitable ())
closeConnection connection = do
  -- gracefulClose may fail but guarantees that the socket is deallocated
  Socket.close connection.socket `catch` \(_ :: SomeException) -> pure ()
  pure $ pure ()
