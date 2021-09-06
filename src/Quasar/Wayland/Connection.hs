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
import Quasar.Wayland.Core
import Quasar.Wayland.Protocol


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

newWaylandConnection :: forall s m. MonadResourceManager m => Callback s STM I_wl_display -> Socket -> m (WaylandConnection s)
newWaylandConnection wlDisplayCallback socket = do
  protocolStateVar <- liftIO $ newTVarIO $ initialProtocolState wlDisplayCallback
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
      async $ liftIO $ waylandConnectionSendThread connection `catchAll` \ex -> traceIO (displayException ex) >> void (dispose resourceManager)
      async $ liftIO $ waylandConnectionReceiveThread connection `catchAll` \ex -> traceIO (displayException ex) >> void (dispose resourceManager)

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


waylandConnectionSendThread :: WaylandConnection s -> IO ()
waylandConnectionSendThread connection = forever do
  bytes <- atomically $ takeTMVar connection.outboxVar

  traceIO $ "Sending data: " <> show (BSL.length bytes) <> " bytes"
  SocketL.sendAll connection.socket bytes


waylandConnectionReceiveThread :: WaylandConnection s -> IO ()
waylandConnectionReceiveThread connection = forever do
  bytes <- Socket.recv connection.socket 4096

  when (BS.length bytes == 0) do
    fail "Socket is closed"

  traceIO $ "Received " <> show (BS.length bytes) <> " bytes"

  stepProtocol connection $ feedInput bytes

closeConnection :: WaylandConnection s -> IO (Awaitable ())
closeConnection connection = do
  -- gracefulClose may fail but guarantees that the socket is deallocated
  Socket.close connection.socket `catch` \(_ :: SomeException) -> pure ()
  pure $ pure ()
