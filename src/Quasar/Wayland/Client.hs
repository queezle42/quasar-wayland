module Quasar.Wayland.Client (
  connectWaylandClient,
  newWaylandClient,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Socket.ByteString.Lazy qualified as SocketL
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Core
import Quasar.Wayland.Protocol
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), isRelative)
import Text.Read (readEither)


data WaylandClient = WaylandClient {
  protocolStateVar :: TVar ClientProtocolState,
  socket :: Socket,
  resourceManager :: ResourceManager
}

instance IsResourceManager WaylandClient where
  toResourceManager client = client.resourceManager

instance IsDisposable WaylandClient where
  toDisposable client = toDisposable client.resourceManager

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = do
  protocolStateVar <- liftIO $ newTVarIO initialClientProtocolState
  resourceManager <- newResourceManager

  onResourceManager resourceManager do
    let client = WaylandClient {
      protocolStateVar,
      socket,
      resourceManager
    }

    registerDisposeAction $ closeWaylandClient client

    runUnlimitedAsync do
      async $ liftIO $ waylandClientSendThread client `catchAll` \ex -> traceIO (displayException ex) >> void (dispose resourceManager)
      async $ liftIO $ waylandClientReceiveThread client `catchAll` \ex -> traceIO (displayException ex) >> void (dispose resourceManager)

    pure client

waylandClientSendThread :: WaylandClient -> IO ()
waylandClientSendThread client = forever do
  bytes <- atomically do
    outbox <- stateTVar client.protocolStateVar takeOutbox
    case outbox of
      Just bytes -> pure bytes
      Nothing -> retry

  traceIO $ "Sending data"
  SocketL.sendAll client.socket bytes


waylandClientReceiveThread :: WaylandClient -> IO ()
waylandClientReceiveThread client = forever do
  bytes <- Socket.recv client.socket 4096
  traceIO $ "Received data"
  events <- atomically $ stateTVar client.protocolStateVar $ feedInput bytes

  traceIO $ "Received " <> show (length events) <> " events"
  mapM_ (traceIO . show) events

  state <- atomically $ readTVar client.protocolStateVar
  traceIO $ show state.bytesReceived

closeWaylandClient :: WaylandClient -> IO (Awaitable ())
closeWaylandClient client = isDisposed <$> forkTask do
  -- gracefulClose may fail but guarantees that the socket is deallocated
  Socket.gracefulClose client.socket 2000 `catch` \(_ :: SomeException) -> pure ()


connectWaylandClient :: MonadResourceManager m => m WaylandClient
connectWaylandClient = mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket
  where
    connectWaylandSocket :: IO Socket
    connectWaylandSocket = do
      lookupEnv "WAYLAND_SOCKET" >>= \case
        -- Parent process already established connection
        Just waylandSocketEnv -> do
          case readEither waylandSocketEnv of
            Left err -> fail $ "Failed to parse WAYLAND_SOCKET: " <> err
            Right fd -> Socket.mkSocket fd
        Nothing -> do
          path <- getWaylandSocketPath
          newUnixSocket path

    getWaylandSocketPath :: IO FilePath
    getWaylandSocketPath = do
      waylandDisplayEnv <- lookupEnv "WAYLAND_DISPLAY"
      let waylandDisplay = fromMaybe "wayland-0" waylandDisplayEnv
      if isRelative waylandDisplay
        then do
          xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
          pure (xdgRuntimeDir </> waylandDisplay)
        else
          pure waylandDisplay

    newUnixSocket :: FilePath -> IO Socket
    newUnixSocket socketPath =
      bracketOnError (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close $ \sock -> do
        Socket.withFdSocket sock Socket.setCloseOnExecIfNeeded
        Socket.connect sock $ Socket.SockAddrUnix socketPath
        pure sock
