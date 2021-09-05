module Quasar.Wayland.Client (
  connectWaylandClient,
  newWaylandClient,
) where

import Control.Monad.Catch
import Network.Socket qualified as Socket
import Network.Socket (Socket)
import Quasar.Disposable
import Quasar.Prelude
import Quasar.ResourceManager
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), isRelative)
import Text.Read (readEither)

data WaylandClient = WaylandClient {
  socket :: Socket,
  resourceManager :: ResourceManager
}

instance IsResourceManager WaylandClient where
  toResourceManager client = client.resourceManager

instance IsDisposable WaylandClient where
  toDisposable client = toDisposable client.resourceManager

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = do
  resourceManager <- newResourceManager
  onResourceManager resourceManager do
    registerDisposeAction (pure () <$ Socket.close socket)
    pure WaylandClient {
      socket,
      resourceManager
    }

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
