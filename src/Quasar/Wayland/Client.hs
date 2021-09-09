module Quasar.Wayland.Client (
  connectWaylandClient,
  newWaylandClient,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.State (StateT, lift, runStateT, execStateT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Socket.ByteString.Lazy qualified as SocketL
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), isRelative)
import Text.Read (readEither)


data WaylandClient = WaylandClient (WaylandConnection 'Client)

instance IsResourceManager WaylandClient where
  toResourceManager (WaylandClient connection) = toResourceManager connection

instance IsDisposable WaylandClient where
  toDisposable (WaylandClient connection) = toDisposable connection

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = WaylandClient <$> newWaylandConnection @I_wl_display @I_wl_registry clientCallback clientCallback socket

clientCallback :: IsInterfaceSide 'Client i => ClientCallback STM i
clientCallback = Callback {
  messageCallback = \object message ->
    lift $ traceM $ showObjectMessage object message
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
