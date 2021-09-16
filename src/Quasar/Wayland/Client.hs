module Quasar.Wayland.Client (
  connectWaylandClient,
  newWaylandClient,
  connectWaylandSocket,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), isRelative)
import Text.Read (readEither)


data WaylandClient = WaylandClient (WaylandConnection 'Client) (Object 'Client I_wl_display)

instance IsResourceManager WaylandClient where
  toResourceManager (WaylandClient connection _) = toResourceManager connection

instance IsDisposable WaylandClient where
  toDisposable (WaylandClient connection _) = toDisposable connection

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = do
  (connection, wlDisplay) <- newWaylandConnection @I_wl_display (traceCallback ignoreMessage) socket

  (_wlRegistry, newId) <- stepProtocol connection $ newObject @'Client @I_wl_registry (traceCallback ignoreMessage)
  stepProtocol connection $ sendMessage wlDisplay $ R_wl_display_get_registry newId
  pure $ WaylandClient connection wlDisplay

connectWaylandClient :: MonadResourceManager m => m WaylandClient
connectWaylandClient = mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket

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

  where
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
