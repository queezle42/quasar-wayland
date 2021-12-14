module Quasar.Wayland.Client.Socket (
  connectWaylandSocket
) where

import Control.Monad.Catch
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Quasar.Prelude
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), isRelative)
import Text.Read (readEither)


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
