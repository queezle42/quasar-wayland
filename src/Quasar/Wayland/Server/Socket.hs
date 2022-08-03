module Quasar.Wayland.Server.Socket (
  listenUnixPath
) where

import Control.Monad.Catch
import Network.Socket
import Quasar.Prelude
import System.IO
import System.Directory

listenUnixPath :: FilePath -> (Socket -> STM ()) -> IO a
listenUnixPath socketPath socketFn = do
  hPutStrLn stderr $ "Creating socket at " <> socketPath
  socketExists <- doesFileExist socketPath
  when socketExists $ removeFile socketPath
  bracket aquire release \sock -> do
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock (SockAddrUnix socketPath)
    listen sock 128
    mask_ $ forever $ do
      (conn, _) <- accept sock
      onException (atomically (socketFn conn)) (close conn)
  where
    aquire = socket AF_UNIX Stream defaultProtocol
    release sock = close sock >> removeFile socketPath
