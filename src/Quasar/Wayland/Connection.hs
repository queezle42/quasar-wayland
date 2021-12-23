{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Connection (
  WaylandConnection,
  newWaylandConnection,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal (createUptoN)
import Data.ByteString.Lazy qualified as BSL
import Foreign.Storable (sizeOf)
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket
import Network.Socket.ByteString.Lazy qualified as SocketL
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Utils.Socket
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd)


C.include "<sys/socket.h>"

maxFds :: C.CInt
maxFds = 28 -- from wayland (connection.c)

cmsgBufferSize :: Int
cmsgBufferSize = fromIntegral [CU.pure|int { CMSG_LEN($(int maxFds) * sizeof(int32_t)) }|]



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

    t1 <- connectionThread connection $ sendThread connection
    t2 <- connectionThread connection $ receiveThread connection

    registerAsyncDisposeAction do
      await $ isDisposed t1
      await $ isDisposed t2
      closeConnection connection

    pure (result, connection)

connectionThread :: MonadResourceManager m => WaylandConnection s -> IO () -> m (Async ())
connectionThread connection work = asyncWithHandler traceAndDisposeConnection $ liftIO $ work
  where
    traceAndDisposeConnection :: SomeException -> IO ()
    traceAndDisposeConnection ex = traceIO (displayException ex) >> void (dispose connection)

sendThread :: WaylandConnection s -> IO ()
sendThread connection = mask_ $ forever do
  (msg, fds) <- takeOutbox connection.protocolHandle
  finally
    do
      traceIO $ "Sending " <> show (BSL.length msg) <> " bytes"

      -- TODO limit max fds
      send (fromIntegral (BSL.length msg)) (BSL.toChunks msg) fds

    do
      mapM closeFd fds
  where
    send :: Int -> [BS.ByteString] -> [Fd] -> IO ()
    send remaining chunks fds = do
      -- TODO add MSG_NOSIGNAL (not exposed by `network`)
      sent <- sendMsg connection.socket chunks (Socket.encodeCmsg <$> fds) mempty
      let nowRemaining = remaining - sent
      when (nowRemaining > 0) do
        send nowRemaining (drop sent chunks) []

    drop :: Int -> [BS.ByteString] -> [BS.ByteString]
    drop _ [] = []
    drop amount (chunk:chunks) =
      if (amount < BS.length chunk)
        then (BS.drop amount chunk : chunks)
        else drop (amount - BS.length chunk) chunks


receiveThread :: IsSide s => WaylandConnection s -> IO ()
receiveThread connection = forever do
  -- TODO add MSG_CMSG_CLOEXEC (not exposed by `network`)
  (chunk, cmsgs, flags) <- recvMsg connection.socket 4096 cmsgBufferSize mempty

  let fds = catMaybes (Socket.decodeCmsg @Fd <$> cmsgs)

  when (flags .&. Socket.MSG_CTRUNC > 0) do
    -- TODO close fds
    fail "Wayland connection: Ancillary data was truncated"

  when (length fds /= length cmsgs) do
    -- TODO close fds
    fail "Wayland connection: Received unexpected ancillary message (only SCM_RIGHTS is supported)"

  when (BS.null chunk) do
    throwM SocketClosed

  traceIO $ "Received " <> show (BS.length chunk) <> " bytes"

  feedInput connection.protocolHandle chunk fds


closeConnection :: WaylandConnection s -> IO ()
closeConnection connection = Socket.close connection.socket
