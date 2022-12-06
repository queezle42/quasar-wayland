module Quasar.Wayland.Utils.Socket (
  recvMsg,
  sendMsg,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal (ByteString(PS), create)
import Foreign
import Network.Socket
import Network.Socket.Address qualified as SA
import Network.Socket.Internal (zeroMemory)
import Quasar.Prelude


instance SA.SocketAddress () where
  sizeOfSocketAddress _ = 0
  peekSocketAddress _ptr = pure ()
  pokeSocketAddress _ptr () = pure ()



withBufSizs :: [ByteString] -> ([(Ptr Word8, Int)] -> IO a) -> IO a
withBufSizs bss0 f = loop bss0 id
  where
    loop []                    !build = f $ build []
    loop (PS fptr off len:bss) !build = withForeignPtr fptr $ \ptr -> do
        let !ptr' = ptr `plusPtr` off
        loop bss (build . ((ptr',len) :))

-- | Send data to the connected socket using sendmsg(2).
sendMsg :: Socket       -- ^ Socket
        -> [BS.ByteString] -- ^ Data to be sent
        -> [Cmsg]       -- ^ Control messages
        -> MsgFlag      -- ^ Message flags
        -> IO Int       -- ^ The length actually sent
sendMsg _    []  _ _ = pure 0
sendMsg s bss cmsgs flags = withBufSizs bss $ \bufsizs ->
    SA.sendBufMsg s () bufsizs cmsgs flags

-- | Receive data from the connected socket using recvmsg(2).
recvMsg :: Socket  -- ^ Socket
        -> Int     -- ^ The maximum length of data to be received
                   --   If the total length is not large enough,
                   --   'MSG_TRUNC' is returned
        -> Int     -- ^ The buffer size for control messages.
                   --   If the length is not large enough,
                   --   'MSG_CTRUNC' is returned
        -> MsgFlag -- ^ Message flags
        -> IO (BS.ByteString, [Cmsg], MsgFlag) -- ^ Source address, received data, control messages and message flags
recvMsg s siz clen flags = do
    bs@(PS fptr _ _) <- create siz $ \ptr -> zeroMemory ptr (fromIntegral siz)
    withForeignPtr fptr $ \ptr -> do
        ((),len,cmsgs,flags') <- SA.recvBufMsg s [(ptr,siz)] clen flags
        let bs' | len < siz = PS fptr 0 len
                | otherwise = bs
        pure (bs', cmsgs, flags')
