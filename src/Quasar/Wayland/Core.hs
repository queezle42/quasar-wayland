module Quasar.Wayland.Core (
  ObjectId,
  Opcode,
  IsInterface(..),
  Side(..),
  Object,
  IsSomeObject,
  IsMessage(..),
  ProtocolState,
  ClientProtocolState,
  ServerProtocolState,
  ClientCallback,
  ServerCallback,
  Callback(..),
  Request,
  Event,
  ProtocolStep,
  initialProtocolState,
  sendMessage,
  feedInput,
  setException,
) where

import Control.Monad.Catch
import Control.Monad.State (StateT, runStateT, state, modify)
import Control.Monad.State qualified as State
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (isJust)
import Data.Void (absurd)
import Quasar.Prelude


type ObjectId = Word32
type Opcode = Word16


-- | A wayland interface
class (Binary (TRequest a), Binary (TEvent a)) => IsInterface a where
  type TRequest a
  type TEvent a
  interfaceName :: String

class IsInterface a => IsObject (s :: Side) a where
  type Up s a
  type Down s a

data Side = Client | Server

data Object s m a = IsInterface a => Object ObjectId (Callback s m a)

instance IsInterface a => IsObject 'Client a where
  type Up 'Client a = TRequest a
  type Down 'Client a = TEvent a

instance IsInterface a => IsObject 'Server a where
  type Up 'Server a = TEvent a
  type Down 'Server a = TRequest a

instance forall s m a. IsInterface a => IsSomeObject (Object s m a) where
  objectId (Object oId _) = oId
  objectInterfaceName _ = interfaceName @a

mkObject :: forall s m a. IsInterface a => ObjectId -> Callback s m a -> Object s m a
mkObject oId callback = Object @s @m @a oId callback


class IsSomeObject a where
  objectId :: a -> ObjectId
  objectInterfaceName :: a -> String

-- | Wayland object quantification wrapper
data SomeObject = forall a. IsSomeObject a => SomeObject a

instance IsSomeObject SomeObject where
  objectId (SomeObject object) = objectId object
  objectInterfaceName (SomeObject object) = objectInterfaceName object


class IsMessage a where
  messageName :: a -> String

instance IsMessage Void where
  messageName = absurd


data Argument
  = IntArgument Int32
  | UIntArgument Word32
  -- TODO
  | FixedArgument Void
  | StringArgument String
  | ObjectArgument ObjectId
  | NewIdArgument ObjectId
  | FdArgument ()

argumentSize :: Argument -> Word16
argumentSize (IntArgument _) = 4
argumentSize (UIntArgument _) = 4
argumentSize (ObjectArgument _) = 4
argumentSize (NewIdArgument _) = 4
argumentSize _ = undefined

putArgument :: Argument -> Put
putArgument (IntArgument x) = putInt32host x
putArgument (UIntArgument x) = putWord32host x
putArgument (ObjectArgument x) = putWord32host x
putArgument (NewIdArgument x) = putWord32host x
putArgument _ = undefined


type ClientProtocolState m = ProtocolState 'Client m
type ServerProtocolState m = ProtocolState 'Server m

data ProtocolState (s :: Side) m = ProtocolState {
  protocolException :: Maybe SomeException,
  bytesReceived :: Word64,
  bytesSent :: Word64,
  inboxDecoder :: Decoder (ObjectId, Opcode, BSL.ByteString),
  outbox :: Maybe Put,
  objects :: HashMap ObjectId SomeObject
}

data Request = Request ObjectId Opcode BSL.ByteString
  deriving stock Show
data Event = Event ObjectId Opcode (Either BSL.ByteString (Word32, BSL.ByteString, Word32))
  deriving stock Show


type ClientCallback m a = Callback 'Client m a
type ServerCallback m a = Callback 'Server m a

data Callback s m a = Callback {
  messageCallback :: Object s m a -> Down s a -> StateT (ProtocolState s m) m ()
}

-- * Exceptions

data CallbackFailed = CallbackFailed SomeException
  deriving stock Show
  deriving anyclass Exception

data ParserFailed = ParserFailed String
  deriving stock Show
  deriving anyclass Exception

-- * Monad plumbing

type ProtocolStep s m a = ProtocolState s m -> m (Either SomeException a, Maybe BSL.ByteString, ProtocolState s m)

protocolStep :: forall s m a. MonadCatch m => StateT (ProtocolState s m) m a -> ProtocolStep s m a
protocolStep action inState = do
  mapM_ throwM inState.protocolException
  (result, (outbox, outState)) <- fmap takeOutbox . storeExceptionIfFailed <$> runStateT (try action) inState
  pure (result, outbox, outState)
  where
    storeExceptionIfFailed :: (Either SomeException a, ProtocolState s m) -> (Either SomeException a, ProtocolState s m)
    storeExceptionIfFailed (Left ex, st) = (Left ex, setException ex st)
    storeExceptionIfFailed x = x
    setException :: (MonadCatch m, Exception e) => e -> (ProtocolState s m) -> (ProtocolState s m)
    setException ex st =
      if isJust st.protocolException
        then st
        else st{protocolException = Just (toException ex)}

-- * Exported functions

initialProtocolState
  :: forall wl_display s m. IsInterface wl_display
  => Callback s m wl_display
  -> ProtocolState s m
initialProtocolState wlDisplayCallback = sendInitialMessage initialState
  where
    wlDisplay :: Object s m wl_display
    wlDisplay = mkObject 1 wlDisplayCallback
    initialState :: ProtocolState s m
    initialState = ProtocolState {
      protocolException = Nothing,
      bytesReceived = 0,
      bytesSent = 0,
      inboxDecoder = runGetIncremental getRawMessage,
      outbox = Nothing,
      objects = HM.singleton 1 (SomeObject wlDisplay)
    }

-- | Feed the protocol newly received data
feedInput :: MonadCatch m => ByteString -> ProtocolStep s m ()
feedInput bytes = protocolStep do
  feed
  runCallbacks
  where
    feed = modify \st -> st {
      bytesReceived = st.bytesReceived + fromIntegral (BS.length bytes),
      inboxDecoder = pushChunk st.inboxDecoder bytes
    }

sendMessage :: MonadCatch m => Object s m a -> Up s a -> ProtocolStep s m ()
sendMessage object message = protocolStep do
  undefined message
  runCallbacks

setException :: (MonadCatch m, Exception e) => e -> ProtocolStep s m ()
setException ex = protocolStep do
  modify \st -> st{protocolException = Just (toException ex)}

-- * Internals

-- | Take data that has to be sent (if available)
takeOutbox :: MonadCatch m => ProtocolState s m ->  (Maybe BSL.ByteString, ProtocolState s m)
takeOutbox st = (runPut <$> st.outbox, st{outbox = Nothing})


sendInitialMessage :: ProtocolState s m -> ProtocolState s m
sendInitialMessage = sendMessageInternal 1 1 [NewIdArgument 2]

runCallbacks :: MonadCatch m => StateT (ProtocolState s m) m ()
runCallbacks = receiveRawMessage >>= \case
  Nothing -> pure ()
  Just message -> do
    traceM $ show message
    runCallbacks


type RawMessage = (ObjectId, Opcode, BSL.ByteString)

getRawMessage :: Get RawMessage
getRawMessage = do
  oId <- getWord32host
  sizeAndOpcode <- getWord32host
  let
    size = fromIntegral (sizeAndOpcode `shiftR` 16) - 8
    opcode = fromIntegral (sizeAndOpcode .&. 0xFFFF)
  body <- getLazyByteString size
  pure (oId, opcode, body)

receiveRawMessage :: MonadCatch m => StateT (ProtocolState s m) m (Maybe RawMessage)
receiveRawMessage = do
  st <- State.get
  (result, newDecoder) <- checkDecoder st.inboxDecoder
  State.put st{inboxDecoder = newDecoder}

  pure result
  where
    checkDecoder :: MonadCatch m => Decoder RawMessage -> StateT (ProtocolState s m) m (Maybe RawMessage, Decoder RawMessage)
    checkDecoder d@(Fail _ _ message) = throwM (ParserFailed message)
    checkDecoder x@(Partial _) = pure (Nothing, x)
    checkDecoder (Done leftovers _ result) = pure (Just result, pushChunk (runGetIncremental getRawMessage) leftovers)


decodeEvent :: Get Event
decodeEvent = do
  oId <- getWord32host
  sizeAndOpcode <- getWord32host
  let
    size = fromIntegral (sizeAndOpcode `shiftR` 16) - 8
    opcode = fromIntegral (sizeAndOpcode .&. 0xFFFF)
  body <- if (oId == 2 && opcode == 0)
             then Right <$> parseGlobal
             else Left <$> getLazyByteString size <* skipPadding
  pure $ Event oId opcode body
  where
    parseGlobal :: Get (Word32, BSL.ByteString, Word32)
    parseGlobal = (,,) <$> getWord32host <*> getWaylandString <*> getWord32host

getWaylandString :: Get BSL.ByteString
getWaylandString = do
  size <- getWord32host
  Just (string, 0) <- BSL.unsnoc <$> getLazyByteString (fromIntegral size)
  skipPadding
  pure string

skipPadding :: Get ()
skipPadding = do
  bytes <- bytesRead
  skip $ fromIntegral ((4 - (bytes `mod` 4)) `mod` 4)


sendMessageInternal :: ObjectId -> Opcode -> [Argument] -> ProtocolState s m -> ProtocolState s m
sendMessageInternal oId opcode args = sendRaw do
  putWord32host oId
  putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode
  mapM_ putArgument args
  -- TODO padding
  where
    msgSize :: Word16
    msgSize = if msgSizeInteger <= fromIntegral (maxBound :: Word16) then fromIntegral msgSizeInteger else undefined
    msgSizeInteger :: Integer
    msgSizeInteger = foldr ((+) . (fromIntegral . argumentSize)) 8 args :: Integer

sendRaw :: Put -> ProtocolState s m -> ProtocolState s m
sendRaw put oldState = oldState {
  outbox = Just (maybe put (<> put) oldState.outbox)
}
