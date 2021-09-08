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
  ProtocolStep,
  initialProtocolState,
  sendMessage,
  feedInput,
  setException,
) where

import Control.Monad (replicateM_)
import Control.Monad.Catch
import Control.Monad.State (StateT, runStateT, lift)
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
import GHC.TypeLits
import Quasar.Prelude


type ObjectId = Word32
type Opcode = Word16

-- | Signed 24.8 decimal numbers.
newtype Fixed = Fixed Word32
  deriving Eq

class WireFormat a where
  type Argument a
  putArgument :: Argument a -> StateT (ProtocolState s m) PutM ()
  getArgument :: StateT (ProtocolState s m) Get (Argument a)

instance WireFormat "int" where
  type Argument "int" = Int32
  putArgument = lift . putInt32host
  getArgument = lift getInt32host

instance WireFormat "uint" where
  type Argument "uint" = Word32
  putArgument = lift . putWord32host
  getArgument = lift getWord32host

instance WireFormat "fixed" where
  type Argument "fixed" = Fixed
  putArgument (Fixed repr) = lift $ putWord32host repr
  getArgument = lift $ Fixed <$> getWord32host

instance WireFormat "string" where
  type Argument "string" = BS.ByteString
  putArgument = lift . putWaylandBlob
  getArgument = lift getWaylandBlob

data WireObject s m i

instance forall (s :: Side) m i. MonadCatch m => WireFormat (WireObject s m i) where
  type Argument (WireObject s m i) = Object s m i
  putArgument = undefined
  getArgument = undefined

instance WireFormat "new_id" where
  type Argument "new_id" = Void
  putArgument = undefined
  getArgument = undefined

instance WireFormat "array" where
  type Argument "array" = BS.ByteString
  putArgument = lift . putWaylandBlob
  getArgument = lift getWaylandBlob

instance WireFormat "fd" where
  type Argument "fd" = Void
  putArgument = undefined
  getArgument = undefined



-- | A wayland interface
class (Binary (TRequest i), Binary (TEvent i)) => IsInterface i where
  type TRequest i
  type TEvent i
  interfaceName :: String

class IsInterface i => IsObject (s :: Side) i where
  type Up s i
  type Down s i

data Side = Client | Server

data Object s m i = IsInterface i => Object ObjectId (Callback s m i)

instance IsInterface i => IsObject 'Client i where
  type Up 'Client i = TRequest i
  type Down 'Client i = TEvent i

instance IsInterface i => IsObject 'Server i where
  type Up 'Server i = TEvent i
  type Down 'Server i = TRequest i

instance forall s m i. IsInterface i => IsSomeObject (Object s m i) where
  objectId (Object oId _) = oId
  objectInterfaceName _ = interfaceName @i


class IsSomeObject i where
  objectId :: i -> ObjectId
  objectInterfaceName :: i -> String

-- | Wayland object quantification wrapper
data SomeObject = forall i. IsSomeObject i => SomeObject i

instance IsSomeObject SomeObject where
  objectId (SomeObject object) = objectId object
  objectInterfaceName (SomeObject object) = objectInterfaceName object


class IsMessage i where
  messageName :: i -> String

instance IsMessage Void where
  messageName = absurd


-- TODO remove
data DynamicArgument
  = IntArgument Int32
  | UIntArgument Word32
  -- TODO
  | FixedArgument Void
  | StringArgument String
  | ObjectArgument ObjectId
  | NewIdArgument ObjectId
  | FdArgument ()

argumentSize :: DynamicArgument -> Word16
argumentSize (IntArgument _) = 4
argumentSize (UIntArgument _) = 4
argumentSize (ObjectArgument _) = 4
argumentSize (NewIdArgument _) = 4
argumentSize _ = undefined

putDynamicArgument :: DynamicArgument -> Put
putDynamicArgument (IntArgument x) = putInt32host x
putDynamicArgument (UIntArgument x) = putWord32host x
putDynamicArgument (ObjectArgument x) = putWord32host x
putDynamicArgument (NewIdArgument x) = putWord32host x
putDynamicArgument _ = undefined



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


type ClientCallback m i = Callback 'Client m i
type ServerCallback m i = Callback 'Server m i

data Callback s m i = Callback {
  messageCallback :: Object s m i -> Down s i -> StateT (ProtocolState s m) m ()
}

-- * Exceptions

data CallbackFailed = CallbackFailed SomeException
  deriving stock Show
  deriving anyclass Exception

data ParserFailed = ParserFailed String
  deriving stock Show
  deriving anyclass Exception

data ProtocolException = ProtocolException String
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
    wlDisplay = Object 1 wlDisplayCallback
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
    feed = State.modify \st -> st {
      bytesReceived = st.bytesReceived + fromIntegral (BS.length bytes),
      inboxDecoder = pushChunk st.inboxDecoder bytes
    }

sendMessage :: MonadCatch m => Object s m i -> Up s i -> ProtocolStep s m ()
sendMessage object message = protocolStep do
  undefined message
  runCallbacks

setException :: (MonadCatch m, Exception e) => e -> ProtocolStep s m ()
setException ex = protocolStep do
  State.modify \st -> st{protocolException = Just (toException ex)}

-- * Internals

-- | Take data that has to be sent (if available)
takeOutbox :: MonadCatch m => ProtocolState s m ->  (Maybe BSL.ByteString, ProtocolState s m)
takeOutbox st = (outboxBytes, st{outbox = Nothing})
  where
    outboxBytes = if isJust st.protocolException then Nothing else runPut <$> st.outbox


sendInitialMessage :: ProtocolState s m -> ProtocolState s m
sendInitialMessage = sendMessageInternal 1 1 [NewIdArgument 2]

runCallbacks :: MonadCatch m => StateT (ProtocolState s m) m ()
runCallbacks = receiveRawMessage >>= \case
  Nothing -> pure ()
  Just message -> do
    handleMessage message
    runCallbacks

handleMessage :: MonadCatch m => RawMessage -> StateT (ProtocolState s m) m ()
handleMessage (oId, opcode, body) = do
  st <- State.get
  case HM.lookup oId st.objects of
    Nothing -> throwM $ ProtocolException $ "Received message with invalid object id " <> show oId
    Just object -> traceM (objectInterfaceName object)

type RawMessage = (ObjectId, Opcode, BSL.ByteString)

receiveRawMessage :: MonadCatch m => StateT (ProtocolState s m) m (Maybe RawMessage)
receiveRawMessage = do
  st <- State.get
  (result, newDecoder) <- checkDecoder st.inboxDecoder
  State.put st{inboxDecoder = newDecoder}

  pure result
  where
    checkDecoder :: MonadCatch m => Decoder RawMessage -> StateT (ProtocolState s m) m (Maybe RawMessage, Decoder RawMessage)
    checkDecoder (Fail _ _ message) = throwM (ParserFailed message)
    checkDecoder x@(Partial _) = pure (Nothing, x)
    checkDecoder (Done leftovers _ result) = pure (Just result, pushChunk (runGetIncremental getRawMessage) leftovers)

getRawMessage :: Get RawMessage
getRawMessage = do
  oId <- getWord32host
  sizeAndOpcode <- getWord32host
  let
    size = fromIntegral (sizeAndOpcode `shiftR` 16) - 8
    opcode = fromIntegral (sizeAndOpcode .&. 0xFFFF)
  body <- getLazyByteString size
  pure (oId, opcode, body)

getWaylandBlob :: Get BS.ByteString
getWaylandBlob = do
  size <- getWord32host
  Just (string, 0) <- BS.unsnoc <$> getByteString (fromIntegral size)
  skipPadding
  pure string

putWaylandBlob :: BS.ByteString -> Put
putWaylandBlob blob = do
  let size = BS.length blob
  putWord32host (fromIntegral size)
  putByteString blob
  putWord8 0
  replicateM_ ((4 - (size `mod` 4)) `mod` 4) (putWord8 0)


skipPadding :: Get ()
skipPadding = do
  bytes <- bytesRead
  skip $ fromIntegral ((4 - (bytes `mod` 4)) `mod` 4)


sendMessageInternal :: ObjectId -> Opcode -> [DynamicArgument] -> ProtocolState s m -> ProtocolState s m
sendMessageInternal oId opcode args = sendRaw do
  putWord32host oId
  putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode
  mapM_ putDynamicArgument args
  -- TODO padding
  where
    msgSize :: Word16
    msgSize = if msgSizeInteger <= fromIntegral (maxBound :: Word16) then fromIntegral msgSizeInteger else undefined
    msgSizeInteger :: Integer
    msgSizeInteger = foldr ((+) . (fromIntegral . argumentSize)) 8 args :: Integer

sendRaw :: Put -> ProtocolState s m -> ProtocolState s m
sendRaw x oldState = oldState {
  outbox = Just (maybe x (<> x) oldState.outbox)
}
