{-# LANGUAGE DeriveLift #-}

module Quasar.Wayland.Protocol.Core (
  ObjectId,
  Opcode,
  ArgumentType(..),
  Fixed,
  IsSide,
  Side(..),
  IsInterface(..),
  IsInterfaceSide(..),
  Object,
  IsObject(..),
  IsObject,
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

  showObjectMessage,

  -- * Message decoder operations
  WireFormat(..),
  dropRemaining,
) where

import Control.Monad (replicateM_)
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Writer (WriterT, runWriterT, execWriterT, tell)
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
import Data.Kind
import Data.Maybe (isJust)
import Data.Typeable (Typeable, cast)
import Data.Void (absurd)
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift)
import Quasar.Prelude


type ObjectId = Word32
type Opcode = Word16

-- | Signed 24.8 decimal numbers.
newtype Fixed = Fixed Word32
  deriving newtype Eq

instance Show Fixed where
  show x = "[fixed " <> show x <> "]"

newtype NewId = NewId ObjectId
  deriving newtype (Eq, Show)


dropRemaining :: Get ()
dropRemaining = void getRemainingLazyByteString



data ArgumentType
  = IntArgument
  | UIntArgument
  | FixedArgument
  | StringArgument
  | ArrayArgument
  | ObjectArgument String
  | UnknownObjectArgument
  | NewIdArgument String
  | UnknownNewIdArgument
  | FdArgument
  deriving stock (Eq, Show, Lift)

class (Eq (Argument a), Show (Argument a)) => WireFormat a where
  type Argument a
  putArgument :: Argument a -> PutM ()
  getArgument :: Get (Argument a)
  showArgument :: Argument a -> String

instance WireFormat 'IntArgument where
  type Argument 'IntArgument = Int32
  putArgument = putInt32host
  getArgument = getInt32host
  showArgument = show

instance WireFormat 'UIntArgument where
  type Argument 'UIntArgument = Word32
  putArgument = putWord32host
  getArgument = getWord32host
  showArgument = show

instance WireFormat 'FixedArgument where
  type Argument 'FixedArgument = Fixed
  putArgument (Fixed repr) = putWord32host repr
  getArgument = Fixed <$> getWord32host
  showArgument = show

instance WireFormat 'StringArgument where
  type Argument 'StringArgument = BS.ByteString
  putArgument = putWaylandBlob
  getArgument = getWaylandBlob
  showArgument = show

instance WireFormat 'ArrayArgument where
  type Argument 'ArrayArgument = BS.ByteString
  putArgument = putWaylandBlob
  getArgument = getWaylandBlob
  showArgument array = "[array " <> show (BS.length array) <> "B]"

instance WireFormat 'ObjectArgument where
  type Argument 'ObjectArgument = ObjectId
  putArgument = putWord32host
  getArgument = getWord32host
  showArgument oId = "@" <> show oId

instance WireFormat 'UnknownObjectArgument where
  type Argument 'UnknownObjectArgument = ObjectId
  putArgument = putWord32host
  getArgument = getWord32host
  showArgument oId = "@" <> show oId

instance WireFormat 'NewIdArgument where
  type Argument 'NewIdArgument = NewId
  putArgument (NewId newId) = putWord32host newId
  getArgument = NewId <$> getWord32host
  showArgument newId = "new @" <> show newId

instance WireFormat 'UnknownNewIdArgument where
  type Argument 'UnknownNewIdArgument = NewId
  putArgument (NewId newId) = putWord32host newId
  getArgument = NewId <$> getWord32host
  showArgument newId = "new @" <> show newId

instance WireFormat 'FdArgument where
  type Argument 'FdArgument = Void
  putArgument = undefined
  getArgument = undefined
  showArgument = undefined


-- | A proxy type (in the haskell sense) for a Wayland interface.
class
  (
    IsMessage (Request i),
    IsMessage (Event i)
  )
  => IsInterface i where
  type Request i
  type Event i
  interfaceName :: String

class IsSide (s :: Side) where
  type Up s i
  type Down s i
  getDown :: forall m i. IsInterface i => Object s m i -> Opcode -> Get (Down s i)

instance IsSide 'Client where
  type Up 'Client i = Request i
  type Down 'Client i = Event i
  getDown :: forall m i. IsInterface i => Object 'Client m i -> Opcode -> Get (Down 'Client i)
  getDown = getMessage @(Down 'Client i)

instance IsSide 'Server where
  type Up 'Server i = Event i
  type Down 'Server i = Request i
  getDown :: forall m i. IsInterface i => Object 'Server m i -> Opcode -> Get (Down 'Server i)
  getDown = getMessage @(Down 'Server i)


-- | Empty class, only required to combine constraints
class (IsSide s, IsInterface i, IsMessage (Up s i), IsMessage (Down s i)) => IsInterfaceSide (s :: Side) i


-- | Data kind
data Side = Client | Server

data Object s m i = IsInterfaceSide s i => Object ObjectId (Callback s m i)

class IsObject a where
  objectId :: a -> ObjectId
  objectInterfaceName :: a -> String

class IsObjectSide a where
  describeUpMessage :: a -> Opcode -> BSL.ByteString -> String
  describeDownMessage :: a -> Opcode -> BSL.ByteString -> String

instance forall s m i. IsInterface i => IsObject (Object s m i) where
  objectId (Object oId _) = oId
  objectInterfaceName _ = interfaceName @i

instance forall s m i. IsInterfaceSide s i => IsObjectSide (Object s m i) where
  describeUpMessage object opcode body =
    objectInterfaceName object <> "@" <> show (objectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(Up s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"
  describeDownMessage object opcode body =
    objectInterfaceName object <> "@" <> show (objectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(Down s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"

-- | Wayland object quantification wrapper
data SomeObject s m
  = forall i. IsInterfaceSide s i => SomeObject (Object s m i)
  | UnknownObject String ObjectId

instance IsObject (SomeObject s m) where
  objectId (SomeObject object) = objectId object
  objectId (UnknownObject _ oId) = oId
  objectInterfaceName (SomeObject object) = objectInterfaceName object
  objectInterfaceName (UnknownObject interface _) = interface

instance IsObjectSide (SomeObject s m) where
  describeUpMessage (SomeObject object) = describeUpMessage object
  describeUpMessage (UnknownObject interface oId) =
    \opcode body -> interface <> "@" <> show oId <> ".#" <> show opcode <>
      " (" <> show (BSL.length body) <> "B, unknown)"
  describeDownMessage (SomeObject object) = describeDownMessage object
  describeDownMessage (UnknownObject interface oId) =
    \opcode body -> interface <> "@" <> show oId <> ".#" <> show opcode <>
      " (" <> show (BSL.length body) <> "B, unknown)"


class (Eq a, Show a) => IsMessage a where
  opcodeName :: Opcode -> Maybe String
  getMessage :: IsInterface i => Object s m i -> Opcode -> Get a
  putMessage :: a -> PutM ()

instance IsMessage Void where
  opcodeName _ = Nothing
  getMessage = invalidOpcode
  putMessage = absurd

invalidOpcode :: IsInterface i => Object s m i -> Opcode -> Get a
invalidOpcode object opcode =
  fail $ "Invalid opcode " <> show opcode <> " on " <> objectInterfaceName object <> "@" <> show (objectId object)

showObjectMessage :: (IsObject a, IsMessage b) => a -> b -> String
showObjectMessage object message =
  objectInterfaceName object <> "@" <> show (objectId object) <> "." <> show message


-- TODO remove
data DynamicArgument
  = DynamicIntArgument Int32
  | DynamicUIntArgument Word32
  -- TODO
  | DynamicFixedArgument Void
  | DynamicStringArgument String
  | DynamicObjectArgument ObjectId
  | DynamicNewIdArgument ObjectId
  | DynamicFdArgument ()

dynamicArgumentSize :: DynamicArgument -> Word16
dynamicArgumentSize (DynamicIntArgument _) = 4
dynamicArgumentSize (DynamicUIntArgument _) = 4
dynamicArgumentSize (DynamicObjectArgument _) = 4
dynamicArgumentSize (DynamicNewIdArgument _) = 4
dynamicArgumentSize _ = undefined

putDynamicArgument :: DynamicArgument -> Put
putDynamicArgument (DynamicIntArgument x) = putInt32host x
putDynamicArgument (DynamicUIntArgument x) = putWord32host x
putDynamicArgument (DynamicObjectArgument x) = putWord32host x
putDynamicArgument (DynamicNewIdArgument x) = putWord32host x
putDynamicArgument _ = undefined


type ClientProtocolState m = ProtocolState 'Client m
type ServerProtocolState m = ProtocolState 'Server m

data ProtocolState (s :: Side) m = ProtocolState {
  protocolException :: Maybe SomeException,
  bytesReceived :: !Word64,
  bytesSent :: !Word64,
  inboxDecoder :: Decoder RawMessage,
  outbox :: Maybe Put,
  objects :: HashMap ObjectId (SomeObject s m)
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

data ParserFailed = ParserFailed String String
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
  :: forall wl_display wl_registry s m. (IsInterfaceSide s wl_display, IsInterfaceSide s wl_registry)
  => Callback s m wl_display
  -> Callback s m wl_registry
  -> ProtocolState s m
initialProtocolState wlDisplayCallback wlRegistryCallback = sendInitialMessage initialState
  where
    wlDisplay :: Object s m wl_display
    wlDisplay = Object 1 wlDisplayCallback
    wlRegistry :: Object s m wl_registry
    wlRegistry = Object 2 wlRegistryCallback
    initialState :: ProtocolState s m
    initialState = ProtocolState {
      protocolException = Nothing,
      bytesReceived = 0,
      bytesSent = 0,
      inboxDecoder = runGetIncremental getRawMessage,
      outbox = Nothing,
      objects = HM.fromList [(1, (SomeObject wlDisplay)), (2, (SomeObject wlRegistry))]
    }

-- | Feed the protocol newly received data
feedInput :: (IsSide s, MonadCatch m) => ByteString -> ProtocolStep s m ()
feedInput bytes = protocolStep do
  feed
  runCallbacks
  where
    feed = State.modify \st -> st {
      bytesReceived = st.bytesReceived + fromIntegral (BS.length bytes),
      inboxDecoder = pushChunk st.inboxDecoder bytes
    }

sendMessage :: (IsSide s, MonadCatch m) => Object s m i -> Up s i -> ProtocolStep s m ()
sendMessage object message = protocolStep do
  undefined message
  runCallbacks

setException :: (MonadCatch m, Exception e) => e -> ProtocolStep s m ()
setException ex = protocolStep do
  State.modify \st -> st{protocolException = Just (toException ex)}

-- * Internals

-- | Take data that has to be sent (if available)
takeOutbox :: MonadCatch m => ProtocolState s m ->  (Maybe BSL.ByteString, ProtocolState s m)
takeOutbox st = (maybeOutboxBytes, st{outbox = Nothing})
  where
    maybeOutboxBytes = if isJust st.protocolException then Nothing else outboxBytes
    outboxBytes = runPut <$> st.outbox


sendInitialMessage :: ProtocolState s m -> ProtocolState s m
sendInitialMessage = sendMessageInternal 1 1 [DynamicNewIdArgument 2]

runCallbacks :: (IsSide s, MonadCatch m) => StateT (ProtocolState s m) m ()
runCallbacks = receiveRawMessage >>= \case
  Nothing -> pure ()
  Just rawMessage -> do
    handleMessage rawMessage
    runCallbacks

handleMessage :: forall s m. (IsSide s, MonadCatch m) => RawMessage -> StateT (ProtocolState s m) m ()
handleMessage rawMessage@(oId, opcode, body) = do
  st <- State.get
  case HM.lookup oId st.objects of
    Nothing -> throwM $ ProtocolException $ "Received message with invalid object id " <> show oId

    Just (SomeObject object) ->
      case runGetOrFail (getMessageAction st.objects object rawMessage) body of
        Left (_, _, message) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) message
        Right ("", _, result) -> result
        Right (leftovers, _, _) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) (show (BSL.length leftovers) <> "B not parsed")

    Just (UnknownObject interface oId) -> do
      throwM $ ProtocolException $ "Received message for object without handler: " <> interface <> "@" <> show oId

getMessageAction
  :: (IsSide s, IsInterface i, MonadCatch m)
  => HashMap ObjectId (SomeObject s m)
  -> Object s m i
  -> RawMessage
  -> Get (ProtocolAction s m ())
getMessageAction objects object@(Object _ callback) (oId, opcode, body) = do
  message <- getDown object opcode
  pure $ traceM $ "<- " <> showObjectMessage object message

type ProtocolAction s m a = StateT (ProtocolState s m) m a

type RawMessage = (ObjectId, Opcode, BSL.ByteString)

receiveRawMessage :: forall s m a. MonadCatch m => StateT (ProtocolState s m) m (Maybe RawMessage)
receiveRawMessage = do
  st <- State.get
  (result, newDecoder) <- checkDecoder st.inboxDecoder
  State.put st{inboxDecoder = newDecoder}
  pure result
  where
    checkDecoder
      :: MonadCatch m
      => Decoder RawMessage
      -> StateT (ProtocolState s m) m (Maybe RawMessage, Decoder RawMessage)
    checkDecoder (Fail _ _ message) = throwM (ParserFailed "RawMessage" message)
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
    msgSizeInteger = foldr ((+) . (fromIntegral . dynamicArgumentSize)) 8 args :: Integer

sendRaw :: Put -> ProtocolState s m -> ProtocolState s m
sendRaw x oldState = oldState {
  outbox = Just (maybe x (<> x) oldState.outbox)
}
