{-# LANGUAGE DeriveLift #-}

module Quasar.Wayland.Protocol.Core (
  ObjectId,
  NewId(..),
  Opcode,
  ArgumentType(..),
  Fixed,
  IsSide(..),
  Side(..),
  IsInterface(..),
  IsInterfaceSide,
  IsInterfaceHandler(..),
  Object,
  IsObject,
  IsMessage(..),
  ProtocolState,
  Callback(..),
  internalFnCallback,
  traceCallback,
  ignoreMessage,
  ProtocolStep,
  initialProtocolState,
  sendMessage,
  feedInput,
  setException,

  showObjectMessage,

  -- * Message decoder operations
  WireFormat(..),
  dropRemaining,
  invalidOpcode,
) where

import Control.Monad (replicateM_)
import Control.Monad.Catch
import Control.Monad.State (StateT, runStateT)
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


-- | Class for a proxy type (in the haskell sense) that describes a Wayland interface.
class (
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

instance IsSide 'Client where
  type Up 'Client i = Request i
  type Down 'Client i = Event i

instance IsSide 'Server where
  type Up 'Server i = Event i
  type Down 'Server i = Request i


--- | Empty class, used to combine constraints
class (
    IsSide s,
    IsInterface i,
    IsMessage (Up s i),
    IsMessage (Down s i)
  )
  => IsInterfaceSide (s :: Side) i


getDown :: forall s m i. IsInterfaceSide s i => Object s m i -> Opcode -> Get (Down s i)
getDown = getMessage @(Down s i)

putUp :: forall s m i. IsInterfaceSide s i => Object s m i -> Up s i -> PutM Opcode
putUp _ = putMessage @(Up s i)


class IsInterfaceSide s i => IsInterfaceHandler s m i a where
  handleMessage :: a -> Object s m i -> Down s i -> ProtocolAction s m ()


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
  putMessage :: a -> PutM Opcode

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


data ProtocolState (s :: Side) m = ProtocolState {
  protocolException :: Maybe SomeException,
  bytesReceived :: !Int64,
  bytesSent :: !Int64,
  inboxDecoder :: Decoder RawMessage,
  outbox :: Maybe Put,
  objects :: HashMap ObjectId (SomeObject s m)
}


data Callback s m i = forall a. IsInterfaceHandler s m i a => Callback a

instance IsInterfaceSide s i => IsInterfaceHandler s m i (Callback s m i) where
  handleMessage (Callback callback) = handleMessage callback


data LowLevelCallback s m i = IsInterfaceSide s i => FnCallback (Object s m i -> Down s i -> ProtocolAction s m ())

instance IsInterfaceSide s i => IsInterfaceHandler s m i (LowLevelCallback s m i) where
  handleMessage (FnCallback fn) object msg = fn object msg

internalFnCallback :: IsInterfaceSide s i => (Object s m i -> Down s i -> ProtocolAction s m ()) -> Callback s m i
internalFnCallback = Callback . FnCallback


{-# WARNING traceCallback "Trace." #-}
-- | The 'traceCallback' callback outputs a trace for every received message, before passing the message to the callback
-- argument.
--
-- The 'trace' function should /only/ be used for debugging, or for monitoring execution. The function is not
-- referentially transparent: its type indicates that it is a pure function but it has the side effect of outputting the
-- trace message.
--
-- Uses `traceM` internally.
traceCallback :: (IsInterfaceSide 'Client i, Monad m) => Callback 'Client m i -> Callback 'Client m i
traceCallback next = internalFnCallback \object message -> do
  traceM $ "<- " <> showObjectMessage object message
  handleMessage next object message

-- | A `Callback` that ignores all messages. Intended for development purposes, e.g. together with `traceCallback`.
ignoreMessage :: (IsInterfaceSide 'Client i, Monad m) => Callback 'Client m i
ignoreMessage = internalFnCallback \_ _ -> pure ()

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

type ProtocolAction s m a = StateT (ProtocolState s m) m a

protocolStep :: forall s m a. MonadCatch m => ProtocolAction s m a -> ProtocolStep s m a
protocolStep action inState = do
  mapM_ throwM inState.protocolException
  (result, (outbox, outState)) <- fmap takeOutbox . storeExceptionIfFailed <$> runStateT (try action) inState
  pure (result, outbox, outState)
  where
    storeExceptionIfFailed :: (Either SomeException a, ProtocolState s m) -> (Either SomeException a, ProtocolState s m)
    storeExceptionIfFailed (Left ex, st) = (Left ex, setException' ex st)
    storeExceptionIfFailed x = x
    setException' :: Exception e => e -> (ProtocolState s m) -> (ProtocolState s m)
    setException' ex st =
      if isJust st.protocolException
        then st
        else st{protocolException = Just (toException ex)}


-- * Exported functions

initialProtocolState
  :: forall wl_display wl_registry s m. (IsInterfaceSide s wl_display, IsInterfaceSide s wl_registry)
  => Callback s m wl_display
  -> Callback s m wl_registry
  -> (ProtocolState s m, Object s m wl_display)
initialProtocolState wlDisplayCallback wlRegistryCallback = (initialState, wlDisplay)
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
  receiveMessages
  where
    feed = State.modify \st -> st {
      bytesReceived = st.bytesReceived + fromIntegral (BS.length bytes),
      inboxDecoder = pushChunk st.inboxDecoder bytes
    }

setException :: (MonadCatch m, Exception e) => e -> ProtocolStep s m ()
setException ex = protocolStep do
  State.modify \st -> st{protocolException = Just (toException ex)}

-- | Sends a message without checking any ids or creating proxy objects objects.
sendMessage :: forall s m i. (IsInterfaceSide s i, MonadCatch m) => Object s m i -> Up s i -> ProtocolStep s m ()
sendMessage object message = protocolStep $ sendMessageInternal object message

sendMessageInternal :: forall s m i. (IsInterfaceSide s i, MonadCatch m) => Object s m i -> Up s i -> ProtocolAction s m ()
sendMessageInternal object message = do
  traceM $ "-> " <> showObjectMessage object message
  sendRawMessage messageWithHeader
  where
    body :: BSL.ByteString
    opcode :: Opcode
    (opcode, body) = runPutM $ putUp object message
    messageWithHeader :: Put
    messageWithHeader = do
      putWord32host $ objectId object
      putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode
      putLazyByteString body
    msgSize :: Word16
    msgSize = if msgSizeInteger <= fromIntegral (maxBound :: Word16) then fromIntegral msgSizeInteger else error "Message too large"
    -- TODO: body length should be returned from `putMessage`, instead of realizing it to a ByteString here
    msgSizeInteger :: Integer
    msgSizeInteger = 8 + fromIntegral (BSL.length body)

-- | Take data that has to be sent (if available)
takeOutbox :: ProtocolState s m ->  (Maybe BSL.ByteString, ProtocolState s m)
takeOutbox st = (maybeOutboxData, st{outbox = Nothing, bytesSent = st.bytesSent + outboxNumBytes})
  where
    maybeOutboxData = if isJust st.protocolException then Nothing else outboxData
    outboxData = runPut <$> st.outbox
    outboxNumBytes = maybe 0 BSL.length maybeOutboxData


receiveMessages :: (IsSide s, MonadCatch m) => ProtocolAction s m ()
receiveMessages = receiveRawMessage >>= \case
  Nothing -> pure ()
  Just rawMessage -> do
    handleRawMessage rawMessage
    receiveMessages

handleRawMessage :: forall s m. MonadCatch m => RawMessage -> ProtocolAction s m ()
handleRawMessage (oId, opcode, body) = do
  objects <- State.gets (.objects)
  case HM.lookup oId objects of
    Nothing -> throwM $ ProtocolException $ "Received message with invalid object id " <> show oId

    Just (SomeObject object) ->
      case runGetOrFail (getMessageAction object opcode) body of
        Left (_, _, message) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) message
        Right ("", _, result) -> result
        Right (leftovers, _, _) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) (show (BSL.length leftovers) <> "B not parsed")

    Just (UnknownObject interface _) -> do
      throwM $ ProtocolException $ "Received message for object without handler: " <> interface <> "@" <> show oId

getMessageAction
  :: IsInterfaceSide s i
  => Object s m i
  -> Opcode
  -> Get (ProtocolAction s m ())
getMessageAction object@(Object _ objectHandler) opcode = do
  message <- getDown object opcode
  pure $ handleMessage objectHandler object message

type RawMessage = (ObjectId, Opcode, BSL.ByteString)

receiveRawMessage :: forall s m. MonadCatch m => ProtocolAction s m (Maybe RawMessage)
receiveRawMessage = do
  st <- State.get
  (result, newDecoder) <- checkDecoder st.inboxDecoder
  State.put st{inboxDecoder = newDecoder}
  pure result
  where
    checkDecoder
      :: Decoder RawMessage
      -> ProtocolAction s m (Maybe RawMessage, Decoder RawMessage)
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


sendRawMessage :: MonadCatch m => Put -> ProtocolAction s m ()
sendRawMessage x = State.modify \st -> st {
  outbox = Just (maybe x (<> x) st.outbox)
}
