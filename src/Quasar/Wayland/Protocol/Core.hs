{-# LANGUAGE DeriveLift #-}

module Quasar.Wayland.Protocol.Core (
  ObjectId,
  GenericObjectId,
  NewId,
  Opcode,
  ArgumentType(..),
  Fixed(..),
  WlString(..),
  toString,
  fromString,
  IsSide(..),
  Side(..),
  IsInterface(..),
  IsInterfaceSide,
  IsInterfaceHandler(..),
  Object,
  IsObject,
  IsMessage(..),
  ProtocolHandle,
  ProtocolM,

  -- * Protocol IO
  initializeProtocol,
  feedInput,
  setException,
  takeOutbox,
  runProtocolTransaction,
  runProtocolM,

  -- * Low-level protocol interaction
  sendMessage,
  newObject,

  -- ** Callbacks
  Callback(..),
  internalFnCallback,
  traceCallback,
  ignoreMessage,

  -- * Protocol exceptions
  CallbackFailed(..),
  ParserFailed(..),
  ProtocolException(..),
  MaximumIdReached(..),
  ServerError(..),

  -- * TH utilities
  isNewId,

  -- * Message decoder operations
  WireFormat(..),
  dropRemaining,
  invalidOpcode,
) where

import Control.Concurrent.STM
import Control.Monad (replicateM_)
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as BSUTF8
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Proxy
import Data.String (IsString(..))
import Data.Void (absurd)
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift)
import Quasar.Prelude


newtype ObjectId (j :: Symbol) = ObjectId GenericObjectId
  deriving stock (Eq, Show)

type GenericObjectId = Word32

type Opcode = Word16


newtype NewId (j :: Symbol) = NewId GenericObjectId
  deriving stock (Eq, Show)

newtype GenericNewId = GenericNewId GenericObjectId
  deriving stock (Eq, Show)


-- | Signed 24.8 decimal numbers.
newtype Fixed = Fixed Word32
  deriving newtype Eq

instance Show Fixed where
  show x = "[fixed " <> show x <> "]"


-- | A string. The encoding is not officially specified, but in practice UTF-8 is used.
--
-- Instances and functions in this library assume UTF-8, but the original data is also available by deconstructing.
newtype WlString = WlString BS.ByteString

instance Show WlString where
  show = show . toString

instance IsString WlString where
  fromString = WlString . BSUTF8.fromString

toString :: WlString -> String
toString (WlString bs) = BSUTF8.toString bs


dropRemaining :: Get ()
dropRemaining = void getRemainingLazyByteString


data ArgumentType
  = IntArgument
  | UIntArgument
  | FixedArgument
  | StringArgument
  | ArrayArgument
  | ObjectArgument String
  | GenericObjectArgument
  | NewIdArgument String
  | GenericNewIdArgument
  | FdArgument
  deriving stock (Eq, Show, Lift)

isNewId :: ArgumentType -> Bool
isNewId (NewIdArgument _) = True
isNewId GenericNewIdArgument = True
isNewId _ = False

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

instance KnownSymbol j => WireFormat (ObjectId (j :: Symbol)) where
  type Argument (ObjectId j) = ObjectId j
  putArgument (ObjectId oId) = putWord32host oId
  getArgument = ObjectId <$> getWord32host
  showArgument (ObjectId oId) = symbolVal @j Proxy <> "@" <> show oId

instance WireFormat 'GenericObjectArgument where
  type Argument 'GenericObjectArgument = GenericObjectId
  putArgument = putWord32host
  getArgument = getWord32host
  showArgument oId = "[unknown]@" <> show oId

instance KnownSymbol j => WireFormat (NewId (j :: Symbol)) where
  type Argument (NewId j) = NewId j
  putArgument (NewId newId) = putWord32host newId
  getArgument = NewId <$> getWord32host
  showArgument (NewId newId) = "new " <> symbolVal @j Proxy <> "@" <> show newId

instance WireFormat 'GenericNewIdArgument where
  type Argument 'GenericNewIdArgument = GenericNewId
  putArgument (GenericNewId newId) = putWord32host newId
  getArgument = GenericNewId <$> getWord32host
  showArgument newId = "new [unknown]@" <> show newId

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
  type InterfaceName i :: Symbol
  interfaceName :: String

class IsSide (s :: Side) where
  type Up s i
  type Down s i
  initialId :: GenericObjectId
  maximumId :: GenericObjectId

instance IsSide 'Client where
  type Up 'Client i = Request i
  type Down 'Client i = Event i
  -- Id #1 is reserved for wl_display
  initialId = 2
  maximumId = 0xfeffffff

instance IsSide 'Server where
  type Up 'Server i = Event i
  type Down 'Server i = Request i
  initialId = 0xff000000
  maximumId = 0xffffffff


--- | Empty class, used to combine constraints
class (
    IsSide s,
    IsInterface i,
    IsMessage (Up s i),
    IsMessage (Down s i)
  )
  => IsInterfaceSide (s :: Side) i


getDown :: forall s i. IsInterfaceSide s i => Object s i -> Opcode -> Get (Down s i)
getDown = getMessage @(Down s i)

putUp :: forall s i. IsInterfaceSide s i => Object s i -> Up s i -> PutM Opcode
putUp _ = putMessage @(Up s i)


class IsInterfaceSide s i => IsInterfaceHandler s i a where
  handleMessage :: a -> Object s i -> Down s i -> ProtocolM s ()


-- | Data kind
data Side = Client | Server

data Object s i = IsInterfaceSide s i => Object GenericObjectId (Callback s i)

class IsObject a where
  objectId :: a -> GenericObjectId
  objectInterfaceName :: a -> String

class IsObjectSide a where
  describeUpMessage :: a -> Opcode -> BSL.ByteString -> String
  describeDownMessage :: a -> Opcode -> BSL.ByteString -> String

instance forall s i. IsInterface i => IsObject (Object s i) where
  objectId (Object oId _) = oId
  objectInterfaceName _ = interfaceName @i

instance forall s i. IsInterfaceSide s i => IsObjectSide (Object s i) where
  describeUpMessage object opcode body =
    objectInterfaceName object <> "@" <> show (objectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(Up s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"
  describeDownMessage object opcode body =
    objectInterfaceName object <> "@" <> show (objectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(Down s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"

-- | Wayland object quantification wrapper
data SomeObject s
  = forall i. IsInterfaceSide s i => SomeObject (Object s i)
  | UnknownObject String GenericObjectId

instance IsObject (SomeObject s) where
  objectId (SomeObject object) = objectId object
  objectId (UnknownObject _ oId) = oId
  objectInterfaceName (SomeObject object) = objectInterfaceName object
  objectInterfaceName (UnknownObject interface _) = interface

instance IsObjectSide (SomeObject s) where
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
  getMessage :: IsInterface i => Object s i -> Opcode -> Get a
  putMessage :: a -> PutM Opcode

instance IsMessage Void where
  opcodeName _ = Nothing
  getMessage = invalidOpcode
  putMessage = absurd

invalidOpcode :: IsInterface i => Object s i -> Opcode -> Get a
invalidOpcode object opcode =
  fail $ "Invalid opcode " <> show opcode <> " on " <> objectInterfaceName object <> "@" <> show (objectId object)

showObjectMessage :: (IsObject a, IsMessage b) => a -> b -> String
showObjectMessage object message =
  objectInterfaceName object <> "@" <> show (objectId object) <> "." <> show message


data Callback s i = forall a. IsInterfaceHandler s i a => Callback a

instance IsInterfaceSide s i => IsInterfaceHandler s i (Callback s i) where
  handleMessage (Callback callback) = handleMessage callback


data LowLevelCallback s i = IsInterfaceSide s i => FnCallback (Object s i -> Down s i -> ProtocolM s ())

instance IsInterfaceSide s i => IsInterfaceHandler s i (LowLevelCallback s i) where
  handleMessage (FnCallback fn) object msg = fn object msg

internalFnCallback :: IsInterfaceSide s i => (Object s i -> Down s i -> ProtocolM s ()) -> Callback s i
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
traceCallback :: IsInterfaceSide 'Client i => Callback 'Client i -> Callback 'Client i
traceCallback next = internalFnCallback \object message -> do
  traceM $ "<- " <> showObjectMessage object message
  handleMessage next object message

-- | A `Callback` that ignores all messages. Intended for development purposes, e.g. together with `traceCallback`.
ignoreMessage :: IsInterfaceSide 'Client i => Callback 'Client i
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

data MaximumIdReached = MaximumIdReached
  deriving stock Show
  deriving anyclass Exception

data ServerError = ServerError Word32 String
  deriving stock Show
  deriving anyclass Exception

-- * Protocol state and monad plumbing

-- | Top-level protocol handle (used e.g. to send/receive data)
newtype ProtocolHandle (s :: Side) = ProtocolHandle {
  stateVar :: TVar (Either SomeException (ProtocolState s))
}

-- | Protocol state handle, containing state for a non-failed protocol (should be kept in a 'ProtocolStateVar')
data ProtocolState (s :: Side) = ProtocolState {
  bytesReceivedVar :: TVar Int64,
  bytesSentVar :: TVar Int64,
  inboxDecoderVar :: TVar (Decoder RawMessage),
  outboxVar :: TVar (Maybe Put),
  objectsVar :: TVar (HashMap GenericObjectId (SomeObject s)),
  nextIdVar :: TVar Word32
}

type ProtocolM s a = ReaderT (ProtocolState s) STM a

readProtocolVar :: (ProtocolState s -> TVar a) -> ProtocolM s a
readProtocolVar fn = do
  state <- ask
  lift $ readTVar (fn state)

writeProtocolVar :: (ProtocolState s -> TVar a) -> a -> ProtocolM s ()
writeProtocolVar fn x = do
  state <- ask
  lift $ writeTVar (fn state) x

modifyProtocolVar :: (ProtocolState s -> TVar a) -> (a -> a) -> ProtocolM s ()
modifyProtocolVar fn x = do
  state <- ask
  lift $ modifyTVar (fn state) x

stateProtocolVar :: (ProtocolState s -> TVar a) -> (a -> (r, a)) -> ProtocolM s r
stateProtocolVar fn x = do
  state <- ask
  lift $ stateTVar (fn state) x

initializeProtocol
  :: forall s wl_display a. (IsInterfaceSide s wl_display)
  => Callback s wl_display
  -> (Object s wl_display -> ProtocolM s a)
  -> STM (a, ProtocolHandle s)
initializeProtocol wlDisplayCallback initializationAction = do
  bytesReceivedVar <- newTVar 0
  bytesSentVar <- newTVar 0
  inboxDecoderVar <- newTVar $ runGetIncremental getRawMessage
  outboxVar <- newTVar Nothing
  objectsVar <- newTVar $ HM.fromList [(1, (SomeObject wlDisplay))]
  nextIdVar <- newTVar (initialId @s)
  let state = ProtocolState {
    bytesReceivedVar,
    bytesSentVar,
    inboxDecoderVar,
    outboxVar,
    objectsVar,
    nextIdVar
  }
  stateVar <- newTVar (Right state)
  let protocol = ProtocolHandle {
    stateVar
  }
  result <- runReaderT (initializationAction wlDisplay) state
  pure (result, protocol)
  where
    wlDisplay :: Object s wl_display
    wlDisplay = Object 1 wlDisplayCallback

-- | Run a protocol action in 'IO'. If an exception occurs, it is stored as a protocol failure and is then
-- re-thrown.
--
-- Throws an exception, if the protocol is already in a failed state.
runProtocolTransaction :: MonadIO m => ProtocolHandle s -> ProtocolM s a -> m a
runProtocolTransaction (ProtocolHandle stateVar) action = do
  result <- liftIO $ atomically do
    readTVar stateVar >>= \case
      -- Protocol is already in a failed state
      Left ex -> throwM ex
      Right state -> do
        -- Run action, catch exceptions
        runReaderT (try action) state >>= \case
          Left ex -> do
            -- Action failed, change protocol state to failed
            writeTVar stateVar (Left ex)
            pure (Left ex)
          Right result -> do
            pure (Right result)
  -- Transaction is committed, rethrow exception if the action failed
  either (liftIO . throwM) pure result


-- | Run a 'ProtocolM'-action inside 'STM'.
--
-- Exceptions are not handled and reset the transaction (as usual with STM).
--
-- Throws an exception, if the protocol is already in a failed state.
runProtocolM :: ProtocolHandle s -> ProtocolM s a -> STM a
runProtocolM protocol action = either throwM (runReaderT action) =<< readTVar protocol.stateVar


-- | Feed the protocol newly received data.
feedInput :: (IsSide s, MonadIO m, MonadThrow m) => ProtocolHandle s -> ByteString -> m ()
feedInput protocol bytes = runProtocolTransaction protocol do
  -- Exposing MonadIO instead of STM to the outside and using `runProtocolTransaction` here enforces correct exception handling.
  modifyProtocolVar (.bytesReceivedVar) (+ fromIntegral (BS.length bytes))
  modifyProtocolVar (.inboxDecoderVar) (`pushChunk` bytes)
  receiveMessages

-- | Set the protocol to a failed state, e.g. when the socket closed unexpectedly.
setException :: (Exception e, MonadIO m, MonadThrow m) => ProtocolHandle s -> e -> m ()
setException protocol ex = runProtocolTransaction protocol $ throwM ex

-- | Take data that has to be sent. Blocks until data is available.
takeOutbox :: (MonadIO m, MonadThrow m) => ProtocolHandle s -> m (BSL.ByteString)
takeOutbox protocol = runProtocolTransaction protocol do
  mOutboxData <- stateProtocolVar (.outboxVar) (\mOutboxData -> (mOutboxData, Nothing))
  outboxData <- maybe (lift retry) pure mOutboxData
  let sendData = runPut outboxData
  modifyProtocolVar (.bytesSentVar) (+ BSL.length sendData)
  pure sendData


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once; in the same STM
-- transaction; before using the object).
newObject
  :: forall s i. IsInterfaceSide s i
  => Callback s i
  -> ProtocolM s (Object s i, NewId (InterfaceName i))
newObject callback = do
  genOId <- allocateObjectId
  let oId = NewId @(InterfaceName i) genOId
  object <- newObjectFromId oId callback
  pure (object, oId)
  where
    allocateObjectId :: ProtocolM s GenericObjectId
    allocateObjectId = do
      id' <- readProtocolVar (.nextIdVar)

      let nextId' = id' + 1
      when (nextId' == maximumId @s) $ throwM MaximumIdReached

      writeProtocolVar (.nextIdVar) nextId'
      pure id'

newObjectFromId
  :: forall s i. IsInterfaceSide s i
  => NewId (InterfaceName i)
  -> Callback s i
  -> ProtocolM s (Object s i)
newObjectFromId (NewId oId) callback = do
  let
    object = Object oId callback
    someObject = SomeObject object
  modifyProtocolVar (.objectsVar) (HM.insert oId someObject)
  pure object


-- | Sends a message without checking any ids or creating proxy objects objects. (TODO)
sendMessage :: forall s i. IsInterfaceSide s i => Object s i -> Up s i -> ProtocolM s ()
sendMessage object message = do
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
    msgSize =
      if msgSizeInteger <= fromIntegral (maxBound :: Word16)
        then fromIntegral msgSizeInteger
        else error "Message too large"
    -- TODO: body length should be returned from `putMessage`, instead of realizing it to a ByteString here
    msgSizeInteger :: Integer
    msgSizeInteger = 8 + fromIntegral (BSL.length body)


receiveMessages :: IsSide s => ProtocolM s ()
receiveMessages = receiveRawMessage >>= \case
  Nothing -> pure ()
  Just rawMessage -> do
    handleRawMessage rawMessage
    receiveMessages

handleRawMessage :: forall s. RawMessage -> ProtocolM s ()
handleRawMessage (oId, opcode, body) = do
  objects <- readProtocolVar (.objectsVar)
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
  => Object s i
  -> Opcode
  -> Get (ProtocolM s ())
getMessageAction object@(Object _ objectHandler) opcode = do
  message <- getDown object opcode
  pure $ handleMessage objectHandler object message

type RawMessage = (GenericObjectId, Opcode, BSL.ByteString)

receiveRawMessage :: forall s. ProtocolM s (Maybe RawMessage)
receiveRawMessage = do
  (result, nextDecoder) <- checkDecoder =<< readProtocolVar (.inboxDecoderVar)
  writeProtocolVar (.inboxDecoderVar) nextDecoder
  pure result
  where
    checkDecoder
      :: Decoder RawMessage
      -> ProtocolM s (Maybe RawMessage, Decoder RawMessage)
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
  putWord32host (fromIntegral (size + 1))
  putByteString blob
  putWord8 0
  replicateM_ (padding size) (putWord8 0)


skipPadding :: Get ()
skipPadding = do
  bytes <- bytesRead
  skip $ fromIntegral (padding bytes)

paddedSize :: Integral a => a -> a
paddedSize size = size + padding size

padding :: Integral a => a -> a
padding size = ((4 - (size `mod` 4)) `mod` 4)


sendRawMessage :: Put -> ProtocolM s ()
sendRawMessage x = modifyProtocolVar (.outboxVar) (Just . maybe x (<> x))
