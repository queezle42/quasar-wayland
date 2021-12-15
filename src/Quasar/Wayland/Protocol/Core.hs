{-# LANGUAGE DeriveLift #-}

module Quasar.Wayland.Protocol.Core (
  ObjectId,
  GenericObjectId,
  NewId,
  GenericNewId,
  Opcode,
  Fixed(..),
  WlString(..),
  toString,
  fromString,
  IsSide(..),
  Side(..),
  IsInterface(..),
  interfaceName,
  IsInterfaceSide(..),
  IsInterfaceHandler(..),
  Object(objectId),
  setEventHandler,
  setRequestHandler,
  setMessageHandler,
  getMessageHandler,
  NewObject,
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
  enterObject,

  -- * Low-level protocol interaction
  sendMessage,
  newObject,
  newObjectFromId,
  getObject,

  -- * Protocol exceptions
  WireCallbackFailed(..),
  ParserFailed(..),
  ProtocolException(..),
  MaximumIdReached(..),
  ServerError(..),

  -- * Message decoder operations
  WireFormat(..),
  dropRemaining,
  invalidOpcode,
) where

import Control.Concurrent.STM
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
import GHC.Conc (unsafeIOToSTM)
import GHC.TypeLits
import Quasar.Prelude


newtype ObjectId (j :: Symbol) = ObjectId Word32
  deriving newtype (Eq, Show, Hashable)

newtype GenericObjectId = GenericObjectId Word32
  deriving newtype (Eq, Show, Hashable)

toGenericObjectId :: ObjectId j -> GenericObjectId
toGenericObjectId (ObjectId oId) = GenericObjectId oId

type Opcode = Word16


newtype NewId (j :: Symbol) = NewId (ObjectId j)
  deriving newtype (Eq, Show)

newtype GenericNewId = GenericNewId GenericObjectId
  deriving newtype (Eq, Show)


-- | Signed 24.8 decimal numbers.
newtype Fixed = Fixed Word32
  deriving newtype Eq

instance Show Fixed where
  show x = "[fixed " <> show x <> "]"


-- | A string. The encoding is not officially specified, but in practice UTF-8 is used.
--
-- Instances and functions in this library assume UTF-8, but the original data is also available by deconstructing.
newtype WlString = WlString BS.ByteString
  deriving newtype (Eq, Hashable)

instance Show WlString where
  show = show . toString

instance IsString WlString where
  fromString = WlString . BSUTF8.fromString

toString :: WlString -> String
toString (WlString bs) = BSUTF8.toString bs


dropRemaining :: Get ()
dropRemaining = void getRemainingLazyByteString


class (Eq a, Show a) => WireFormat a where
  putArgument :: a -> ProtocolM s (Put, Int)
  getArgument :: Get (ProtocolM s a)
  showArgument :: a -> String

instance WireFormat Int32 where
  putArgument x = pure (putInt32host x, 4)
  getArgument = pure <$> getInt32host
  showArgument = show

instance WireFormat Word32 where
  putArgument x = pure (putWord32host x, 4)
  getArgument = pure <$> getWord32host
  showArgument = show

instance WireFormat Fixed where
  putArgument (Fixed repr) = pure (putWord32host repr, 4)
  getArgument = pure . Fixed <$> getWord32host
  showArgument = show

instance WireFormat WlString where
  putArgument (WlString x) = putWaylandBlob x
  getArgument = pure . WlString <$> getWaylandBlob
  showArgument = show

instance WireFormat BS.ByteString where
  putArgument x = putWaylandBlob x
  getArgument = pure <$> getWaylandBlob
  showArgument array = "[array " <> show (BS.length array) <> "B]"

instance KnownSymbol j => WireFormat (ObjectId (j :: Symbol)) where
  putArgument (ObjectId oId) = pure (putWord32host oId, 4)
  getArgument = pure . ObjectId <$> getWord32host
  showArgument (ObjectId oId) = symbolVal @j Proxy <> "@" <> show oId

instance WireFormat GenericObjectId where
  putArgument (GenericObjectId oId) = pure (putWord32host oId, 4)
  getArgument = pure . GenericObjectId <$> getWord32host
  showArgument oId = "[unknown]@" <> show oId

instance KnownSymbol j => WireFormat (NewId (j :: Symbol)) where
  putArgument (NewId newId) = putArgument newId
  getArgument = NewId <<$>> getArgument
  showArgument (NewId newId) = "new " <> symbolVal @j Proxy <> "@" <> show newId

instance WireFormat GenericNewId where
  putArgument (GenericNewId newId) = putArgument newId
  getArgument = GenericNewId <<$>> getArgument
  showArgument newId = "new [unknown]@" <> show newId

instance WireFormat Void where
  putArgument = absurd
  getArgument = pure <$> get
  showArgument = absurd


-- | Class for a proxy type (in the haskell sense) that describes a Wayland interface.
class (
    IsMessage (WireRequest i),
    IsMessage (WireEvent i),
    KnownSymbol (InterfaceName i)
  )
  => IsInterface i where
  type RequestHandler i
  type EventHandler i
  type WireRequest i
  type WireEvent i
  type InterfaceName i :: Symbol

interfaceName :: forall i. IsInterface i => String
interfaceName = symbolVal @(InterfaceName i) Proxy

class IsSide (s :: Side) where
  type MessageHandler s i
  type WireUp s i
  type WireDown s i
  initialId :: Word32
  maximumId :: Word32

instance IsSide 'Client where
  type MessageHandler 'Client i = EventHandler i
  type WireUp 'Client i = WireRequest i
  type WireDown 'Client i = WireEvent i
  -- Id #1 is reserved for wl_display
  initialId = 2
  maximumId = 0xfeffffff

instance IsSide 'Server where
  type MessageHandler 'Server i = RequestHandler i
  type WireUp 'Server i = WireEvent i
  type WireDown 'Server i = WireRequest i
  initialId = 0xff000000
  maximumId = 0xffffffff


class (
    IsSide s,
    IsInterface i,
    IsMessage (WireUp s i),
    IsMessage (WireDown s i)
  )
  => IsInterfaceSide (s :: Side) i where
  handleMessage :: MessageHandler s i -> WireDown s i -> ProtocolM s ()


getWireDown :: forall s i. IsInterfaceSide s i => Object s i -> Opcode -> Get (ProtocolM s (WireDown s i))
getWireDown = getMessage @(WireDown s i)

putWireUp :: forall s i. IsInterfaceSide s i => Object s i -> WireUp s i -> ProtocolM s (Opcode, [(Put, Int)])
putWireUp _ = putMessage @(WireUp s i)


class IsInterfaceSide s i => IsInterfaceHandler s i a where
  handlerHandleMessage :: a -> Object s i -> WireDown s i -> ProtocolM s ()


-- | Data kind
data Side = Client | Server


-- | An object belonging to a wayland connection.
data Object s i = IsInterfaceSide s i => Object {
  objectProtocol :: (ProtocolHandle s),
  objectId :: ObjectId (InterfaceName i),
  messageHandler :: TVar (Maybe (MessageHandler s i))
}

getMessageHandler :: Object s i -> STM (MessageHandler s i)
getMessageHandler object = maybe retry pure =<< readTVar object.messageHandler

setMessageHandler :: Object s i -> MessageHandler s i -> STM ()
setMessageHandler object = writeTVar object.messageHandler . Just

setRequestHandler :: Object 'Server i -> RequestHandler i -> STM ()
setRequestHandler = setMessageHandler

setEventHandler :: Object 'Client i -> EventHandler i -> STM ()
setEventHandler = setMessageHandler

-- | Type alias to indicate an object is created with a message.
type NewObject s i = Object s i

instance IsInterface i => Show (Object s i) where
  show = showObject

class IsObject a where
  genericObjectId :: a -> GenericObjectId
  objectInterfaceName :: a -> String
  showObject :: a -> String
  showObject object = objectInterfaceName object <> "@" <> show (genericObjectId object)

class IsObjectSide a where
  describeUpMessage :: a -> Opcode -> BSL.ByteString -> String
  describeDownMessage :: a -> Opcode -> BSL.ByteString -> String

instance forall s i. IsInterface i => IsObject (Object s i) where
  genericObjectId object = toGenericObjectId object.objectId
  objectInterfaceName _ = interfaceName @i

instance forall s i. IsInterfaceSide s i => IsObjectSide (Object s i) where
  describeUpMessage object opcode body =
    objectInterfaceName object <> "@" <> show (genericObjectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(WireUp s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"
  describeDownMessage object opcode body =
    objectInterfaceName object <> "@" <> show (genericObjectId object) <>
    "." <> fromMaybe "[invalidOpcode]" (opcodeName @(WireDown s i) opcode) <>
    " (" <> show (BSL.length body) <> "B)"

-- | Wayland object quantification wrapper
data SomeObject s
  = forall i. IsInterfaceSide s i => SomeObject (Object s i)
  | UnknownObject String GenericObjectId

instance IsObject (SomeObject s) where
  genericObjectId (SomeObject object) = genericObjectId object
  genericObjectId (UnknownObject _ oId) = oId
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
  getMessage :: IsInterface i => Object s i -> Opcode -> Get (ProtocolM s a)
  putMessage :: a -> ProtocolM s (Opcode, [(Put, Int)])

instance IsMessage Void where
  opcodeName _ = Nothing
  getMessage = invalidOpcode
  putMessage = absurd

invalidOpcode :: IsInterface i => Object s i -> Opcode -> Get a
invalidOpcode object opcode =
  fail $ "Invalid opcode " <> show opcode <> " on " <> objectInterfaceName object <> "@" <> show (genericObjectId object)

showObjectMessage :: (IsObject a, IsMessage b) => a -> b -> String
showObjectMessage object message =
  showObject object <> "." <> show message


-- * Exceptions

data WireCallbackFailed = WireCallbackFailed SomeException
  deriving stock Show
  deriving anyclass Exception

data ParserFailed = ParserFailed String String
  deriving stock Show
  deriving anyclass Exception

data ProtocolException = ProtocolException String
  deriving stock Show
  deriving anyclass Exception

data ProtocolUsageError = ProtocolUsageError String
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
  protocolKey :: Unique,
  protocolHandle :: ProtocolHandle s,
  bytesReceivedVar :: TVar Int64,
  bytesSentVar :: TVar Int64,
  inboxDecoderVar :: TVar (Decoder RawMessage),
  outboxVar :: TVar (Maybe Put),
  objectsVar :: TVar (HashMap GenericObjectId (SomeObject s)),
  nextIdVar :: TVar Word32
}

type ProtocolM s a = ReaderT (ProtocolState s) STM a

askProtocol :: ProtocolM s (ProtocolHandle s)
askProtocol = (.protocolHandle) <$> ask

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

modifyProtocolVar' :: (ProtocolState s -> TVar a) -> (a -> a) -> ProtocolM s ()
modifyProtocolVar' fn x = do
  state <- ask
  lift $ modifyTVar' (fn state) x

stateProtocolVar :: (ProtocolState s -> TVar a) -> (a -> (r, a)) -> ProtocolM s r
stateProtocolVar fn x = do
  state <- ask
  lift $ stateTVar (fn state) x

initializeProtocol
  :: forall s wl_display a. (IsInterfaceSide s wl_display)
  => MessageHandler s wl_display
  -> (Object s wl_display -> STM a)
  -> STM (a, ProtocolHandle s)
initializeProtocol wlDisplayMessageHandler initializationAction = do
  bytesReceivedVar <- newTVar 0
  bytesSentVar <- newTVar 0
  inboxDecoderVar <- newTVar $ runGetIncremental getRawMessage
  outboxVar <- newTVar Nothing
  protocolKey <- unsafeIOToSTM newUnique
  objectsVar <- newTVar $ HM.empty
  nextIdVar <- newTVar (initialId @s)

  -- Create uninitialized to avoid use of a diverging 'mfix'
  stateVar <- newTVar (Left unreachableCodePath)

  let protocol = ProtocolHandle {
    stateVar
  }

  let state = ProtocolState {
    protocolHandle = protocol,
    protocolKey,
    bytesReceivedVar,
    bytesSentVar,
    inboxDecoderVar,
    outboxVar,
    objectsVar,
    nextIdVar
  }
  writeTVar stateVar (Right state)

  messageHandlerVar <- newTVar (Just wlDisplayMessageHandler)
  let wlDisplay = Object protocol wlDisplayId messageHandlerVar
  modifyTVar' objectsVar (HM.insert (toGenericObjectId wlDisplayId) (SomeObject wlDisplay))

  result <- initializationAction wlDisplay
  pure (result, protocol)
  where
    wlDisplayId :: ObjectId (InterfaceName wl_display)
    wlDisplayId = ObjectId 1

-- | Run a protocol action in 'IO'. If an exception occurs, it is stored as a protocol failure and is then
-- re-thrown.
--
-- Throws an exception, if the protocol is already in a failed state.
runProtocolTransaction :: MonadIO m => ProtocolHandle s -> ProtocolM s a -> m a
runProtocolTransaction ProtocolHandle{stateVar} action = do
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
-- Throws an exception, if the protocol is already in a failed state.
--
-- Exceptions are not handled (i.e. they usually reset the STM transaction and are not stored as a protocol failure).
runProtocolM :: ProtocolHandle s -> ProtocolM s a -> STM a
runProtocolM protocol action = either throwM (runReaderT action) =<< readTVar protocol.stateVar


-- | Feed the protocol newly received data.
feedInput :: (IsSide s, MonadIO m) => ProtocolHandle s -> ByteString -> m ()
feedInput protocol bytes = runProtocolTransaction protocol do
  -- Exposing MonadIO instead of STM to the outside and using `runProtocolTransaction` here enforces correct exception
  -- handling.
  modifyProtocolVar' (.bytesReceivedVar) (+ fromIntegral (BS.length bytes))
  modifyProtocolVar (.inboxDecoderVar) (`pushChunk` bytes)
  receiveMessages

-- | Set the protocol to a failed state, e.g. when the socket closed unexpectedly.
setException :: (Exception e, MonadIO m) => ProtocolHandle s -> e -> m ()
setException protocol ex = runProtocolTransaction protocol $ throwM ex

-- | Take data that has to be sent. Blocks until data is available.
takeOutbox :: MonadIO m => ProtocolHandle s -> m (BSL.ByteString)
takeOutbox protocol = runProtocolTransaction protocol do
  mOutboxData <- stateProtocolVar (.outboxVar) (\mOutboxData -> (mOutboxData, Nothing))
  outboxData <- maybe (lift retry) pure mOutboxData
  let sendData = runPut outboxData
  modifyProtocolVar' (.bytesSentVar) (+ BSL.length sendData)
  pure sendData


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once; in the same STM
-- transaction; before using the object).
--
-- Exported for use in TH generated code.
newObject
  :: forall s i. IsInterfaceSide s i
  => Maybe (MessageHandler s i)
  -> ProtocolM s (Object s i, NewId (InterfaceName i))
newObject messageHandler = do
  oId <- allocateObjectId
  let newId = NewId @(InterfaceName i) oId
  object <- newObjectFromId messageHandler newId
  pure (object, newId)
  where
    allocateObjectId :: ProtocolM s (ObjectId (InterfaceName i))
    allocateObjectId = do
      id' <- readProtocolVar (.nextIdVar)

      let nextId' = id' + 1
      when (nextId' == maximumId @s) $ throwM MaximumIdReached

      writeProtocolVar (.nextIdVar) nextId'
      pure $ ObjectId id'


-- | Create an object from a received id. The caller is responsible for using a 'NewId' exactly once while handling an
-- incoming message
--
-- Exported for use in TH generated code.
newObjectFromId
  :: forall s i. IsInterfaceSide s i
  => Maybe (MessageHandler s i)
  -> NewId (InterfaceName i)
  -> ProtocolM s (Object s i)
newObjectFromId messageHandler (NewId oId) = do
  protocol <- askProtocol
  messageHandlerVar <- lift $ newTVar messageHandler
  let
    object = Object protocol oId messageHandlerVar
    someObject = SomeObject object
  modifyProtocolVar (.objectsVar) (HM.insert (genericObjectId object) someObject)
  pure object


getObject
  :: IsInterfaceSide s i
  => ObjectId (InterfaceName i)
  -> ProtocolM s (Object s i)
getObject = undefined



-- | Sends a message without checking any ids or creating proxy objects objects. (TODO)
sendMessage :: forall s i. IsInterfaceSide s i => Object s i -> WireUp s i -> ProtocolM s ()
sendMessage object message = do
  isActiveObject <- HM.member oId <$> readProtocolVar (.objectsVar)
  unless isActiveObject $ throwM $ ProtocolUsageError $ "Tried to send message on an invalid object: " <> show object

  (opcode, pairs) <- putWireUp object message
  let (putBodyParts, partLengths) = unzip pairs
  let putBody = mconcat putBodyParts

  let bodyLength = foldr (+) 8 partLengths
  when (bodyLength > fromIntegral (maxBound :: Word16)) $
    throwM $ ProtocolUsageError $ "Tried to send message larger than 2^16 bytes"

  traceM $ "-> " <> showObjectMessage object message
  sendRawMessage $ putHeader opcode bodyLength >> putBody
  where
    oId = genericObjectId object
    (GenericObjectId objectIdWord) = genericObjectId object
    putHeader :: Opcode -> Int -> Put
    putHeader opcode msgSize = do
      putWord32host objectIdWord
      putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode

enterObject :: forall s i a. Object s i -> ProtocolM s a -> STM a
enterObject object action = runProtocolM object.objectProtocol action


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
      case runGetOrFail (getMessageAction object) body of
        Left (_, _, message) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) message
        Right ("", _, result) -> result
        Right (leftovers, _, _) ->
          throwM $ ParserFailed (describeDownMessage object opcode body) (show (BSL.length leftovers) <> "B not parsed")

    Just (UnknownObject interface _) -> do
      throwM $ ProtocolException $ "Received message for object without handler: " <> interface <> "@" <> show oId
  where
    getMessageAction
      :: forall i. IsInterfaceSide s i
      => Object s i
      -> Get (ProtocolM s ())
    getMessageAction object = do
      verifyMessage <- getWireDown object opcode
      pure do
        message <- verifyMessage
        traceM $ "<- " <> showObjectMessage object message
        messageHandler <- lift $ getMessageHandler object
        handleMessage @s @i messageHandler message

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
  oId <- GenericObjectId <$> getWord32host
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

putWaylandBlob :: BS.ByteString -> ProtocolM s (Put, Int)
putWaylandBlob blob = do
  when (len > fromIntegral (maxBound :: Word16)) $
    throwM $ ProtocolUsageError $ "Tried to send string or array larger than 2^16 bytes"

  pure (putBlob, 4 + len + pad)
  where
    -- Total data length including null byte
    len = BS.length blob + 1
    -- Padding length
    pad = padding len
    putBlob = do
      putWord32host (fromIntegral (len + 1))
      putByteString blob
      putWord8 0
      replicateM_ pad (putWord8 0)


skipPadding :: Get ()
skipPadding = do
  bytes <- bytesRead
  skip $ fromIntegral (padding bytes)

padding :: Integral a => a -> a
padding size = ((4 - (size `mod` 4)) `mod` 4)


sendRawMessage :: Put -> ProtocolM s ()
sendRawMessage x = modifyProtocolVar (.outboxVar) (Just . maybe x (<> x))
