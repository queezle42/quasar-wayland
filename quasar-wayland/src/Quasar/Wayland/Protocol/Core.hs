{-# LANGUAGE DeriveLift #-}

module Quasar.Wayland.Protocol.Core (
  ObjectId,
  GenericObjectId,
  NewId,
  GenericNewId,
  Opcode,
  WlFixed(..),
  fixedToDouble,
  doubleToFixed,
  WlString(..),
  toString,
  IsSide(..),
  Side(..),
  IsInterface(..),
  interfaceName,
  Version,
  interfaceVersion,
  IsInterfaceSide(..),
  Object(objectProtocol),
  setEventHandler,
  setRequestHandler,
  setMessageHandler,
  getMessageHandler,
  setInterfaceData,
  getInterfaceData,
  isObjectDestroyed,
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
  objectWireArgument,
  nullableObjectWireArgument,
  checkObject,
  sendMessage,
  newObject,
  newObjectFromId,
  bindNewObject,
  bindObjectFromId,
  getObject,
  getNullableObject,
  lookupObject,
  buildMessage,

  -- * wl_display interface
  handleWlDisplayError,
  handleWlDisplayDeleteId,

  -- * Protocol exceptions
  WireCallbackFailed(..),
  ParserFailed(..),
  ProtocolException(..),
  ProtocolUsageError(..),
  MaximumIdReached(..),
  ServerError(..),
  InternalError(..),

  -- * Message decoder operations
  WireFormat(..),
  invalidOpcode,
) where

import Control.Monad.Catch
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks, lift)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.UTF8 qualified as BSUTF8
import Data.Dynamic (Dynamic, toDyn, fromDynamic)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Proxy
import Data.Sequence (Seq(Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.String (IsString(..))
import Data.Typeable (Typeable, cast)
import Data.Void (absurd)
import GHC.TypeLits
import GHC.Records
import Quasar.Prelude
import System.Posix.Types (Fd(Fd))


newtype ObjectId (j :: Symbol) = ObjectId Word32
  deriving newtype (Eq, Show, Hashable)

newtype GenericObjectId = GenericObjectId Word32
  deriving newtype (Eq, Show, Hashable)

toGenericObjectId :: ObjectId j -> GenericObjectId
toGenericObjectId (ObjectId oId) = GenericObjectId oId

objectIdValue :: ObjectId j -> Word32
objectIdValue (ObjectId value) = value

type Opcode = Word16

type Version = Word32


newtype NewId (j :: Symbol) = NewId (ObjectId j)
  deriving newtype (Eq, Show)

data GenericNewId = GenericNewId WlString Version Word32
  deriving stock (Eq, Show)


-- | Signed 24.8 decimal numbers.
newtype WlFixed = WlFixed Int32
  deriving newtype Eq

instance Show WlFixed where
  show (WlFixed x) = "[fixed " <> show x <> "]"

fixedToDouble :: WlFixed -> Double
fixedToDouble (WlFixed f) = fromIntegral f / 256

doubleToFixed :: Double -> WlFixed
doubleToFixed d = WlFixed (round (d * 256))


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


data MessagePart = MessagePart Put Int (Seq Fd)

instance Semigroup MessagePart where
  (MessagePart px lx fx) <> (MessagePart py ly fy) = MessagePart (px <> py) (lx + ly) (fx <> fy)

instance Monoid MessagePart where
  mempty = MessagePart mempty 0 mempty


class (Eq a, Show a) => WireFormat a where
  putArgument :: a -> Either SomeException MessagePart
  getArgument :: Get (ProtocolM s a)
  showArgument :: a -> String

instance WireFormat Int32 where
  putArgument x = pure $ MessagePart (putInt32host x) 4 mempty
  getArgument = pure <$> getInt32host
  showArgument = show

instance WireFormat Word32 where
  putArgument x = pure $ MessagePart (putWord32host x) 4 mempty
  getArgument = pure <$> getWord32host
  showArgument = show

instance WireFormat WlFixed where
  putArgument (WlFixed repr) = pure $ MessagePart (putInt32host repr) 4 mempty
  getArgument = pure . WlFixed <$> getInt32host
  showArgument = show

instance WireFormat WlString where
  putArgument (WlString x) = putWaylandString x
  getArgument = pure . WlString <$> getWaylandString
  showArgument = show

instance WireFormat BS.ByteString where
  putArgument x = putWaylandArray x
  getArgument = pure <$> getWaylandArray
  showArgument array = "[array " <> show (BS.length array) <> "B]"

instance KnownSymbol j => WireFormat (ObjectId (j :: Symbol)) where
  putArgument (ObjectId oId) = pure $ MessagePart (putWord32host oId) 4 mempty
  getArgument = pure . ObjectId <$> getWord32host
  showArgument (ObjectId 0) = "null"
  showArgument (ObjectId oId) = symbolVal @j Proxy <> "@" <> show oId

instance WireFormat GenericObjectId where
  putArgument (GenericObjectId oId) = pure $ MessagePart (putWord32host oId) 4 mempty
  getArgument = pure . GenericObjectId <$> getWord32host
  showArgument (GenericObjectId 0) = "null"
  showArgument oId = "[unknown]@" <> show oId

instance KnownSymbol j => WireFormat (NewId (j :: Symbol)) where
  putArgument (NewId newId) = putArgument newId
  getArgument = NewId <<$>> getArgument
  showArgument (NewId newId) = "new " <> symbolVal @j Proxy <> "@" <> show newId

instance WireFormat GenericNewId where
  putArgument (GenericNewId interface version newId) = do
    p1 <- putArgument interface
    p2 <- putArgument version
    p3 <- putArgument newId
    pure (p1 <> p2 <> p3)
  getArgument = GenericNewId <<$>> getArgument <<*>> getArgument <<*>> getArgument
  showArgument (GenericNewId interface version newId) = mconcat ["new ", toString interface, "[v", show version, "]@", show newId]

instance WireFormat Fd where
  putArgument fd = pure (MessagePart mempty 0 (Seq.singleton fd))
  getArgument = pure getFd
  showArgument (Fd fd) = "fd@" <> show fd

getFd :: ProtocolM s Fd
getFd =
  readProtocolVar (.inboxFdsVar) >>= \case
    (fd :<| fds) -> fd <$ writeProtocolVar (.inboxFdsVar) fds
    Empty -> throwM $ ProtocolException "Expected fd"


-- | Class for a proxy type (in the haskell sense) that describes a Wayland interface.
class (
    IsMessage (WireRequest i),
    IsMessage (WireEvent i),
    KnownSymbol (InterfaceName i),
    KnownNat (InterfaceVersion i),
    Typeable i
  )
  => IsInterface i where
  type RequestHandler i
  type EventHandler i
  type WireRequest i
  type WireEvent i
  type InterfaceName i :: Symbol
  type InterfaceVersion i :: Nat

interfaceName :: forall i. IsInterface i => String
interfaceName = symbolVal @(InterfaceName i) Proxy

interfaceVersion :: forall i. IsInterface i => Word32
interfaceVersion = fromIntegral $ natVal @(InterfaceVersion i) Proxy

class Typeable s => IsSide (s :: Side) where
  type MessageHandler s i
  type WireUp s i
  type WireDown s i
  initialId :: Word32
  maximumId :: Word32
  -- | Should be called by generated code _after_ calling a destructor.
  handleDestructor :: IsInterfaceSide s i => Object s i -> ProtocolM s () -> ProtocolM s ()

instance IsSide 'Client where
  type MessageHandler 'Client i = EventHandler i
  type WireUp 'Client i = WireRequest i
  type WireDown 'Client i = WireEvent i
  -- Id #1 is reserved for wl_display
  initialId = 2
  maximumId = 0xfeffffff
  handleDestructor object msgFn = handleDestructorPre object >> msgFn

instance IsSide 'Server where
  type MessageHandler 'Server i = RequestHandler i
  type WireUp 'Server i = WireEvent i
  type WireDown 'Server i = WireRequest i
  initialId = 0xff000000
  maximumId = 0xffffffff
  handleDestructor object msgFn = do
    handleDestructorPre object
    msgFn
    when (oid <= maximumId @'Client) do
      sendWlDisplayDeleteId :: Word32 -> STM () <- asks (.sendWlDisplayDeleteId)
      liftSTM $ sendWlDisplayDeleteId oid
    where
      oid :: Word32
      oid = objectIdValue object.objectId

-- Shared destructor code for client and server
handleDestructorPre :: IsInterfaceSide s i => Object s i -> ProtocolM s ()
handleDestructorPre object = do
  traceM $ "Destroying " <> showObject object
  lift $ writeTVar object.destroyed True


class (
    IsSide s,
    IsInterface i,
    IsMessage (WireUp s i),
    IsMessage (WireDown s i)
  )
  => IsInterfaceSide (s :: Side) i where
  handleMessage :: Object s i -> MessageHandler s i -> WireDown s i -> ProtocolM s ()


getWireDown :: forall s i. IsInterfaceSide s i => Object s i -> Opcode -> Get (ProtocolM s (WireDown s i))
getWireDown = getMessage @(WireDown s i)

putWireUp :: forall s i. IsInterfaceSide s i => Object s i -> WireUp s i -> Either SomeException (Opcode, MessagePart)
putWireUp _ = putMessage @(WireUp s i)


-- | Data kind
data Side = Client | Server
  deriving stock (Eq, Show)


-- | An object belonging to a wayland connection.
data Object s i = IsInterfaceSide s i => Object {
  objectProtocol :: (ProtocolHandle s),
  objectId :: ObjectId (InterfaceName i),
  messageHandler :: TVar (Maybe (MessageHandler s i)),
  -- FIXME type-safe variant for `interfaceData`?
  interfaceData :: TVar Dynamic,
  destroyed :: TVar Bool
}

getMessageHandler :: IsInterfaceSide s i => Object s i -> STM (MessageHandler s i)
getMessageHandler object = maybe (throwM (InternalError ("No message handler attached to " <> showObject object))) pure =<< readTVar object.messageHandler

setMessageHandler :: Object s i -> MessageHandler s i -> STM ()
setMessageHandler object = writeTVar object.messageHandler . Just

setRequestHandler :: Object 'Server i -> RequestHandler i -> STM ()
setRequestHandler = setMessageHandler

setEventHandler :: Object 'Client i -> EventHandler i -> STM ()
setEventHandler = setMessageHandler

-- | Attach interface-specific data to the object. Should only be used by the primary interface implementation.
setInterfaceData :: Typeable a => Object s i -> a -> STM ()
setInterfaceData object value = writeTVar object.interfaceData (toDyn value)

-- | Get interface-specific data that was attached to the object.
getInterfaceData :: Typeable a => Object s i -> STM (Maybe a)
getInterfaceData object = fromDynamic <$> readTVar object.interfaceData

isObjectDestroyed :: Object s i -> STM Bool
isObjectDestroyed object = readTVar object.destroyed

instance HasField "isDestroyed" (Object s i) (STM Bool) where
  getField obj = isObjectDestroyed obj

instance HasField "isDestroyed" (SomeObject s) (STM Bool) where
  getField (SomeObject obj) = isObjectDestroyed obj

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
  describeUpMessage object opcode body = mconcat [
    objectInterfaceName object, "@", show (genericObjectId object),
    ".", fromMaybe "[invalidOpcode]" (opcodeName @(WireUp s i) opcode),
    " (", show (BSL.length body), "B)"]
  describeDownMessage object opcode body = mconcat [
    objectInterfaceName object, "@", show (genericObjectId object),
    ".", fromMaybe "[invalidOpcode]" (opcodeName @(WireDown s i) opcode),
    " (", show (BSL.length body), "B)"]

-- | Wayland object quantification wrapper
data SomeObject s = forall i. IsInterfaceSide s i => SomeObject (Object s i)

instance IsObject (SomeObject s) where
  genericObjectId (SomeObject object) = genericObjectId object
  objectInterfaceName (SomeObject object) = objectInterfaceName object

instance IsObjectSide (SomeObject s) where
  describeUpMessage (SomeObject object) = describeUpMessage object
  describeDownMessage (SomeObject object) = describeDownMessage object


class (Eq a, Show a) => IsMessage a where
  opcodeName :: Opcode -> Maybe String
  getMessage :: IsInterface i => Object s i -> Opcode -> Get (ProtocolM s a)
  putMessage :: a -> Either SomeException (Opcode, MessagePart)

instance IsMessage Void where
  opcodeName _ = Nothing
  getMessage = invalidOpcode
  putMessage = absurd

buildMessage :: Opcode -> [Either SomeException MessagePart] -> Either SomeException (Opcode, MessagePart)
buildMessage opcode parts = (opcode,) . mconcat <$> sequence parts

invalidOpcode :: IsInterface i => Object s i -> Opcode -> Get a
invalidOpcode object opcode = fail $ mconcat [
  "Invalid opcode ", show opcode, " on ", objectInterfaceName object,
  "@", show (genericObjectId object)]

showObjectMessage :: (IsObject a, IsMessage b) => a -> b -> String
showObjectMessage object message =
  showObject object <> "." <> show message


-- * Exceptions

data WireCallbackFailed = WireCallbackFailed SomeException
  deriving stock Show
instance Exception WireCallbackFailed

data ParserFailed = ParserFailed String String
  deriving stock Show
instance Exception ParserFailed

data ProtocolException = ProtocolException String
  deriving stock Show
instance Exception ProtocolException

data ProtocolUsageError = ProtocolUsageError String
  deriving stock Show
instance Exception ProtocolUsageError

data MaximumIdReached = MaximumIdReached
  deriving stock Show
instance Exception MaximumIdReached

data ServerError = ServerError Word32 String
  deriving stock Show
instance Exception ServerError

data InternalError = InternalError String
  deriving stock Show
instance Exception InternalError

data InvalidObject = InvalidObject String
  deriving stock Show
instance Exception InvalidObject

-- * Protocol state and monad plumbing

-- | Top-level protocol handle (used e.g. to send/receive data)
data ProtocolHandle (s :: Side) = ProtocolHandle {
  protocolKey :: Unique,
  stateVar :: TVar (Either SomeException (ProtocolState s))
}

instance Eq (ProtocolHandle s) where
  x == y = x.protocolKey == y.protocolKey

instance Hashable (ProtocolHandle s) where
  hashWithSalt salt x = hashWithSalt salt x.protocolKey
  hash x = hash x.protocolKey

-- | Protocol state handle, containing state for a non-failed protocol (should be kept in a 'ProtocolStateVar')
data ProtocolState (s :: Side) = ProtocolState {
  protocolHandle :: ProtocolHandle s,
  bytesReceivedVar :: TVar Int64,
  bytesSentVar :: TVar Int64,
  inboxDecoderVar :: TVar (Decoder RawMessage),
  inboxFdsVar :: TVar (Seq Fd),
  outboxVar :: TVar (Maybe Put),
  outboxFdsVar :: TVar (Seq Fd),
  objectsVar :: TVar (HashMap GenericObjectId (SomeObject s)),
  nextIdVar :: TVar Word32,
  sendWlDisplayDeleteId :: Word32 -> STM ()
}

type ProtocolM s a = ReaderT (ProtocolState s) STM a

askProtocol :: ProtocolM s (ProtocolHandle s)
askProtocol = asks (.protocolHandle)

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

swapProtocolVar :: (ProtocolState s -> TVar a) -> a -> ProtocolM s a
swapProtocolVar fn x = do
  state <- ask
  lift $ swapTVar (fn state) x

initializeProtocol
  :: forall s wl_display a. (IsInterfaceSide s wl_display)
  => (ProtocolHandle s -> MessageHandler s wl_display)
  -- FIXME only required for server code
  -> (Object s wl_display -> Word32 -> STM ())
  -- ^ Send a wl_display.delete_id message. Because this is part of the core protocol but generated from the xml it has
  -- to be provided by the main server module.
  -> (Object s wl_display -> STM a)
  -> STM (a, ProtocolHandle s)
initializeProtocol wlDisplayMessageHandler sendWlDisplayDeleteId initializationAction = do
  protocolKey <- newUniqueSTM

  bytesReceivedVar <- newTVar 0
  bytesSentVar <- newTVar 0
  inboxDecoderVar <- newTVar $ runGetIncremental getRawMessage
  inboxFdsVar <- newTVar mempty
  outboxVar <- newTVar Nothing
  outboxFdsVar <- newTVar mempty
  objectsVar <- newTVar $ HM.empty
  nextIdVar <- newTVar (initialId @s)

  -- Create uninitialized to avoid use of a diverging 'mfix'
  stateVar <- newTVar (Left unreachableCodePath)

  let protocol = ProtocolHandle {
    protocolKey,
    stateVar
  }

  messageHandlerVar <- newTVar (Just (wlDisplayMessageHandler protocol))
  interfaceData <- newTVar (toDyn ())
  destroyed <- newTVar False
  let wlDisplay = Object {
        objectProtocol = protocol,
        objectId = wlDisplayId,
        messageHandler = messageHandlerVar,
        interfaceData,
        destroyed
      }

  let state = ProtocolState {
    protocolHandle = protocol,
    bytesReceivedVar,
    bytesSentVar,
    inboxDecoderVar,
    inboxFdsVar,
    outboxVar,
    outboxFdsVar,
    objectsVar,
    nextIdVar,
    sendWlDisplayDeleteId = (sendWlDisplayDeleteId wlDisplay)
  }
  writeTVar stateVar (Right state)

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
feedInput :: (IsSide s, MonadIO m) => ProtocolHandle s -> ByteString -> [Fd] -> m ()
feedInput protocol bytes fds = runProtocolTransaction protocol do
  -- Exposing MonadIO instead of STM to the outside and using `runProtocolTransaction` here enforces correct exception
  -- handling.
  modifyProtocolVar' (.bytesReceivedVar) (+ fromIntegral (BS.length bytes))
  modifyProtocolVar (.inboxDecoderVar) (`pushChunk` bytes)
  modifyProtocolVar (.inboxFdsVar) (<> Seq.fromList fds)
  receiveMessages

-- | Set the protocol to a failed state, e.g. when the socket closed unexpectedly.
setException :: (Exception e, MonadIO m) => ProtocolHandle s -> e -> m ()
setException protocol ex = runProtocolTransaction protocol $ throwM ex

-- | Take data that has to be sent. Blocks until data is available.
takeOutbox :: MonadIO m => ProtocolHandle s -> m (BSL.ByteString, [Fd])
takeOutbox protocol = runProtocolTransaction protocol do
  mOutboxData <- stateProtocolVar (.outboxVar) (\mOutboxData -> (mOutboxData, Nothing))
  outboxData <- maybe (lift retry) pure mOutboxData
  let sendData = runPut outboxData
  modifyProtocolVar' (.bytesSentVar) (+ BSL.length sendData)
  fds <- swapProtocolVar (.outboxFdsVar) mempty
  pure (sendData, toList fds)


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once and before using the
-- object).
--
-- For use in generated code.
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
-- For use in generated code.
newObjectFromId
  :: forall s i. IsInterfaceSide s i
  => Maybe (MessageHandler s i)
  -> NewId (InterfaceName i)
  -> ProtocolM s (Object s i)
newObjectFromId messageHandler (NewId oId) = do
  protocol <- askProtocol
  messageHandlerVar <- lift $ newTVar messageHandler
  interfaceDataVar <- lift $ newTVar (toDyn ())
  destroyed <- lift $ newTVar False
  let
    object = Object {
      objectProtocol = protocol,
      objectId = oId,
      messageHandler = messageHandlerVar,
      interfaceData = interfaceDataVar,
      destroyed
    }
    someObject = SomeObject object
  modifyProtocolVar (.objectsVar) (HM.insert (genericObjectId object) someObject)
  pure object


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once and before using the
-- object).
--
-- For implementing wl_registry.bind (which is low-level protocol functionality, but which depends on generated code).
bindNewObject
  :: forall i. IsInterfaceSide 'Client i
  => ProtocolHandle 'Client
  -> Version
  -> Maybe (MessageHandler 'Client i)
  -> STM (Object 'Client i, GenericNewId)
bindNewObject protocol version messageHandler =
  runProtocolM protocol do
    (object, NewId (ObjectId newId)) <- newObject messageHandler
    pure (object, GenericNewId (fromString (interfaceName @i)) version newId)

-- | Create an object from a received id.
-- object).
--
-- For implementing wl_registry.bind (which is low-level protocol functionality, but which depends on generated code).
bindObjectFromId
  :: forall i. IsInterfaceSide 'Server i
  => Maybe (MessageHandler 'Server i)
  -> GenericNewId
  -> ProtocolM 'Server (Object 'Server i)
bindObjectFromId messageHandler (GenericNewId interface version value) =
  newObjectFromId messageHandler (NewId (ObjectId value))


fromSomeObject
  :: forall s i. IsInterfaceSide s i
  => SomeObject s -> Either String (Object s i)
fromSomeObject (SomeObject someObject) =
  case cast someObject of
    Nothing -> Left $ mconcat ["Expected object with type ",
      interfaceName @i, ", but object has type ",
      objectInterfaceName someObject]
    Just object -> pure object


lookupObject
  :: forall s i. IsInterfaceSide s i
  => ObjectId (InterfaceName i)
  -> ProtocolM s (Either String (Object s i))
lookupObject oId = do
  objects <- readProtocolVar (.objectsVar)
  pure case HM.lookup (toGenericObjectId oId) objects of
    Nothing -> Left $ mconcat ["No object with id ", show oId, " is registered"]
    Just someObject ->
      case fromSomeObject someObject of
        Left err -> Left err
        Right object -> pure object

-- | Lookup an object for an id or throw a `ProtocolException`. To be used from generated code when receiving an object
-- id.
getObject
  :: forall s i. IsInterfaceSide s i
  => ObjectId (InterfaceName i)
  -> ProtocolM s (Object s i)
getObject oId = either (throwM . ProtocolException . ("Received invalid object id: " <>)) pure =<< lookupObject oId

-- | Lookup an object for an id or throw a `ProtocolException`. To be used from generated code when receiving an object
-- id.
getNullableObject
  :: forall s i. IsInterfaceSide s i
  => ObjectId (InterfaceName i)
  -> ProtocolM s (Maybe (Object s i))
getNullableObject (ObjectId 0) = pure Nothing
getNullableObject oId = Just <$> getObject oId



-- | Handle a wl_display.error message. Because this is part of the core protocol but generated from the xml it has to
-- be called from the client module.
handleWlDisplayError :: ProtocolHandle 'Client -> GenericObjectId -> Word32 -> WlString -> STM ()
handleWlDisplayError _protocol _oId code message =
  -- TODO lookup object id
  throwM $ ServerError code (toString message)

-- | Handle a wl_display.delete_id message. Because this is part of the core protocol but generated from the xml it has
-- to be called from the client module.
handleWlDisplayDeleteId :: ProtocolHandle 'Client -> Word32 -> STM ()
handleWlDisplayDeleteId protocol oId = runProtocolM protocol do
  -- TODO mark as deleted
  modifyProtocolVar (.objectsVar) $ HM.delete (GenericObjectId oId)


checkObject :: IsInterface i => Object s i -> ProtocolM s (Either String ())
checkObject object = do
  -- TODO check if object belongs to current connection
  isActiveObject <- HM.member (genericObjectId object) <$> readProtocolVar (.objectsVar)
  pure
    if isActiveObject
      then pure ()
      else Left $ mconcat ["Object ", show object, " has been deleted"]


-- | Verify that an object can be used as an argument (throws otherwise) and return its id.
objectWireArgument :: IsInterface i => Object s i -> ProtocolM s (ObjectId (InterfaceName i))
objectWireArgument object = do
  checkObject object >>= \case
    Left msg -> throwM $ ProtocolUsageError $ "Tried to send a reference to an invalid object: " <> msg
    Right () -> pure object.objectId

-- | Verify that an object can be used as an argument (throws otherwise) and return its id.
nullableObjectWireArgument :: IsInterface i => Maybe (Object s i) -> ProtocolM s (ObjectId (InterfaceName i))
nullableObjectWireArgument Nothing = pure (ObjectId 0)
nullableObjectWireArgument (Just object) = objectWireArgument object


-- | Sends a message, for use in generated code.
sendMessage :: forall s i. IsInterfaceSide s i => Object s i -> WireUp s i -> ProtocolM s ()
sendMessage object message = do
  checkObject object >>= \case
    Left msg -> throwM $ ProtocolUsageError $ "Tried to send message to an invalid object: " <> msg
    Right () -> pure ()

  (opcode, MessagePart putBody bodyLength fds) <- either throwM pure $ putWireUp object message

  when (bodyLength > fromIntegral (maxBound :: Word16)) $
    throwM $ ProtocolUsageError $ "Tried to send message larger than 2^16 bytes"

  traceM $ "-> " <> showObjectMessage object message
  sendRawMessage (putHeader opcode (8 + bodyLength) >> putBody) fds
  where
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
        handleMessage @s @i object messageHandler message

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

getWaylandString :: Get BS.ByteString
getWaylandString = do
  Just (string, 0) <- BS.unsnoc <$> getWaylandArray
  pure string

getWaylandArray :: Get BS.ByteString
getWaylandArray = do
  size <- getWord32host
  array <- getByteString (fromIntegral size)
  skipPadding
  pure array

putWaylandString :: MonadThrow m => BS.ByteString -> m MessagePart
putWaylandString blob = do
  when (len > fromIntegral (maxBound :: Word16)) $
    throwM $ ProtocolUsageError $ "Tried to send string larger than 2^16 bytes"

  pure $ MessagePart putBlob (4 + len + pad) mempty
  where
    -- Total data length including null byte
    len = BS.length blob + 1
    -- Padding length
    pad = padding len
    putBlob = do
      putWord32host (fromIntegral len)
      putByteString blob
      putWord8 0
      replicateM_ pad (putWord8 0)

putWaylandArray :: MonadThrow m => BS.ByteString -> m MessagePart
putWaylandArray blob = do
  when (len > fromIntegral (maxBound :: Word16)) $
    throwM $ ProtocolUsageError $ "Tried to send array larger than 2^16 bytes"

  pure $ MessagePart putBlob (4 + len + pad) mempty
  where
    -- Total data length without padding
    len = BS.length blob
    -- Padding length
    pad = padding len
    putBlob = do
      putWord32host (fromIntegral len)
      putByteString blob
      replicateM_ pad (putWord8 0)


skipPadding :: Get ()
skipPadding = do
  bytes <- bytesRead
  skip $ fromIntegral (padding bytes)

padding :: Integral a => a -> a
padding size = ((4 - (size `mod` 4)) `mod` 4)


sendRawMessage :: Put -> Seq Fd -> ProtocolM s ()
sendRawMessage x fds = do
  modifyProtocolVar (.outboxVar) (Just . maybe x (<> x))
  modifyProtocolVar (.outboxFdsVar) (<> fds)
