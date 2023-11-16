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
  nullWlString,
  SharedFd,
  IsSide(..),
  Side(..),
  IsInterface(..),
  interfaceName,
  Version,
  maxVersion,
  interfaceVersion,
  IsInterfaceSide(..),
  Object(objectProtocol, version),
  setEventHandler,
  setRequestHandler,
  setMessageHandler,
  getMessageHandler,
  setInterfaceData,
  getInterfaceData,
  isObjectDestroyed,
  attachFinalizer,
  NewObject,
  IsObject,
  IsMessage(..),
  ProtocolHandle,
  ProtocolM,
  CallM,
  runCallM,
  tryCall,
  close,
  CallbackM,
  runCallbackM,

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
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable(hash, hashWithSalt))
import Data.Proxy
import Data.Sequence (Seq(Empty, (:<|)))
import Data.Sequence qualified as Seq
import Data.String (IsString(..))
import Data.Typeable (Typeable, cast)
import Data.Void (absurd)
import GHC.Records
import GHC.TypeLits
import Quasar.Prelude
import Quasar.Wayland.Utils.SharedFd


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

maxVersion :: Version
maxVersion = maxBound


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
  deriving newtype (Eq, Hashable, Monoid, Semigroup)

instance Show WlString where
  show = show . toString

instance IsString WlString where
  fromString = WlString . BSUTF8.fromString

toString :: WlString -> String
toString (WlString bs) = BSUTF8.toString bs

nullWlString :: WlString -> Bool
nullWlString (WlString bs) = BS.null bs


data MessagePart = MessagePart Put Int (Seq SharedFd)

instance Semigroup MessagePart where
  (MessagePart px lx fx) <> (MessagePart py ly fy) = MessagePart (px <> py) (lx + ly) (fx <> fy)

instance Monoid MessagePart where
  mempty = MessagePart mempty 0 mempty


class Show a => WireFormat a where
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

instance WireFormat SharedFd where
  putArgument fd = pure (MessagePart mempty 0 (Seq.singleton fd))
  getArgument = pure getFd
  showArgument fd = show fd

getFd :: ProtocolM s SharedFd
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
  handleDestructor object msgFn = do
    liftSTMc $ handleDestructorPre object
    msgFn
    liftSTMc $ handleDestructorPost object

instance IsSide 'Server where
  type MessageHandler 'Server i = RequestHandler i
  type WireUp 'Server i = WireEvent i
  type WireDown 'Server i = WireRequest i
  initialId = 0xff000000
  maximumId = 0xffffffff
  handleDestructor object msgFn = do
    liftSTMc $ handleDestructorPre object
    msgFn
    when (oid <= maximumId @'Client) do
      sendWlDisplayDeleteId :: Word32 -> CallM () <- asks (.sendWlDisplayDeleteId)
      runCallM $ sendWlDisplayDeleteId oid
    liftSTMc $ handleDestructorPost object
    where
      oid :: Word32
      oid = objectIdValue object.objectId

-- Shared destructor code for client and server
handleDestructorPre :: IsInterfaceSide s i => Object s i -> STMc NoRetry '[] ()
handleDestructorPre object = do
  traceM $ "Destroying " <> showObject object
  writeTVar object.destroyed True

-- Shared destructor code for client and server
handleDestructorPost :: IsInterfaceSide s i => Object s i -> STMc NoRetry '[]  ()
handleDestructorPost object = liftSTMc do
  finalizers <- swapTVar object.finalizers []
  unless (null finalizers) do
    traceM $ "Running finalizers for " <> showObject object
    sequence_ finalizers
  traceM $ "Destroyed " <> showObject object


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


-- TODO use TypeData in a future GHC (hopefully 9.6.1)
-- | Data kind
data Side = Client | Server
  deriving stock (Eq, Show)


-- | An object belonging to a wayland connection.
data Object s i = IsInterfaceSide s i => Object {
  objectProtocol :: ProtocolHandle s,
  objectId :: ObjectId (InterfaceName i),
  version :: Version,
  messageHandler :: TVar (Maybe (MessageHandler s i)),
  -- FIXME type-safe variant for `interfaceData`?
  interfaceData :: TVar Dynamic,
  finalizers :: TVar [STMc NoRetry '[] ()],
  destroyed :: TVar Bool
}

instance HasField "isDestroyed" (Object s i) (STMc NoRetry '[] Bool) where
  getField = isObjectDestroyed

instance HasField "isDestroyed" (SomeObject s) (STMc NoRetry '[] Bool) where
  getField (SomeObject obj) = isObjectDestroyed obj


getMessageHandler :: (IsInterfaceSide s i, MonadSTMc NoRetry '[SomeException] m) => Object s i -> m (MessageHandler s i)
getMessageHandler object = liftSTMc @NoRetry @'[SomeException] do
  readTVar object.messageHandler >>= \case
    Just handler -> pure handler
    Nothing -> throwM (InternalError ("No message handler attached to " <> showObject object))

setMessageHandler :: MonadSTMc NoRetry '[] m => Object s i -> MessageHandler s i -> m ()
setMessageHandler object = writeTVar object.messageHandler . Just

setRequestHandler :: MonadSTMc NoRetry '[] m => Object 'Server i -> RequestHandler i -> m ()
setRequestHandler = setMessageHandler

setEventHandler :: MonadSTMc NoRetry '[] m => Object 'Client i -> EventHandler i -> m ()
setEventHandler = setMessageHandler

-- | Attach interface-specific data to the object. Should only be used by the primary interface implementation.
setInterfaceData :: (Typeable a, MonadSTMc NoRetry '[] m) => Object s i -> a -> m ()
setInterfaceData object value = writeTVar object.interfaceData (toDyn value)

-- | Get interface-specific data that was attached to the object.
getInterfaceData :: (Typeable a, MonadSTMc NoRetry '[] m) => Object s i -> m (Maybe a)
getInterfaceData object = fromDynamic <$> readTVar object.interfaceData

-- | Returns true if the object is destroyed, or the underlying wayland connection has been closed.
isObjectDestroyed :: MonadSTMc NoRetry '[] m => Object s i -> m Bool
isObjectDestroyed object = liftSTMc do
  destroyed <- readTVar object.destroyed
  if destroyed
    then pure True
    else isDisconnected object.objectProtocol

-- | Attach a finalizer to an object that is run when the object is destroyed
-- or the wayland connection is closed.
attachFinalizer :: MonadSTMc NoRetry '[] m => Object s i -> STMc NoRetry '[] () -> m ()
attachFinalizer object finalizer = modifyTVar object.finalizers (finalizer:)

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


class Show a => IsMessage a where
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
  inboxFdsVar :: TVar (Seq SharedFd),
  outboxVar :: TVar (Maybe Put),
  outboxFdsVar :: TVar (Seq SharedFd),
  objectsVar :: TVar (HashMap GenericObjectId (SomeObject s)),
  nextIdVar :: TVar Word32,
  sendWlDisplayDeleteId :: Word32 -> CallM ()
}

isDisconnected :: ProtocolHandle s -> STMc NoRetry '[] Bool
isDisconnected protocol =
  readTVar protocol.stateVar <&> \case
    Left _ -> True
    Right _ -> False

type ProtocolM s a = ReaderT (ProtocolState s) (STMc NoRetry '[SomeException]) a

-- | A type alias for a monad that is used for wayland request- and event proxy
-- functions.
type CallM a = STMc NoRetry '[SomeException] a

runCallM :: CallM a -> ProtocolM s a
runCallM = liftSTMc

-- | Try to send an request or an event and ignore all exceptions.
--
-- Intended to be used for destructors (including @wl_callback.done@), since
-- they are often called in exception-free contexts, are a no-op when the object
-- is destroyed or disconnected, and usually do not throw exceptions during
-- message serialisation.
--
-- TODO Should maybe abort the connection if an unexpected exception occurs,
-- i.e. exceptions other than destroyed or disconnected states.
tryCall :: CallM () -> STMc NoRetry '[] ()
tryCall call = do
  tryAllSTMc call >>= \case
    Left ex -> traceM $ "TODO handle exception during tryCall; exception is: " <> show ex
    Right () -> pure ()

-- | A type alias for a monad that is used for wayland request- and event
-- callbacks.
type CallbackM a = STMc NoRetry '[SomeException] a

runCallbackM :: CallM a -> ProtocolM s a
runCallbackM = liftSTMc

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

initializeProtocol ::
  forall s wl_display m a.
  (IsInterfaceSide s wl_display, MonadSTMc NoRetry '[] m) =>
  (ProtocolHandle s -> MessageHandler s wl_display) ->
  -- | Send a wl_display.delete_id message. Because this is part of the core protocol but generated from the xml it has
  -- to be provided by the main server module.
  --
  -- FIXME only required for server code
  (Object s wl_display -> Word32 -> CallM ()) ->
  (Object s wl_display -> m a) ->
  m (a, ProtocolHandle s)
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
  finalizers <- newTVar mempty
  destroyed <- newTVar False
  let wlDisplay = Object {
        objectProtocol = protocol,
        objectId = wlDisplayId,
        version = 1,
        messageHandler = messageHandlerVar,
        interfaceData,
        finalizers,
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
    sendWlDisplayDeleteId = sendWlDisplayDeleteId wlDisplay
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
runProtocolTransaction protocol action =
  disconnectOnException protocol (liftSTMc . runReaderT action)

-- | Run a protocol action in 'IO'. If an exception occurs, it is stored as a protocol failure and is then
-- re-thrown.
--
-- Throws an exception, if the protocol is already in a failed state.
disconnectOnException :: MonadIO m => ProtocolHandle s -> (ProtocolState s -> STM a) -> m a
disconnectOnException protocol action = do
  result <- liftIO $ atomically do
    readTVar protocol.stateVar >>= \case
      -- Protocol is already in a failed state
      Left ex -> throwM ex
      Right state -> do
        -- Run action, catch exceptions
        try (action state) >>= \case
          Left ex -> do
            -- Action failed, change protocol state to failed
            close protocol ex
            writeTVar protocol.stateVar (Left ex)
            pure (Left ex)
          Right result -> do
            pure (Right result)
  -- Transaction is committed, rethrow exception if the action failed
  either (liftIO . throwM) pure result

close :: MonadSTMc NoRetry '[] m => ProtocolHandle s -> SomeException -> m ()
close protocol exception = liftSTMc do
  readTVar protocol.stateVar >>= \case
    Left _ -> pure ()
    Right state -> do
      writeTVar protocol.stateVar (Left exception)
      objects <- readTVar state.objectsVar
      forM_ objects \(SomeObject object) -> do
        finalizers <- readTVar object.finalizers
        unless (null finalizers) do
          traceM $ "Running finalizers for " <> showObject object
          sequence_ finalizers


-- | Run a 'ProtocolM'-action inside 'STM'.
--
-- Throws an exception, if the protocol is already in a failed state.
--
-- Exceptions are not handled (i.e. they usually reset the STM transaction and are not stored as a protocol failure).
runProtocolM :: ProtocolHandle s -> ProtocolM s a -> STMc NoRetry '[SomeException] a
runProtocolM protocol action = either throwM (liftSTMc . runReaderT action) =<< readTVar protocol.stateVar


-- | Feed the protocol newly received data.
feedInput :: (IsSide s, MonadIO m) => ProtocolHandle s -> ByteString -> [SharedFd] -> m ()
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
takeOutbox :: MonadIO m => ProtocolHandle s -> m (BSL.ByteString, [SharedFd])
takeOutbox protocol = disconnectOnException protocol \state -> do
  mOutboxData <- swapTVar state.outboxVar Nothing
  outboxData <- maybe retry pure mOutboxData
  let sendData = runPut outboxData
  modifyTVar' state.bytesSentVar (+ BSL.length sendData)
  fds <- swapTVar state.outboxFdsVar mempty
  pure (sendData, toList fds)


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once and before using the
-- object).
--
-- Used in generated code.
newObject
  :: forall s i. IsInterfaceSide s i
  => Maybe (MessageHandler s i)
  -> Version
  -> ProtocolM s (Object s i, NewId (InterfaceName i))
newObject messageHandler version = do
  oId <- allocateObjectId
  let newId = NewId @(InterfaceName i) oId
  object <- newObjectFromId messageHandler version newId
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
-- Used in generated code.
newObjectFromId
  :: forall s i. IsInterfaceSide s i
  => Maybe (MessageHandler s i)
  -> Version
  -> NewId (InterfaceName i)
  -> ProtocolM s (Object s i)
newObjectFromId messageHandler version (NewId oId) = do
  -- TODO verify (version <= interfaceVersion @i)
  protocol <- askProtocol
  messageHandlerVar <- newTVar messageHandler
  interfaceDataVar <- newTVar (toDyn ())
  finalizers <- newTVar mempty
  destroyed <- newTVar False
  let
    object = Object {
      objectProtocol = protocol,
      objectId = oId,
      version,
      messageHandler = messageHandlerVar,
      interfaceData = interfaceDataVar,
      finalizers,
      destroyed
    }
    someObject = SomeObject object
  modifyProtocolVar (.objectsVar) (HM.insert (genericObjectId object) someObject)
  pure object


-- | Create an object. The caller is responsible for sending the 'NewId' immediately (exactly once and before using the
-- object).
--
-- For implementing the client side of wl_registry.bind (which is low-level protocol functionality, but which depends on generated code).
bindNewObject
  :: forall i. IsInterfaceSide 'Client i
  => ProtocolHandle 'Client
  -> Version
  -> Maybe (MessageHandler 'Client i)
  -> STMc NoRetry '[SomeException] (Object 'Client i, GenericNewId)
bindNewObject protocol version messageHandler =
  runProtocolM protocol do
    (object, NewId (ObjectId newId)) <- newObject messageHandler version
    pure (object, GenericNewId (fromString (interfaceName @i)) version newId)

-- | Create an object from a received id.
-- object).
--
-- For implementing the server side of wl_registry.bind (which is low-level protocol functionality, but which depends on generated code).
bindObjectFromId
  :: forall i. IsInterfaceSide 'Server i
  => Maybe (MessageHandler 'Server i)
  -> Version
  -> GenericNewId
  -> ProtocolM 'Server (Object 'Server i)
bindObjectFromId messageHandler supportedVersion (GenericNewId interface version value) = do
  when (interface /= fromString (interfaceName @i)) $ throwM $ userError "wl_registry.bind: Interface does not match requested global"
  when (version > supportedVersion) $ throwM $ userError $ mconcat ["wl_registry.bind: Invalid version (requested: ", show version, ", available: ", show maxVersion, ")"]
  newObjectFromId messageHandler version (NewId (ObjectId value))


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
handleWlDisplayError :: ProtocolHandle 'Client -> GenericObjectId -> Word32 -> WlString -> STMc NoRetry '[SomeException] ()
handleWlDisplayError _protocol _oId code message =
  -- TODO lookup object id
  throwM $ ServerError code (toString message)

-- | Handle a wl_display.delete_id message. Because this is part of the core protocol but generated from the xml it has
-- to be called from the client module.
handleWlDisplayDeleteId :: ProtocolHandle 'Client -> Word32 -> STMc NoRetry '[SomeException] ()
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
    throwM $ ProtocolUsageError "Tried to send message larger than 2^16 bytes"

  traceM $ "-> " <> showObjectMessage object message
  sendRawMessage (putHeader opcode (8 + bodyLength) >> putBody) fds
  where
    (GenericObjectId objectIdWord) = genericObjectId object
    putHeader :: Opcode -> Int -> Put
    putHeader opcode msgSize = do
      putWord32host objectIdWord
      putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode

enterObject :: forall s i a. Object s i -> ProtocolM s a -> CallM a
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
    throwM $ ProtocolUsageError "Tried to send string larger than 2^16 bytes"

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
    throwM $ ProtocolUsageError "Tried to send array larger than 2^16 bytes"

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
padding size = (4 - (size `mod` 4)) `mod` 4


sendRawMessage :: Put -> Seq SharedFd -> ProtocolM s ()
sendRawMessage x fds = do
  modifyProtocolVar (.outboxVar) (Just . maybe x (<> x))
  modifyProtocolVar (.outboxFdsVar) (<> fds)
