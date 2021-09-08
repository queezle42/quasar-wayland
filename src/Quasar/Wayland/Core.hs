module Quasar.Wayland.Core (
  ObjectId,
  Opcode,
  Fixed,
  IsInterface(..),
  Side(..),
  IsSide,
  Object,
  IsSomeObject(..),
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

  -- Message decoder operations
  WireGet,
  WireFormat(..),
  dropRemaining,
) where

import Control.Monad (replicateM_)
import Control.Monad.Catch
import Control.Monad.Catch.Pure
import Control.Monad.Reader (ReaderT, runReaderT)
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
import Data.Void (absurd)
import GHC.TypeLits
import Quasar.Prelude


type ObjectId = Word32
type Opcode = Word16

-- | Signed 24.8 decimal numbers.
newtype Fixed = Fixed Word32
  deriving Eq



type WireGet s m a =
  ReaderT (HashMap ObjectId (SomeObject s m))
    (WriterT [ProtocolAction s m ()]
      (CatchT
        Get
      )
    )
    a

runWireGet
  :: MonadCatch m
  => HashMap ObjectId (SomeObject s m)
  -> WireGet s m ()
  -> Get (ProtocolAction s m ())
runWireGet objects action = do
  result <- runCatchT $ execWriterT $ runReaderT action objects
  case result of
    Left ex -> pure $ throwM ex
    Right actions -> pure $ sequence_ actions

wireQueueAction :: ProtocolAction s m () -> WireGet s m ()
wireQueueAction action = tell [action]


liftGet :: Get a -> WireGet s m a
liftGet = lift . lift . lift

dropRemaining :: WireGet s m ()
dropRemaining = liftGet $ void getRemainingLazyByteString


class WireFormat a where
  type Argument a
  putArgument :: Argument a -> StateT (ProtocolState s m) PutM ()
  getArgument :: WireGet s m (Argument a)

instance WireFormat "int" where
  type Argument "int" = Int32
  putArgument = lift . putInt32host
  getArgument = liftGet getInt32host

instance WireFormat "uint" where
  type Argument "uint" = Word32
  putArgument = lift . putWord32host
  getArgument = liftGet getWord32host

instance WireFormat "fixed" where
  type Argument "fixed" = Fixed
  putArgument (Fixed repr) = lift $ putWord32host repr
  getArgument = liftGet $ Fixed <$> getWord32host

instance WireFormat "string" where
  type Argument "string" = BS.ByteString
  putArgument = lift . putWaylandBlob
  getArgument = liftGet getWaylandBlob

instance forall (s :: Side) m i. MonadCatch m => WireFormat (Object s m i) where
  type Argument (Object s m i) = Object s m i
  putArgument = undefined
  getArgument = undefined

instance WireFormat (NewId s m i) where
  type Argument (NewId s m i) = (NewId s m i)
  putArgument = undefined
  getArgument = undefined

instance WireFormat "array" where
  type Argument "array" = BS.ByteString
  putArgument = lift . putWaylandBlob
  getArgument = liftGet getWaylandBlob

instance WireFormat "fd" where
  type Argument "fd" = Void
  putArgument = undefined
  getArgument = undefined


-- | A wayland interface
class
  (
    Binary (Request i),
    Binary (Event i),
    IsMessage (Request i),
    IsMessage (Event i),
    IsMessage (Up 'Client i),
    IsMessage (Up 'Server i),
    IsMessage (Down 'Client i),
    IsMessage (Down 'Server i)
  )
  => IsInterface i where
  type Request i
  type Event i
  interfaceName :: String

class IsSide (s :: Side) where
  type Up s i
  type Down s i
  getDown :: forall m i. IsInterface i => Object s m i -> Opcode -> WireGet s m (Down s i)

instance IsSide 'Client where
  type Up 'Client i = Request i
  type Down 'Client i = Event i
  getDown :: forall m i. IsInterface i => Object 'Client m i -> Opcode -> WireGet 'Client m (Down 'Client i)
  getDown = getMessage @(Down 'Client i)

instance IsSide 'Server where
  type Up 'Server i = Event i
  type Down 'Server i = Request i
  getDown :: forall m i. IsInterface i => Object 'Server m i -> Opcode -> WireGet 'Server m (Down 'Server i)
  getDown = getMessage @(Down 'Server i)



-- | Data kind
data Side = Client | Server

data Object s m i = IsInterface i => Object ObjectId (Callback s m i)


instance forall s m i. IsInterface i => IsSomeObject (Object s m i) where
  objectId (Object oId _) = oId
  objectInterfaceName _ = interfaceName @i


class IsSomeObject a where
  objectId :: a -> ObjectId
  objectInterfaceName :: a -> String

-- | Wayland object quantification wrapper
data SomeObject s m = forall i. IsInterface i => SomeObject (Object s m i)

instance IsSomeObject (SomeObject s m) where
  objectId (SomeObject object) = objectId object
  objectInterfaceName (SomeObject object) = objectInterfaceName object


data NewId s m i = IsInterface i => NewId ObjectId


class IsMessage a where
  messageName :: a -> String
  getMessage :: IsInterface i => Object s m i -> Opcode -> WireGet s m a
  putMessage :: a -> StateT (ProtocolState s m) PutM ()

instance IsMessage Void where
  messageName = absurd
  getMessage = invalidOpcode
  putMessage = absurd

describeMessage
  :: forall s m i. IsInterface i
  => Object s m i
  -> Opcode
  -> BSL.ByteString
  -> String
describeMessage object opcode body =
  objectInterfaceName object <> "@" <> show (objectId object) <>
  ".msg#" <> show opcode <>
  " (" <> show (BSL.length body) <> "B)"

invalidOpcode :: IsInterface i => Object s m i -> Opcode -> WireGet s m a
invalidOpcode object opcode =
  throwM $ ProtocolException $ "Invalid opcode " <> show opcode <> " on " <> objectInterfaceName object <> "@" <> show (objectId object)


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
  :: forall wl_display wl_registry s m. (IsInterface wl_display, IsInterface wl_registry)
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
takeOutbox st = (outboxBytes, st{outbox = Nothing})
  where
    outboxBytes = if isJust st.protocolException then Nothing else runPut <$> st.outbox


sendInitialMessage :: ProtocolState s m -> ProtocolState s m
sendInitialMessage = sendMessageInternal 1 1 [NewIdArgument 2]

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
    Just (SomeObject object) -> do
      case runGetOrFail (getMessageAction st.objects object rawMessage) body of
        Left (_, _, message) ->
          throwM $ ParserFailed (describeMessage object opcode body) message
        Right ("", _, result) ->
          traceM $ "Received message " <> (describeMessage object opcode body)
        Right (leftovers, _, _) ->
          throwM $ ParserFailed (describeMessage object opcode body) (show (BSL.length leftovers) <> "B not parsed")

getMessageAction
  :: (IsSide s, IsInterface i, MonadCatch m)
  => HashMap ObjectId (SomeObject s m)
  -> Object s m i
  -> RawMessage
  -> Get (ProtocolAction s m ())
getMessageAction objects object@(Object _ callback) (oId, opcode, body) =
  runWireGet objects do
    message <- getDown object opcode
    wireQueueAction $ traceM $ "Received message " <> describeMessage object opcode body

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
    msgSizeInteger = foldr ((+) . (fromIntegral . argumentSize)) 8 args :: Integer

sendRaw :: Put -> ProtocolState s m -> ProtocolState s m
sendRaw x oldState = oldState {
  outbox = Just (maybe x (<> x) oldState.outbox)
}
