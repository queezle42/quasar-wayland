module Quasar.Wayland.Core (
  ProtocolState,
  ClientProtocolState,
  initialClientProtocolState,
  --ServerProtocolState,
  --initialServerProtocolState,
  Request,
  Event,
  initialProtocolState,
  feedInput,
  takeOutbox,
) where

import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Quasar.Prelude


type ObjectId = Word32
type ObjectType = String
type Opcode = Word16

data Object = Object {
  objectId :: ObjectId,
  objectType :: ObjectType
}


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


type ClientProtocolState = ProtocolState Request Event
type ServerProtocolState = ProtocolState Event Request

data ProtocolState up down = ProtocolState {
  bytesReceived :: Word64,
  bytesSent :: Word64,
  parser :: Decoder down,
  inboxDecoder :: Decoder down,
  outbox :: Maybe Put,
  objects :: HashMap ObjectId Object
}

data Request = Request ObjectId Opcode BSL.ByteString
  deriving stock Show
data Event = Event ObjectId Opcode (Either BSL.ByteString (Word32, BSL.ByteString, Word32))
  deriving stock Show

initialClientProtocolState :: ClientProtocolState
initialClientProtocolState = initialProtocolState decodeEvent

initialProtocolState :: Get down -> ProtocolState up down
initialProtocolState downGet = sendInitialMessage ProtocolState {
  bytesReceived = 0,
  bytesSent = 0,
  parser = runGetIncremental downGet,
  inboxDecoder = runGetIncremental downGet,
  outbox = Nothing,
  objects = HM.singleton 1 (Object 1 "wl_display")
}

sendInitialMessage :: ProtocolState up down -> ProtocolState up down
sendInitialMessage = sendMessage 1 1 [NewIdArgument 2]

feedInput :: forall up down. ByteString -> ProtocolState up down -> ([down], ProtocolState up down)
feedInput bytes = State.runState do
  State.modify (receive bytes)
  go
  where
    go :: State (ProtocolState up down) [down]
    go = State.state takeDownMsg >>= \case
      Nothing -> pure []
      Just msg -> (msg :) <$> go


receive :: forall up down. ByteString -> ProtocolState up down -> ProtocolState up down
receive bytes state = state {
  bytesReceived = state.bytesReceived + fromIntegral (BS.length bytes),
  inboxDecoder = pushChunk state.inboxDecoder bytes
}

takeDownMsg :: forall up down. ProtocolState up down -> (Maybe down, ProtocolState up down)
takeDownMsg state = (result, state{inboxDecoder = newDecoder})
  where
    result :: Maybe down
    newDecoder :: Decoder down
    (result, newDecoder) = checkDecoder state.inboxDecoder
    checkDecoder :: Decoder down -> (Maybe down, Decoder down)
    checkDecoder (Fail _ _ _) = undefined
    checkDecoder x@(Partial _) = (Nothing, x)
    checkDecoder (Done leftovers _ result) = (Just result, pushChunk state.parser leftovers)


decodeEvent :: Get Event
decodeEvent = do
  objectId <- getWord32host
  sizeAndOpcode <- getWord32host
  let
    size = fromIntegral (sizeAndOpcode `shiftR` 16) - 8
    opcode = fromIntegral (sizeAndOpcode .&. 0xFFFF)
  body <- if (objectId == 2 && opcode == 0)
             then Right <$> parseGlobal
             else Left <$> getLazyByteString size <* skipPadding
  pure $ Event objectId opcode body
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


sendMessage :: ObjectId -> Opcode -> [Argument] -> ProtocolState up down -> ProtocolState up down
sendMessage objectId opcode args = sendRaw do
  putWord32host objectId
  putWord32host $ (fromIntegral msgSize `shiftL` 16) .|. fromIntegral opcode
  mapM_ putArgument args
  -- TODO padding
  where
    msgSize :: Word16
    msgSize = if msgSizeInteger <= fromIntegral (maxBound :: Word16) then fromIntegral msgSizeInteger else undefined
    msgSizeInteger :: Integer
    msgSizeInteger = foldr ((+) . (fromIntegral . argumentSize)) 8 args :: Integer

sendRaw :: Put -> ProtocolState up down -> ProtocolState up down
sendRaw put oldState = oldState {
  outbox = Just (maybe put (<> put) oldState.outbox)
}

takeOutbox :: ProtocolState up down -> (Maybe BSL.ByteString, ProtocolState up down)
takeOutbox state = (runPut <$> state.outbox, state{outbox = Nothing})
akeOutbox state = (runPut <$> state.outbox, state{outbox = Nothing})
