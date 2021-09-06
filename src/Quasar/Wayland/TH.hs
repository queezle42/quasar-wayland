module Quasar.Wayland.TH (
  generateWaylandProcol
) where

import Quasar.Prelude
import Text.XML.Light
import Data.ByteString qualified as BS

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (addDependentFile)
--import Quasar.Wayland.Core



generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- parseProtocol xml

  traceIO $ show $ interfaces protocol

  pure []


type Opcode = Word16

data Protocol = Protocol {interfaces :: [Interface]}
  deriving stock (Show)

data Interface = Interface {
  name :: String,
  requests :: [Request],
  events :: [Event]
}
  deriving stock (Show)

data Request = Request {
  name :: String,
  opcode :: Opcode
}
  deriving stock (Show)

data Event = Event {
  name :: String,
  opcode :: Opcode
}
  deriving stock (Show)

parseProtocol :: MonadFail m => BS.ByteString -> m Protocol
parseProtocol xml = do
  (Just element) <- pure $ parseXMLDoc xml
  interfaces <- mapM parseInterface $ findChildren (blank_name { qName = "interface" }) element
  pure Protocol {
    interfaces
  }

parseInterface :: MonadFail m => Element -> m Interface
parseInterface element = do
  name <- getAttr "name" element
  requests <- mapM parseRequest $ zip [0..] $ findChildren (qname "request") element
  events <- mapM parseEvent $ zip [0..] $ findChildren (qname "events") element
  pure Interface {
    name,
    requests,
    events
  }

parseRequest :: MonadFail m => (Opcode, Element) -> m Request
parseRequest (opcode, element) = do
  name <- getAttr "name" element
  pure Request {
    name,
    opcode
  }

parseEvent :: MonadFail m => (Opcode, Element) -> m Event
parseEvent (opcode, element) = do
  name <- getAttr "name" element
  pure Event {
    name,
    opcode
  }

qname :: String -> QName
qname name = blank_name { qName = name }

getAttr :: MonadFail m => String -> Element -> m String
getAttr name element = do
  (Just value) <- pure $ findAttr (qname name) element
  pure value
