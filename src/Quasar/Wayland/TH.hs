module Quasar.Wayland.TH (
  generateWaylandProcol
) where

import Quasar.Prelude
import Text.XML.Light
import Data.ByteString qualified as BS

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (addDependentFile)
import Quasar.Wayland.Core



generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- parseProtocol xml

  traceIO $ show $ interfaces protocol

  pure []


data ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock (Show)

data InterfaceSpec = InterfaceSpec {
  name :: String,
  requests :: [RequestSpec],
  events :: [EventSpec]
}
  deriving stock (Show)

data RequestSpec = RequestSpec {
  name :: String,
  opcode :: Opcode
}
  deriving stock (Show)

data EventSpec = EventSpec {
  name :: String,
  opcode :: Opcode
}
  deriving stock (Show)

parseProtocol :: MonadFail m => BS.ByteString -> m ProtocolSpec
parseProtocol xml = do
  (Just element) <- pure $ parseXMLDoc xml
  interfaces <- mapM parseInterface $ findChildren (blank_name { qName = "interface" }) element
  pure ProtocolSpec {
    interfaces
  }

parseInterface :: MonadFail m => Element -> m InterfaceSpec
parseInterface element = do
  name <- getAttr "name" element
  requests <- mapM parseRequest $ zip [0..] $ findChildren (qname "request") element
  events <- mapM parseEvent $ zip [0..] $ findChildren (qname "events") element
  pure InterfaceSpec {
    name,
    requests,
    events
  }

parseRequest :: MonadFail m => (Opcode, Element) -> m RequestSpec
parseRequest (opcode, element) = do
  name <- getAttr "name" element
  pure RequestSpec {
    name,
    opcode
  }

parseEvent :: MonadFail m => (Opcode, Element) -> m EventSpec
parseEvent (opcode, element) = do
  name <- getAttr "name" element
  pure EventSpec {
    name,
    opcode
  }

qname :: String -> QName
qname name = blank_name { qName = name }

getAttr :: MonadFail m => String -> Element -> m String
getAttr name element = do
  (Just value) <- pure $ findAttr (qname name) element
  pure value
