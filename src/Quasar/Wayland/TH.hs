module Quasar.Wayland.TH (
  generateWaylandProcol
) where

import Quasar.Prelude
import Text.XML.Light
import Data.ByteString qualified as BS

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (addDependentFile)


generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- loadProtocol xml

  traceIO $ show $ (.name) <$> (interfaces protocol)

  pure []


data Protocol = Protocol {interfaces :: [Interface]}
  deriving (Show)
data Interface = Interface { name :: String }
  deriving (Show)

loadProtocol :: MonadFail m => BS.ByteString -> m Protocol
loadProtocol xml = do
  (Just protocolEl) <- pure $ parseXMLDoc xml
  interfaces <- mapM loadInterface $ findChildren (blank_name { qName = "interface" }) protocolEl
  pure $ Protocol interfaces

loadInterface :: MonadFail m => Element -> m Interface
loadInterface interfaceEl = do
  name <- interfaceName
  pure $ Interface name
  where
    interfaceName :: MonadFail m => m String
    interfaceName = do
      (Just name) <- pure $ findAttr (blank_name { qName = "name" }) interfaceEl
      pure name

