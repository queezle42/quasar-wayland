module Quasar.Wayland.TH (
  generateWaylandProcol
) where

import Control.Monad.Writer
import Data.Binary
import Data.ByteString qualified as BS
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (addDependentFile)
import Quasar.Prelude
import Quasar.Wayland.Core
import Text.XML.Light


data ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock (Show)

data InterfaceSpec = InterfaceSpec {
  name :: String,
  requests :: [RequestSpec],
  events :: [EventSpec]
}
  deriving stock (Show)

newtype RequestSpec = RequestSpec MessageSpec
  deriving stock (Show)

newtype EventSpec = EventSpec MessageSpec
  deriving stock (Show)

data MessageSpec = MessageSpec {
  name :: String,
  opcode :: Opcode
}
  deriving stock (Show)


generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- parseProtocol xml

  traceIO $ show $ interfaces protocol

  concat <$> mapM interfaceDec protocol.interfaces


tellQ :: Q a -> WriterT [a] Q ()
tellQ action = tell =<< lift (singleton <$> action)
  where
    -- TODO use from base (base-4.14.0.0)
    singleton :: a -> [a]
    singleton x = [x]

tellQs :: Q [a] -> WriterT [a] Q ()
tellQs = tell <=< lift

interfaceDec :: InterfaceSpec -> Q [Dec]
interfaceDec interface = execWriterT do
  tellQ $ dataD (pure []) iName [] Nothing [normalC iName []] []
  tellQ $ instanceD (pure []) [t|IsInterface $iT|] instanceDecs

  when (length interface.requests > 0) do
    tellQ $ dataD (pure []) rTypeName [] Nothing (rCon <$> interface.requests) []
    tellQ $ messageInstanceD rT ((\req@(RequestSpec msg) -> (msg, rConName req)) <$> interface.requests)
    tellQs $ binaryInstanceD rT

  when (length interface.events > 0) do
    tellQ $ dataD (pure []) eTypeName [] Nothing (eCon <$> interface.events) []
    tellQ $ messageInstanceD eT ((\ev@(EventSpec msg) -> (msg, eConName ev)) <$> interface.events)
    tellQs $ binaryInstanceD eT

  where
    iName = interfaceN interface
    iT = interfaceT interface
    instanceDecs = [
      valD (varP 'interfaceName) (normalB (stringE interface.name)) [],
      tySynInstD (tySynEqn Nothing (appT (conT ''Request) iT) rT),
      tySynInstD (tySynEqn Nothing (appT (conT ''Event) iT) eT)
      ]
    rT :: Q Type
    rT = if length interface.requests > 0 then conT rTypeName else [t|Void|]
    rTypeName :: Name
    rTypeName = mkName $ "R_" <> interface.name
    rConName :: RequestSpec -> Name
    rConName (RequestSpec request) = mkName $ "R_" <> interface.name <> "_" <> request.name
    rCon :: RequestSpec -> Q Con
    rCon request = normalC (rConName request) []
    eT :: Q Type
    eT = if length interface.events > 0 then conT eTypeName else [t|Void|]
    eTypeName :: Name
    eTypeName = mkName $ "E_" <> interface.name
    eConName :: EventSpec -> Name
    eConName (EventSpec event) = mkName $ "E_" <> interface.name <> "_" <> event.name
    eCon :: EventSpec -> Q Con
    eCon event = normalC (eConName event) []
    messageInstanceD :: Q Type -> [(MessageSpec, Name)] -> Q Dec
    messageInstanceD t messages = instanceD (pure []) [t|IsMessage $t|] messageNameD
      where
        messageNameD :: [Q Dec]
        messageNameD =
          if length messages > 0
            then [funD 'messageName (messageNameInstanceClauseD <$> messages)]
            else []
        messageNameInstanceClauseD :: (MessageSpec, Name) -> Q Clause
        messageNameInstanceClauseD (msg, conName) = clause [conP conName []] (normalB (stringE msg.name)) []
    binaryInstanceD :: Q Type -> Q [Dec]
    binaryInstanceD mT = [d|instance Binary $mT where {get = undefined; put = undefined}|]

interfaceN :: InterfaceSpec -> Name
interfaceN interface = mkName $ "I_" <> interface.name

interfaceT :: InterfaceSpec -> Q Type
interfaceT interface = conT (interfaceN interface)

derivingShow :: Q DerivClause
derivingShow = derivClause (Just StockStrategy) [[t|Show|]]




-- * XML parser

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
  events <- mapM parseEvent $ zip [0..] $ findChildren (qname "event") element
  pure InterfaceSpec {
    name,
    requests,
    events
  }

parseRequest :: MonadFail m => (Opcode, Element) -> m RequestSpec
parseRequest (opcode, element) = do
  name <- getAttr "name" element
  pure $ RequestSpec MessageSpec {
    name,
    opcode
  }

parseEvent :: MonadFail m => (Opcode, Element) -> m EventSpec
parseEvent (opcode, element) = do
  name <- getAttr "name" element
  pure $ EventSpec MessageSpec  {
    name,
    opcode
  }

qname :: String -> QName
qname name = blank_name { qName = name }

getAttr :: MonadFail m => String -> Element -> m String
getAttr name element = do
  (Just value) <- pure $ findAttr (qname name) element
  pure value
