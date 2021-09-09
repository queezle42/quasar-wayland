module Quasar.Wayland.Protocol.TH (
  generateWaylandProcol
) where

import Control.Monad.Writer
import Data.Binary
import Data.ByteString qualified as BS
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.Syntax qualified as TH
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Text.XML.Light


data ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock Show

data InterfaceSpec = InterfaceSpec {
  name :: String,
  requests :: [RequestSpec],
  events :: [EventSpec]
}
  deriving stock Show

newtype RequestSpec = RequestSpec MessageSpec
  deriving stock Show

newtype EventSpec = EventSpec MessageSpec
  deriving stock Show

data MessageSpec = MessageSpec {
  name :: String,
  opcode :: Opcode,
  arguments :: [ArgumentSpec]
}
  deriving stock Show

data ArgumentSpec = ArgumentSpec {
  name :: String,
  argType :: ArgumentType
}
  deriving stock Show


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
  tellQ $ dataD (pure []) iName [] Nothing [normalC iName []] [derivingInterfaceClient, derivingInterfaceServer]
  tellQ $ instanceD (pure []) [t|IsInterface $iT|] instanceDecs

  when (length interface.requests > 0) do
    tellQ $ dataD (pure []) rTypeName [] Nothing (rCon <$> interface.requests) []
    tellQ $ messageInstanceD rT ((\req@(RequestSpec msg) -> (msg, rConName req)) <$> interface.requests)

  when (length interface.events > 0) do
    tellQ $ dataD (pure []) eTypeName [] Nothing (eCon <$> interface.events) []
    tellQ $ messageInstanceD eT ((\ev@(EventSpec msg) -> (msg, eConName ev)) <$> interface.events)

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
messageInstanceD t messages = instanceD (pure []) [t|IsMessage $t|] [opcodeNameD, showMessageD, getMessageD, putMessageD]
  where
    opcodeNameD :: Q Dec
    opcodeNameD = funD 'opcodeName (opcodeNameClauseD <$> messages)
    opcodeNameClauseD :: (MessageSpec, Name) -> Q Clause
    opcodeNameClauseD (msg, conName) = clause [litP (integerL (fromIntegral msg.opcode))] (normalB ([|Just $(stringE msg.name)|])) []
    showMessageD :: Q Dec
    showMessageD = funD 'showMessage (showMessageClauseD <$> messages)
    showMessageClauseD :: (MessageSpec, Name) -> Q Clause
    showMessageClauseD (msg, conName) = clause [conP conName []] (normalB (stringE msg.name)) []
    getMessageD :: Q Dec
    getMessageD = funD 'getMessage (getMessageClauseD <$> messages)
    getMessageClauseD :: (MessageSpec, Name) -> Q Clause
    getMessageClauseD (msg, conName) = clause [[p|_object|], litP (integerL (fromIntegral msg.opcode))] (normalB ([|$(conE conName) <$ dropRemaining|])) []
    putMessageD :: Q Dec
    putMessageD = funD 'putMessage [clause [] (normalB [|undefined|]) []]

interfaceN :: InterfaceSpec -> Name
interfaceN interface = mkName $ "I_" <> interface.name

interfaceT :: InterfaceSpec -> Q Type
interfaceT interface = conT (interfaceN interface)

derivingShow :: Q DerivClause
derivingShow = derivClause (Just StockStrategy) [[t|Show|]]

derivingInterfaceClient :: Q DerivClause
derivingInterfaceClient = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Client|]]

derivingInterfaceServer :: Q DerivClause
derivingInterfaceServer = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Server|]]

promoteArgumentType :: ArgumentType -> Q Type
promoteArgumentType arg = do
  argExp <- (TH.lift arg)
  ConT <$> matchCon argExp
  where
    matchCon :: Exp -> Q Name
    matchCon (ConE name) = pure name
    matchCon _ = fail "Can only promote ConE expression"


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
parseRequest x = RequestSpec <$> parseMessage x

parseEvent :: MonadFail m => (Opcode, Element) -> m EventSpec
parseEvent x = EventSpec <$> parseMessage x

parseMessage :: MonadFail m => (Opcode, Element) -> m MessageSpec
parseMessage (opcode, element) = do
  name <- getAttr "name" element
  arguments <- mapM parseArgument $ findChildren (qname "arg") element
  pure MessageSpec  {
    name,
    opcode,
    arguments
  }


parseArgument :: forall m. MonadFail m => Element -> m ArgumentSpec
parseArgument element = do
  name <- getAttr "name" element
  argTypeStr <- getAttr "type" element
  interface <- peekAttr "interface" element
  argType <- parseArgumentType argTypeStr interface
  pure ArgumentSpec {
    name,
    argType
  }
  where
    parseArgumentType :: String -> Maybe String -> m ArgumentType
    parseArgumentType "int" Nothing = pure IntArgument
    parseArgumentType "uint" Nothing = pure UIntArgument
    parseArgumentType "fixed" Nothing = pure FixedArgument
    parseArgumentType "string" Nothing = pure StringArgument
    parseArgumentType "array" Nothing = pure ArrayArgument
    parseArgumentType "object" (Just interface) = pure (ObjectArgument interface)
    parseArgumentType "object" Nothing = pure UnknownObjectArgument
    parseArgumentType "new_id" (Just interface) = pure (NewIdArgument interface)
    parseArgumentType "new_id" Nothing = pure UnknownNewIdArgument
    parseArgumentType "fd" Nothing = pure FdArgument
    parseArgumentType x Nothing = fail $ "Unknown argument type \"" <> x <> "\" encountered"
    parseArgumentType x _ = fail $ "Argument type \"" <> x <> "\" should not have \"interface\" attribute"


qname :: String -> QName
qname name = blank_name { qName = name }

getAttr :: MonadFail m => String -> Element -> m String
getAttr name element = do
  (Just value) <- pure $ findAttr (qname name) element
  pure value

peekAttr :: Applicative m => String -> Element -> m (Maybe String)
peekAttr name element = pure $ findAttr (qname name) element
