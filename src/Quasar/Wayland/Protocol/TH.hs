module Quasar.Wayland.Protocol.TH (
  generateWaylandProcol
) where

import Control.Monad.Writer
import Data.Binary
import Data.ByteString qualified as BS
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (BangType, VarBangType, addDependentFile)
import Language.Haskell.TH.Syntax qualified as TH
import Data.List (intersperse)
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Text.XML.Light


data ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock Show

data InterfaceSpec = InterfaceSpec {
  name :: String,
  version :: Integer,
  requests :: [RequestSpec],
  events :: [EventSpec]
}
  deriving stock Show

newtype RequestSpec = RequestSpec {messageSpec :: MessageSpec}
  deriving stock Show

newtype EventSpec = EventSpec {messageSpec :: MessageSpec}
  deriving stock Show

data MessageSpec = MessageSpec {
  name :: String,
  since :: Maybe Integer,
  opcode :: Opcode,
  arguments :: [ArgumentSpec]
}
  deriving stock Show

data ArgumentSpec = ArgumentSpec {
  name :: String,
  index :: Integer,
  argType :: ArgumentType
}
  deriving stock Show


generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- parseProtocol xml
  (public, internals) <- unzip <$> mapM interfaceDecs protocol.interfaces
  pure $ mconcat public <> mconcat internals


tellQ :: Q a -> WriterT [a] Q ()
tellQ action = tell =<< lift (singleton <$> action)
  where
    -- TODO use from base (base-4.14.0.0)
    singleton :: a -> [a]
    singleton x = [x]

tellQs :: Q [a] -> WriterT [a] Q ()
tellQs = tell <=< lift

interfaceDecs :: InterfaceSpec -> Q ([Dec], [Dec])
interfaceDecs interface = do
  public <- execWriterT do
    pure ()
  internals <- execWriterT do
    tellQ $ dataD (pure []) iName [] Nothing [normalC iName []] [derivingInterfaceClient, derivingInterfaceServer]
    tellQ $ instanceD (pure []) [t|IsInterface $iT|] instanceDecs

    when (length interface.requests > 0) do
      tellQs $ messageTypeDecs rTypeName (requestContext <$> interface.requests)

    when (length interface.events > 0) do
      tellQs $ messageTypeDecs eTypeName (eventContext <$> interface.events)

  pure (public, internals)

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
    rTypeName = mkName $ "Request_" <> interface.name
    rConName :: RequestSpec -> Name
    rConName (RequestSpec request) = mkName $ "Request_" <> interface.name <> "_" <> request.name
    eT :: Q Type
    eT = if length interface.events > 0 then conT eTypeName else [t|Void|]
    eTypeName :: Name
    eTypeName = mkName $ "Event_" <> interface.name
    eConName :: EventSpec -> Name
    eConName (EventSpec event) = mkName $ "Event_" <> interface.name <> "_" <> event.name
    requestContext :: RequestSpec -> MessageContext
    requestContext req@(RequestSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = rT,
      msgConName = rConName req,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }
    eventContext :: EventSpec -> MessageContext
    eventContext ev@(EventSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = eT,
      msgConName = eConName ev,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }


data MessageContext = MessageContext {
  msgInterfaceT :: Q Type,
  msgT :: Q Type,
  msgConName :: Name,
  msgInterfaceSpec :: InterfaceSpec,
  msgSpec :: MessageSpec
}

-- | Pattern to match a message. Arguments can then be accessed by using 'msgArgE'.
msgConP :: MessageContext -> Q Pat
msgConP msg = conP msg.msgConName (varP . msgArgTempName <$> msg.msgSpec.arguments)

-- | Expression for accessing a message argument which has been matched from a request/event using 'msgArgConP'.
msgArgE :: MessageContext -> ArgumentSpec -> Q Exp
msgArgE _msg arg = varE (msgArgTempName arg)

-- | Helper for 'msgConP' and 'msgArgE'.
msgArgTempName :: ArgumentSpec -> Name
msgArgTempName = mkName . ("x" <>) . show . (.index)

messageTypeDecs :: Name -> [MessageContext] -> Q [Dec]
messageTypeDecs name msgs = execWriterT do
  tellQ $ messageTypeD
  tellQ $ isMessageInstanceD t msgs
  tellQ $ showInstanceD
  where
    t :: Q Type
    t = conT name
    messageTypeD :: Q Dec
    messageTypeD = dataD (pure []) name [] Nothing (con <$> msgs) [derivingEq]
    con :: MessageContext -> Q Con
    con msg = normalC (msg.msgConName) (conField <$> msg.msgSpec.arguments)
      where
        conField :: ArgumentSpec -> Q BangType
        conField arg = defaultBangType (argumentType arg)
    showInstanceD :: Q Dec
    showInstanceD = instanceD (pure []) [t|Show $t|] [showD]
    showD :: Q Dec
    showD = funD 'show (showClause <$> msgs)
    showClause :: MessageContext -> Q Clause
    showClause msg =
      clause
        [msgConP msg]
        (normalB [|mconcat $(listE ([stringE (msg.msgSpec.name ++ "(")] <> mconcat (intersperse [stringE ", "] (showArgE <$> msg.msgSpec.arguments) <> [[stringE ")"]])))|])
        []
      where
        showArgE :: ArgumentSpec -> [Q Exp]
        showArgE arg = [stringE (arg.name ++ "="), [|showArgument @($(argumentSpecType arg)) $(msgArgE msg arg)|]]

isMessageInstanceD :: Q Type -> [MessageContext] -> Q Dec
isMessageInstanceD t msgs = instanceD (pure []) [t|IsMessage $t|] [opcodeNameD, getMessageD, putMessageD]
  where
    opcodeNameD :: Q Dec
    opcodeNameD = funD 'opcodeName (opcodeNameClauseD <$> msgs)
    opcodeNameClauseD :: MessageContext -> Q Clause
    opcodeNameClauseD msg = clause [litP (integerL (fromIntegral msg.msgSpec.opcode))] (normalB ([|Just $(stringE msg.msgSpec.name)|])) []
    getMessageD :: Q Dec
    getMessageD = funD 'getMessage (getMessageClauseD <$> msgs)
    getMessageClauseD :: MessageContext -> Q Clause
    getMessageClauseD msg = clause [wildP, litP (integerL (fromIntegral msg.msgSpec.opcode))] (normalB getMessageE) []
      where
        getMessageE :: Q Exp
        getMessageE = applyA (conE (msg.msgConName)) ((\argT -> [|getArgument @($argT)|]) . argumentSpecType <$> msg.msgSpec.arguments)
    putMessageD :: Q Dec
    putMessageD = funD 'putMessage (putMessageClauseD <$> msgs)
    putMessageClauseD :: MessageContext -> Q Clause
    putMessageClauseD msg = clause [msgConP msg] (normalB (putMessageE msg.msgSpec.arguments)) []
      where
        putMessageE :: [ArgumentSpec] -> Q Exp
        putMessageE [] = [|pure ()|]
        putMessageE args = doE ((\arg -> noBindS [|putArgument @($(argumentSpecType arg)) $(msgArgE msg arg)|]) <$> args)


interfaceN :: InterfaceSpec -> Name
interfaceN interface = mkName $ "I_" <> interface.name

interfaceT :: InterfaceSpec -> Q Type
interfaceT interface = conT (interfaceN interface)

derivingEq :: Q DerivClause
derivingEq = derivClause (Just StockStrategy) [[t|Eq|]]

derivingShow :: Q DerivClause
derivingShow = derivClause (Just StockStrategy) [[t|Show|]]

derivingInterfaceClient :: Q DerivClause
derivingInterfaceClient = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Client|]]

derivingInterfaceServer :: Q DerivClause
derivingInterfaceServer = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Server|]]

argumentType :: ArgumentSpec -> Q Type
argumentType argSpec = [t|Argument $(promoteArgumentSpecType argSpec.argType)|]

argumentSpecType :: ArgumentSpec -> Q Type
argumentSpecType argSpec = promoteArgumentSpecType argSpec.argType

promoteArgumentSpecType :: ArgumentType -> Q Type
promoteArgumentSpecType arg = do
  argExp <- (TH.lift arg)
  ConT <$> matchCon argExp
  where
    matchCon :: Exp -> Q Name
    matchCon (ConE name) = pure name
    matchCon (AppE x _) = matchCon x
    matchCon _ = fail "Can only promote ConE expression"

defaultBangType :: Q Type -> Q BangType
defaultBangType = bangType (bang noSourceUnpackedness noSourceStrictness)


-- | (a -> b -> c -> d) -> [m a, m b, m c] -> m d
applyA :: Q Exp -> [Q Exp] -> Q Exp
applyA con [] = [|pure $con|]
applyA con (monadicE:monadicEs) = foldl (\x y -> [|$x <*> $y|]) [|$con <$> $monadicE|] monadicEs

-- | (a -> b -> c -> m d) -> [m a, m b, m c] -> m d
applyM :: Q Exp -> [Q Exp] -> Q Exp
applyM con [] = con
applyM con args = [|join $(applyA con args)|]


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
  version <- read <$> getAttr "version" element
  requests <- mapM parseRequest $ zip [0..] $ findChildren (qname "request") element
  events <- mapM parseEvent $ zip [0..] $ findChildren (qname "event") element
  pure InterfaceSpec {
    name,
    version,
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
  since <- read <<$>> peekAttr "since" element
  arguments <- mapM parseArgument $ zip [0..] $ findChildren (qname "arg") element
  pure MessageSpec  {
    name,
    since,
    opcode,
    arguments
  }


parseArgument :: forall m. MonadFail m => (Integer, Element) -> m ArgumentSpec
parseArgument (index, element) = do
  name <- getAttr "name" element
  argTypeStr <- getAttr "type" element
  interface <- peekAttr "interface" element
  argType <- parseArgumentType argTypeStr interface
  pure ArgumentSpec {
    name,
    index,
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
