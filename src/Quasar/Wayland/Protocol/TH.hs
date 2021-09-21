module Quasar.Wayland.Protocol.TH (
  generateWaylandProcol
) where

import Control.Monad.STM
import Control.Monad.Writer
import Data.ByteString qualified as BS
import Data.List (intersperse)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (BangType, VarBangType, addDependentFile)
import Prelude qualified
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
  arguments :: [ArgumentSpec],
  isDestructor :: Bool
}
  deriving stock Show

data ArgumentSpec = ArgumentSpec {
  name :: String,
  index :: Integer,
  argType :: ArgumentType,
  nullable :: Bool
}
  deriving stock Show

data ArgumentType
  = IntArgument
  | UIntArgument
  | FixedArgument
  | StringArgument
  | ArrayArgument
  | ObjectArgument String
  | GenericObjectArgument
  | NewIdArgument String
  | GenericNewIdArgument
  | FdArgument
  deriving stock (Eq, Show)

isNewId :: ArgumentType -> Bool
isNewId (NewIdArgument _) = True
isNewId GenericNewIdArgument = True
isNewId _ = False



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
    tellQ requestRecordD
    tellQ eventRecordD
  internals <- execWriterT do
    tellQ $ dataD (pure []) iName [] Nothing [] [derivingInterfaceClient, derivingInterfaceServer]
    tellQ $ instanceD (pure []) [t|IsInterface $iT|] instanceDecs

    when (length interface.requests > 0) do
      tellQs $ messageTypeDecs rTypeName requestContexts

    when (length interface.events > 0) do
      tellQs $ messageTypeDecs eTypeName eventContexts

  pure (public, internals)

  where
    iName = interfaceN interface
    iT = interfaceT interface
    instanceDecs = [
      tySynInstD (tySynEqn Nothing (appT (conT ''Request) iT) rT),
      tySynInstD (tySynEqn Nothing (appT (conT ''Event) iT) eT),
      tySynInstD (tySynEqn Nothing (appT (conT ''InterfaceName) iT) (litT (strTyLit interface.name))),
      valD (varP 'interfaceName) (normalB (stringE interface.name)) []
      ]
    rT :: Q Type
    rT = if length interface.requests > 0 then conT rTypeName else [t|Void|]
    rTypeName :: Name
    rTypeName = mkName $ "WireRequest_" <> interface.name
    rConName :: RequestSpec -> Name
    rConName (RequestSpec request) = mkName $ "WireRequest_" <> interface.name <> "_" <> request.name
    eT :: Q Type
    eT = if length interface.events > 0 then conT eTypeName else [t|Void|]
    eTypeName :: Name
    eTypeName = mkName $ "WireEvent_" <> interface.name
    eConName :: EventSpec -> Name
    eConName (EventSpec event) = mkName $ "WireEvent_" <> interface.name <> "_" <> event.name
    requestContext :: RequestSpec -> MessageContext
    requestContext req@(RequestSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = rT,
      msgConName = rConName req,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }
    requestContexts = requestContext <$> interface.requests
    eventContext :: EventSpec -> MessageContext
    eventContext ev@(EventSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = eT,
      msgConName = eConName ev,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }
    eventContexts = eventContext <$> interface.events

    requestRecordD :: Q Dec
    requestRecordD = messageRecordD (requestClassN interface) requestContexts

    eventRecordD :: Q Dec
    eventRecordD = messageRecordD (eventClassN interface) eventContexts

    messageRecordD :: Name -> [MessageContext] -> Q Dec
    messageRecordD name messageContexts = dataD (cxt []) name [] Nothing [con] []
      where
        con = recC name (recField <$> messageContexts)
        recField :: MessageContext -> Q VarBangType
        recField msg = varDefaultBangType (mkName msg.msgSpec.name) [t|$(applyArgTypes [t|STM ()|])|]
          where
            applyArgTypes :: Q Type -> Q Type
            applyArgTypes xt = foldr (\x y -> [t|$x -> $y|]) xt (argumentType <$> msg.msgSpec.arguments)


interfaceSideInstanceDs :: InterfaceSpec -> Q [Dec]
interfaceSideInstanceDs interface = execWriterT do
  tellQs [d|instance IsInterfaceSide 'Client $iT|]
  tellQs [d|instance IsInterfaceSide 'Server $iT|]
  where
    iT = interfaceT interface


interfaceN :: InterfaceSpec -> Name
interfaceN interface = mkName $ "Interface_" <> interface.name

interfaceT :: InterfaceSpec -> Q Type
interfaceT interface = conT (interfaceN interface)

requestClassN :: InterfaceSpec -> Name
requestClassN interface = mkName $ "Requests_" <> interface.name

requestClassT :: InterfaceSpec -> Q Type
requestClassT interface = conT (requestClassN interface)

eventClassN :: InterfaceSpec -> Name
eventClassN interface = mkName $ "Events_" <> interface.name

eventClassT :: InterfaceSpec -> Q Type
eventClassT interface = conT (eventClassN interface)



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
-- Add an "_" to prevent name conflicts with everything
msgArgTempName arg = mkName $ arg.name <> "_"


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
        showArgE arg = [stringE (arg.name ++ "="), [|showArgument @($(argumentType arg)) $(msgArgE msg arg)|]]

isMessageInstanceD :: Q Type -> [MessageContext] -> Q Dec
isMessageInstanceD t msgs = instanceD (pure []) [t|IsMessage $t|] [opcodeNameD, getMessageD, putMessageD]
  where
    opcodeNameD :: Q Dec
    opcodeNameD = funD 'opcodeName ((opcodeNameClause <$> msgs) <> [opcodeNameInvalidClause])
    opcodeNameClause :: MessageContext -> Q Clause
    opcodeNameClause msg = clause [litP (integerL (fromIntegral msg.msgSpec.opcode))] (normalB ([|Just $(stringE msg.msgSpec.name)|])) []
    opcodeNameInvalidClause :: Q Clause
    opcodeNameInvalidClause = clause [wildP] (normalB ([|Nothing|])) []
    getMessageD :: Q Dec
    getMessageD = funD 'getMessage ((getMessageClause <$> msgs) <> [getMessageInvalidOpcodeClause])
    getMessageClause :: MessageContext -> Q Clause
    getMessageClause msg = clause [wildP, litP (integerL (fromIntegral msg.msgSpec.opcode))] (normalB getMessageE) []
      where
        getMessageE :: Q Exp
        getMessageE = applyALifted (conE (msg.msgConName)) ((\argT -> [|getArgument @($argT)|]) . argumentType <$> msg.msgSpec.arguments)
    getMessageInvalidOpcodeClause :: Q Clause
    getMessageInvalidOpcodeClause = do
      let object = mkName "object"
      let opcode = mkName "opcode"
      clause [varP object, varP opcode] (normalB [|invalidOpcode $(varE object) $(varE opcode)|]) []
    putMessageD :: Q Dec
    putMessageD = funD 'putMessage (putMessageClauseD <$> msgs)
    putMessageClauseD :: MessageContext -> Q Clause
    putMessageClauseD msg = clause [msgConP msg] (normalB (putMessageE msg.msgSpec.arguments)) []
      where
        putMessageE :: [ArgumentSpec] -> Q Exp
        putMessageE args = [|($(litE $ integerL $ fromIntegral msg.msgSpec.opcode), ) <$> $(putMessageBodyE args)|]
        putMessageBodyE :: [ArgumentSpec] -> Q Exp
        putMessageBodyE [] = [|pure []|]
        putMessageBodyE args = [|sequence $(listE ((\arg -> [|putArgument @($(argumentType arg)) $(msgArgE msg arg)|]) <$> args))|]


derivingEq :: Q DerivClause
derivingEq = derivClause (Just StockStrategy) [[t|Eq|]]

derivingShow :: Q DerivClause
derivingShow = derivClause (Just StockStrategy) [[t|Show|]]

derivingInterfaceClient :: Q DerivClause
derivingInterfaceClient = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Client|]]

derivingInterfaceServer :: Q DerivClause
derivingInterfaceServer = derivClause (Just AnyclassStrategy) [[t|IsInterfaceSide 'Server|]]

argumentType :: ArgumentSpec -> Q Type
argumentType argSpec = promoteArgumentType argSpec.argType

promoteArgumentType :: ArgumentType -> Q Type
promoteArgumentType IntArgument = [t|Int32|]
promoteArgumentType UIntArgument = [t|Word32|]
promoteArgumentType FixedArgument = [t|Fixed|]
promoteArgumentType StringArgument = [t|WlString|]
promoteArgumentType ArrayArgument = [t|BS.ByteString|]
promoteArgumentType (ObjectArgument iName) = [t|ObjectId $(litT (strTyLit iName))|]
promoteArgumentType GenericObjectArgument = [t|GenericObjectId|]
promoteArgumentType (NewIdArgument iName) = [t|NewId $(litT (strTyLit iName))|]
promoteArgumentType GenericNewIdArgument = [t|GenericNewId|]
promoteArgumentType FdArgument = [t|Void|] -- TODO

defaultBangType :: Q Type -> Q BangType
defaultBangType = bangType (bang noSourceUnpackedness noSourceStrictness)

varDefaultBangType  :: Name -> Q Type -> Q VarBangType
varDefaultBangType name qType = varBangType name $ bangType (bang noSourceUnpackedness noSourceStrictness) qType


-- | (a -> b -> c -> d) -> [m a, m b, m c] -> m d
applyA :: Q Exp -> [Q Exp] -> Q Exp
applyA con [] = [|pure $con|]
applyA con (monadicE:monadicEs) = foldl (\x y -> [|$x <*> $y|]) [|$con <$> $monadicE|] monadicEs

-- | (a -> b -> c -> m d) -> [m a, m b, m c] -> m d
applyM :: Q Exp -> [Q Exp] -> Q Exp
applyM con [] = con
applyM con args = [|join $(applyA con args)|]


-- | (a -> b -> c -> d) -> [f (g a), f (g b), f (g c)] -> f (g d)
applyALifted :: Q Exp -> [Q Exp] -> Q Exp
applyALifted con [] = [|pure $ pure $con|]
applyALifted con (monadicE:monadicEs) = foldl (\x y -> [|$x <<*>> $y|]) [|$con <<$>> $monadicE|] monadicEs


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
  version <- Prelude.read <$> getAttr "version" element
  requests <- mapM (parseRequest name) $ zip [0..] $ findChildren (qname "request") element
  events <- mapM (parseEvent name) $ zip [0..] $ findChildren (qname "event") element
  pure InterfaceSpec {
    name,
    version,
    requests,
    events
  }

parseRequest :: MonadFail m => String -> (Opcode, Element) -> m RequestSpec
parseRequest x y = RequestSpec <$> parseMessage True x y

parseEvent :: MonadFail m => String -> (Opcode, Element) -> m EventSpec
parseEvent x y = EventSpec <$> parseMessage False x y

parseMessage :: MonadFail m => Bool -> String -> (Opcode, Element) -> m MessageSpec
parseMessage isRequest interface (opcode, element) = do
  let isEvent = not isRequest

  name <- getAttr "name" element

  let description = interface <> "." <> name

  mtype <- peekAttr "type" element
  since <- Prelude.read <<$>> peekAttr "since" element
  arguments <- mapM (parseArgument description) $ zip [0..] $ findChildren (qname "arg") element

  isDestructor <-
    case mtype of
      Nothing -> pure False
      Just "destructor" -> pure True
      Just messageType -> fail $ "Unknown message type: " <> messageType

  when
    do isEvent && isDestructor
    do fail $ "Event cannot be a destructor: " <> description

  when
    do (foldr (\arg -> if isNewId arg.argType then (+ 1) else id) 0 arguments) > (1 :: Int)
    do fail $ "Message creates multiple objects: " <> description

  forM_ arguments \arg -> do
    when
      do arg.argType == GenericNewIdArgument && (interface /= "wl_registry" || name /= "bind")
      do fail $ "Invalid \"new_id\" argument without \"interface\" attribute encountered on " <> description <> " (only valid on wl_registry.bind)"
    when
      do arg.argType == GenericObjectArgument && (interface /= "wl_display" || name /= "error")
      do fail $ "Invalid \"object\" argument without \"interface\" attribute encountered on " <> description <> " (only valid on wl_display.error)"

  pure MessageSpec  {
    name,
    since,
    opcode,
    arguments,
    isDestructor
  }


parseArgument :: forall m. MonadFail m => String -> (Integer, Element) -> m ArgumentSpec
parseArgument messageDescription (index, element) = do
  name <- getAttr "name" element
  argTypeStr <- getAttr "type" element
  interface <- peekAttr "interface" element
  argType <- parseArgumentType argTypeStr interface

  let description = messageDescription <> "." <> name

  nullable <- peekAttr "allow-null" element >>= \case
    Just "true" -> pure True
    Just "false" -> pure False
    Just x -> fail $ "Invalid value for attribute \"allow-null\" on " <> description <> ": " <> x
    Nothing -> pure False
  pure ArgumentSpec {
    name,
    index,
    argType,
    nullable
  }
  where
    parseArgumentType :: String -> Maybe String -> m ArgumentType
    parseArgumentType "int" Nothing = pure IntArgument
    parseArgumentType "uint" Nothing = pure UIntArgument
    parseArgumentType "fixed" Nothing = pure FixedArgument
    parseArgumentType "string" Nothing = pure StringArgument
    parseArgumentType "array" Nothing = pure ArrayArgument
    parseArgumentType "object" (Just interface) = pure (ObjectArgument interface)
    parseArgumentType "object" Nothing = pure GenericObjectArgument
    parseArgumentType "new_id" (Just interface) = pure (NewIdArgument interface)
    parseArgumentType "new_id" Nothing = pure GenericNewIdArgument
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
