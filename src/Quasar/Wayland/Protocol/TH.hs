module Quasar.Wayland.Protocol.TH (
  generateWaylandProcol
) where

import Control.Monad.STM
import Control.Monad.Writer
import Data.ByteString qualified as BS
import Data.List (intersperse, singleton)
import Data.Void (absurd)
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (BangType, VarBangType, addDependentFile)
import Prelude qualified
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Text.XML.Light


data ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock Show

data DescriptionSpec = DescriptionSpec {
  summary :: Maybe String,
  content :: Maybe String
}
  deriving stock Show

data InterfaceSpec = InterfaceSpec {
  name :: String,
  version :: Integer,
  description :: Maybe DescriptionSpec,
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
  description :: Maybe DescriptionSpec,
  opcode :: Opcode,
  arguments :: [ArgumentSpec],
  isDestructor :: Bool
}
  deriving stock Show

data ArgumentSpec = ArgumentSpec {
  name :: String,
  index :: Integer,
  summary :: Maybe String,
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


toWlDoc :: Maybe DescriptionSpec -> Maybe String
toWlDoc (Just DescriptionSpec{content = Just x}) = Just x
toWlDoc (Just DescriptionSpec{summary = Just x}) = Just x
toWlDoc _ = Nothing

withWlDoc :: Maybe DescriptionSpec -> Q Dec -> Q Dec
withWlDoc (toWlDoc -> Just doc) = withDecDoc doc
withWlDoc _ = id


generateWaylandProcol :: FilePath -> Q [Dec]
generateWaylandProcol protocolFile = do
  addDependentFile protocolFile
  xml <- liftIO (BS.readFile protocolFile)
  protocol <- parseProtocol xml
  (public, internals) <- unzip <$> mapM interfaceDecs protocol.interfaces
  pure $ mconcat public <> mconcat internals


tellQ :: Q a -> WriterT [a] Q ()
tellQ action = tell =<< lift (singleton <$> action)

tellQs :: Q [a] -> WriterT [a] Q ()
tellQs = tell <=< lift


interfaceDecs :: InterfaceSpec -> Q ([Dec], [Dec])
interfaceDecs interface = do
  public <- execWriterT do
    -- Main interface type
    tellQ $ dataD_doc (pure []) iName [] Nothing [] [] (toWlDoc interface.description)
    -- IsInterface instance
    tellQ $ instanceD (pure []) [t|IsInterface $iT|] [
      tySynInstD (tySynEqn Nothing [t|$(conT ''RequestHandler) $iT|] (orUnit (requestsT interface))),
      tySynInstD (tySynEqn Nothing [t|$(conT ''EventHandler) $iT|] (orUnit (eventsT interface))),
      tySynInstD (tySynEqn Nothing (appT (conT ''WireRequest) iT) wireRequestT),
      tySynInstD (tySynEqn Nothing (appT (conT ''WireEvent) iT) wireEventT),
      tySynInstD (tySynEqn Nothing (appT (conT ''InterfaceName) iT) (litT (strTyLit interface.name)))
      ]
    -- | IsInterfaceSide instance
    tellQs interfaceSideInstanceDs

    when (length interface.requests > 0) do
      -- | Requests record
      tellQ requestCallbackRecordD
      -- | Request proxies
      tellQs requestProxyInstanceDecs

    when (length interface.events > 0) do
      -- | Events record
      tellQ eventCallbackRecordD
      -- | Event proxies
      tellQs eventProxyInstanceDecs

  internals <- execWriterT do
    -- | Request wire type
    when (length interface.requests > 0) do
      tellQs $ messageTypeDecs rTypeName wireRequestContexts

    -- | Event wire type
    when (length interface.events > 0) do
      tellQs $ messageTypeDecs eTypeName wireEventContexts

  pure (public, internals)

  where
    iName = interfaceN interface
    iT = interfaceT interface
    sT = sideTVar
    wireRequestT :: Q Type
    wireRequestT = if length interface.requests > 0 then conT rTypeName else [t|Void|]
    rTypeName :: Name
    rTypeName = mkName $ "WireRequest_" <> interface.name
    rConName :: RequestSpec -> Name
    rConName (RequestSpec request) = mkName $ "WireRequest_" <> interface.name <> "__" <> request.name
    wireEventT :: Q Type
    wireEventT = if length interface.events > 0 then conT eTypeName else [t|Void|]
    eTypeName :: Name
    eTypeName = mkName $ "WireEvent_" <> interface.name
    eConName :: EventSpec -> Name
    eConName (EventSpec event) = mkName $ "WireEvent_" <> interface.name <> "__" <> event.name
    wireRequestContext :: RequestSpec -> MessageContext
    wireRequestContext req@(RequestSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = wireRequestT,
      msgConName = rConName req,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }
    wireRequestContexts = wireRequestContext <$> interface.requests
    wireEventContext :: EventSpec -> MessageContext
    wireEventContext ev@(EventSpec msgSpec) = MessageContext {
      msgInterfaceT = iT,
      msgT = wireEventT,
      msgConName = eConName ev,
      msgInterfaceSpec = interface,
      msgSpec = msgSpec
    }
    wireEventContexts = wireEventContext <$> interface.events

    requestCallbackRecordD :: Q Dec
    requestCallbackRecordD = messageRecordD (requestsName interface) wireRequestContexts

    requestProxyInstanceDecs :: Q [Dec]
    requestProxyInstanceDecs = messageProxyInstanceDecs [t|'Client|] wireRequestContexts

    eventCallbackRecordD :: Q Dec
    eventCallbackRecordD = messageRecordD (eventsName interface) wireEventContexts

    eventProxyInstanceDecs :: Q [Dec]
    eventProxyInstanceDecs = messageProxyInstanceDecs [t|'Server|] wireEventContexts

    objectName = mkName "object"
    objectP = varP objectName
    objectE = varE objectName

    interfaceSideInstanceDs :: Q [Dec]
    interfaceSideInstanceDs = execWriterT do
      tellQ $ instanceD (pure []) ([t|IsInterfaceSide 'Client $iT|]) [handleMessageD Client]
      tellQ $ instanceD (pure []) ([t|IsInterfaceSide 'Server $iT|]) [handleMessageD Server]

    handleMessageD :: Side -> Q Dec
    handleMessageD Client = funD 'objectHandleMessage (handleMessageClauses wireEventContexts)
    handleMessageD Server = funD 'objectHandleMessage (handleMessageClauses wireRequestContexts)

    handleMessageClauses :: [MessageContext] -> [Q Clause]
    handleMessageClauses [] = [clause [wildP] (normalB [|absurd|]) []]
    handleMessageClauses messageContexts = handleMessageClause <$> messageContexts

    handleMessageClause :: MessageContext -> Q Clause
    handleMessageClause msg = clause [objectP, msgConP msg] (normalB bodyE) []
      where
        fieldNameLitT :: Q Type
        fieldNameLitT = litT (strTyLit (messageFieldNameString msg))
        fieldE :: Q Exp
        fieldE = [|$(appTypeE [|getField|] fieldNameLitT) $objectE.messageHandler|]
        bodyE :: Q Exp
        bodyE = applyMsgArgs msg fieldE

messageProxyInstanceDecs :: Q Type -> [MessageContext] -> Q [Dec]
messageProxyInstanceDecs sideT messageContexts = mapM messageProxyInstanceD messageContexts
  where
    messageProxyInstanceD :: MessageContext -> Q Dec
    messageProxyInstanceD msg = instanceD (pure []) instanceT [
      funD 'getField [clause ([varP objectName] <> msgArgPats msg) (normalB [|objectSendMessage object $(msgE msg)|]) []]
      ]
      where
        objectName = mkName "object"
        instanceT :: Q Type
        instanceT = [t|HasField $(litT (strTyLit msg.msgSpec.name)) $objectT $proxyT|]
        objectT :: Q Type
        objectT = [t|Object $sideT $(msg.msgInterfaceT)|]
        proxyT :: Q Type
        proxyT = [t|$(applyArgTypes [t|STM ()|])|]
        applyArgTypes :: Q Type -> Q Type
        applyArgTypes xt = foldr (\x y -> [t|$x -> $y|]) xt (argumentType <$> msg.msgSpec.arguments)


messageFieldName :: MessageContext -> Name
messageFieldName msg = mkName $ messageFieldNameString msg

messageFieldNameString :: MessageContext -> String
messageFieldNameString msg = msg.msgSpec.name

messageRecordD :: Name -> [MessageContext] -> Q Dec
messageRecordD name messageContexts = dataD (cxt []) name [] Nothing [con] []
  where
    con = recC name (recField <$> messageContexts)
    recField :: MessageContext -> Q VarBangType
    recField msg = varDefaultBangType (messageFieldName msg) [t|$(applyArgTypes [t|STM ()|])|]
      where
        applyArgTypes :: Q Type -> Q Type
        applyArgTypes xt = foldr (\x y -> [t|$x -> $y|]) xt (argumentType <$> msg.msgSpec.arguments)


sideTVarName :: Name
sideTVarName = mkName "s"
sideTVar :: Q Type
sideTVar = varT sideTVarName

interfaceN :: InterfaceSpec -> Name
interfaceN interface = mkName $ "Interface_" <> interface.name

interfaceT :: InterfaceSpec -> Q Type
interfaceT interface = conT (interfaceN interface)

interfaceTFromName :: String -> Q Type
interfaceTFromName name = conT (mkName ("Interface_" <> name))

requestsName :: InterfaceSpec -> Name
requestsName interface = mkName $ "RequestHandler_" <> interface.name

requestsT :: InterfaceSpec -> Maybe (Q Type)
requestsT interface = if (length interface.requests) > 0 then Just [t|$(conT (requestsName interface))|] else Nothing

eventsName :: InterfaceSpec -> Name
eventsName interface = mkName $ "EventHandler_" <> interface.name

eventsT :: InterfaceSpec -> Maybe (Q Type)
eventsT interface = if (length interface.events) > 0 then Just [t|$(conT (eventsName interface))|] else Nothing

orVoid :: Maybe (Q Type) -> Q Type
orVoid = fromMaybe [t|Void|]

orUnit :: Maybe (Q Type) -> Q Type
orUnit = fromMaybe [t|()|]



data MessageContext = MessageContext {
  msgInterfaceT :: Q Type,
  msgT :: Q Type,
  msgConName :: Name,
  msgInterfaceSpec :: InterfaceSpec,
  msgSpec :: MessageSpec
}

-- | Pattern to match a message. Arguments can then be accessed by using 'msgArgE'.
msgConP :: MessageContext -> Q Pat
msgConP msg = conP msg.msgConName (msgArgPats msg)

-- | Pattern to match all arguments of a message. Arguments can then be accessed by using e.g. 'msgArgE'.
msgArgPats :: MessageContext -> [Q Pat]
msgArgPats msg = varP . msgArgTempName <$> msg.msgSpec.arguments

-- | Expression for accessing a message argument which has been matched from a request/event using 'msgArgConP'.
msgArgE :: MessageContext -> ArgumentSpec -> Q Exp
msgArgE _msg arg = varE (msgArgTempName arg)

-- | Helper for 'msgConP' and 'msgArgE'.
msgArgTempName :: ArgumentSpec -> Name
-- Add a prefix to prevent name conflicts with exports from the Prelude
msgArgTempName arg = mkName $ "arg_" <> arg.name

applyMsgArgs :: MessageContext -> Q Exp -> Q Exp
applyMsgArgs msg base = foldl appE base (msgArgE msg <$> msg.msgSpec.arguments)

-- | Expression to construct a wire message with arguments which have been matched using 'msgConP'/'msgArgPats'.
msgE :: MessageContext -> Q Exp
msgE msg = applyMsgArgs msg (conE msg.msgConName)


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
        conField arg = defaultBangType (argumentWireType arg)
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
        showArgE arg = [stringE (arg.name ++ "="), [|showArgument @($(argumentWireType arg)) $(msgArgE msg arg)|]]

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
        getMessageE = applyALifted (conE (msg.msgConName)) ((\argT -> [|getArgument @($argT)|]) . argumentWireType <$> msg.msgSpec.arguments)
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
        putMessageBodyE args = [|sequence $(listE ((\arg -> [|putArgument @($(argumentWireType arg)) $(msgArgE msg arg)|]) <$> args))|]


derivingEq :: Q DerivClause
derivingEq = derivClause (Just StockStrategy) [[t|Eq|]]

derivingShow :: Q DerivClause
derivingShow = derivClause (Just StockStrategy) [[t|Show|]]

-- | Map an argument to its high-level api type
argumentType :: ArgumentSpec -> Q Type
argumentType argSpec = liftArgumentType argSpec.argType

liftArgumentType :: ArgumentType -> Q Type
--liftArgumentType (ObjectArgument iName) = [t|Object $sideTVar $(interfaceTFromName iName)|]
liftArgumentType x = liftArgumentWireType x


-- | Map an argument to its wire representation type
argumentWireType :: ArgumentSpec -> Q Type
argumentWireType argSpec = liftArgumentWireType argSpec.argType

liftArgumentWireType :: ArgumentType -> Q Type
liftArgumentWireType IntArgument = [t|Int32|]
liftArgumentWireType UIntArgument = [t|Word32|]
liftArgumentWireType FixedArgument = [t|Fixed|]
liftArgumentWireType StringArgument = [t|WlString|]
liftArgumentWireType ArrayArgument = [t|BS.ByteString|]
liftArgumentWireType (ObjectArgument iName) = [t|ObjectId $(litT (strTyLit iName))|]
liftArgumentWireType GenericObjectArgument = [t|GenericObjectId|]
liftArgumentWireType (NewIdArgument iName) = [t|NewId $(litT (strTyLit iName))|]
liftArgumentWireType GenericNewIdArgument = [t|GenericNewId|]
liftArgumentWireType FdArgument = [t|Void|] -- TODO


-- * Generic TH utilities

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


buildTupleType :: Q [Type] -> Q Type
buildTupleType fields = buildTupleType' =<< fields
  where
    buildTupleType' :: [Type] -> Q Type
    buildTupleType' [] = [t|()|]
    buildTupleType' [single] = pure single
    buildTupleType' fs = pure $ go (TupleT (length fs)) fs
    go :: Type -> [Type] -> Type
    go t [] = t
    go t (f:fs) = go (AppT t f) fs


-- * XML parser

parseProtocol :: MonadFail m => BS.ByteString -> m ProtocolSpec
parseProtocol xml = do
  (Just element) <- pure $ parseXMLDoc xml
  interfaces <- mapM parseInterface $ findChildren (blank_name { qName = "interface" }) element
  pure ProtocolSpec {
    interfaces
  }

parseDescription :: MonadFail m => Element -> m DescriptionSpec
parseDescription element = do
  let
    summary = findAttr (qname "summary") element
  content <- case element.elContent of
    [Text CData{cdVerbatim=CDataText, cdData=content}] -> pure $ Just content
    [] -> pure Nothing
    x -> fail $ "Cannot parse description xml: " <> show element
  pure DescriptionSpec {
    summary,
    content
  }

-- | Find the description node on an element and convert it to a `DescriptionSpec`.
findDescription :: MonadFail m => Element -> m (Maybe DescriptionSpec)
findDescription element = do
  case findChildren (qname "description") element of
    [] -> pure Nothing
    [descriptionElement] -> Just <$> parseDescription descriptionElement
    _ -> fail "Element has more than one description"


parseInterface :: MonadFail m => Element -> m InterfaceSpec
parseInterface element = do
  name <- getAttr "name" element
  version <- Prelude.read <$> getAttr "version" element
  description <- findDescription element
  requests <- mapM (parseRequest name) $ zip [0..] $ findChildren (qname "request") element
  events <- mapM (parseEvent name) $ zip [0..] $ findChildren (qname "event") element
  pure InterfaceSpec {
    name,
    version,
    description,
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

  let location = interface <> "." <> name

  mtype <- peekAttr "type" element
  since <- Prelude.read <<$>> peekAttr "since" element
  description <- findDescription element
  arguments <- mapM (parseArgument location) $ zip [0..] $ findChildren (qname "arg") element

  isDestructor <-
    case mtype of
      Nothing -> pure False
      Just "destructor" -> pure True
      Just messageType -> fail $ "Unknown message type: " <> messageType

  when
    do isEvent && isDestructor
    do fail $ "Event cannot be a destructor: " <> location

  when
    do (foldr (\arg -> if isNewId arg.argType then (+ 1) else id) 0 arguments) > (1 :: Int)
    do fail $ "Message creates multiple objects: " <> location

  forM_ arguments \arg -> do
    when
      do arg.argType == GenericNewIdArgument && (interface /= "wl_registry" || name /= "bind")
      do fail $ "Invalid \"new_id\" argument without \"interface\" attribute encountered on " <> location <> " (only valid on wl_registry.bind)"
    when
      do arg.argType == GenericObjectArgument && (interface /= "wl_display" || name /= "error")
      do fail $ "Invalid \"object\" argument without \"interface\" attribute encountered on " <> location <> " (only valid on wl_display.error)"

  pure MessageSpec  {
    name,
    since,
    description,
    opcode,
    arguments,
    isDestructor
  }


parseArgument :: forall m. MonadFail m => String -> (Integer, Element) -> m ArgumentSpec
parseArgument messageDescription (index, element) = do
  name <- getAttr "name" element
  summary <- peekAttr "summary" element
  argTypeStr <- getAttr "type" element
  interface <- peekAttr "interface" element
  argType <- parseArgumentType argTypeStr interface

  let location = messageDescription <> "." <> name

  nullable <- peekAttr "allow-null" element >>= \case
    Just "true" -> pure True
    Just "false" -> pure False
    Just x -> fail $ "Invalid value for attribute \"allow-null\" on " <> location <> ": " <> x
    Nothing -> pure False
  pure ArgumentSpec {
    name,
    index,
    summary,
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
