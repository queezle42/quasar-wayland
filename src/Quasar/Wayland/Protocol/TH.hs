{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Protocol.TH (
  generateWaylandProcol,
  generateWaylandProcols,
) where

import Control.Monad.STM
import Control.Monad.Writer
import Data.ByteString qualified as BS
import Data.List (intersperse, singleton)
import Data.Void (absurd)
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import Quasar.Prelude hiding (Type)
import Quasar.Wayland.Protocol.Core
import System.Posix.Types (Fd)
import Text.Read (readEither)
import Text.XML.Light


newtype ProtocolSpec = ProtocolSpec {interfaces :: [InterfaceSpec]}
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

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
  events :: [EventSpec],
  enums :: [EnumSpec]
}
  deriving stock Show

newtype RequestSpec = RequestSpec {messageSpec :: MessageSpec}
  deriving stock Show

newtype EventSpec = EventSpec {messageSpec :: MessageSpec}
  deriving stock Show

data MessageSpec = MessageSpec {
  name :: String,
  since :: Maybe Version,
  description :: Maybe DescriptionSpec,
  opcode :: Opcode,
  arguments :: [ArgumentSpec],
  isConstructor :: Bool,
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

data EnumSpec = EnumSpec {
  name :: String,
  description :: Maybe DescriptionSpec,
  entries :: [EnumEntrySpec],
  isBitfield :: Bool
}
  deriving stock Show

data EnumEntrySpec = EnumEntrySpec {
  name :: String,
  value :: Word32,
  summary :: Maybe String,
  since :: Maybe Version
}
  deriving stock Show

data ArgumentType
  = IntArgument
  | UIntArgument
  | FixedArgument
  | StringArgument
  | ArrayArgument
  | ObjectArgument String
  | NullableObjectArgument String
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

generateWaylandProcols :: [FilePath] -> Q [Dec]
generateWaylandProcols protocolFiles = do
  mapM addDependentFile protocolFiles
  xmls <- mapM (liftIO . BS.readFile) protocolFiles
  protocol <- mconcat <$> mapM parseProtocol xmls
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
      tySynInstD (tySynEqn Nothing (appT (conT ''InterfaceName) iT) (litT (strTyLit interface.name))),
      tySynInstD (tySynEqn Nothing (appT (conT ''InterfaceVersion) iT) (litT (numTyLit interface.version)))
      ]
    -- IsInterfaceSide instance
    tellQs interfaceSideInstanceDs

    when (length interface.requests > 0) do
      -- Requests record
      tellQ requestCallbackRecordD
      -- Request proxies
      tellQs requestProxyInstanceDecs

    when (length interface.events > 0) do
      -- Events record
      tellQ eventCallbackRecordD
      -- Event proxies
      tellQs eventProxyInstanceDecs

  internals <- execWriterT do
    -- Request wire type
    when (length interface.requests > 0) do
      tellQs $ messageTypeDecs rTypeName wireRequestContexts

    -- Event wire type
    when (length interface.events > 0) do
      tellQs $ messageTypeDecs eTypeName wireEventContexts

  pure (public, internals)

  where
    iName = interfaceN interface
    iT = interfaceT interface
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
    requestCallbackRecordD = messageHandlerRecordD Server (requestsName interface) wireRequestContexts

    requestProxyInstanceDecs :: Q [Dec]
    requestProxyInstanceDecs = messageProxyInstanceDecs Client wireRequestContexts

    eventCallbackRecordD :: Q Dec
    eventCallbackRecordD = messageHandlerRecordD Client (eventsName interface) wireEventContexts

    eventProxyInstanceDecs :: Q [Dec]
    eventProxyInstanceDecs = messageProxyInstanceDecs Server wireEventContexts

    objectName = mkName "object"
    objectP :: Q Pat
    objectP = varP objectName
    objectE :: Q Exp
    objectE = varE objectName

    handlerName = mkName "handler"
    handlerP :: Q Pat
    handlerP = varP handlerName
    handlerE :: Q Exp
    handlerE = varE handlerName

    interfaceSideInstanceDs :: Q [Dec]
    interfaceSideInstanceDs = execWriterT do
      tellQ $ instanceD (pure []) ([t|IsInterfaceSide 'Client $iT|]) [handleMessageD Client]
      tellQ $ instanceD (pure []) ([t|IsInterfaceSide 'Server $iT|]) [handleMessageD Server]

    handleMessageD :: Side -> Q Dec
    handleMessageD Client = funD 'handleMessage (handleMessageClauses wireEventContexts)
    handleMessageD Server = funD 'handleMessage (handleMessageClauses wireRequestContexts)

    handleMessageClauses :: [MessageContext] -> [Q Clause]
    handleMessageClauses [] = [clause [wildP, wildP] (normalB [|absurd|]) []]
    handleMessageClauses messageContexts = handleMessageClause <$> messageContexts

    handleMessageClause :: MessageContext -> Q Clause
    handleMessageClause msg = clause [objectIfRequiredP, handlerP, msgConP msg] (normalB bodyE) []
      where
        objectIfRequiredP :: Q Pat
        objectIfRequiredP = if msg.msgSpec.isDestructor then objectP else wildP
        fieldNameLitT :: Q Type
        fieldNameLitT = litT (strTyLit (messageFieldNameString msg))
        msgHandlerE :: Q Exp
        msgHandlerE = [|$(appTypeE [|getField|] fieldNameLitT) $handlerE|]
        bodyE :: Q Exp
        bodyE
          | msg.msgSpec.isDestructor = [|handleDestructor $objectE >> $msgE|]
          | otherwise = msgE
        msgE :: Q Exp
        msgE = [|$(applyMsgArgs msgHandlerE) >>= lift|]

        applyMsgArgs :: Q Exp -> Q Exp
        applyMsgArgs base = applyA base (argE <$> msg.msgSpec.arguments)

        argE :: ArgumentSpec -> Q Exp
        argE arg = fromWireArgument arg.argType (msgArgE msg arg)

        fromWireArgument :: ArgumentType -> Q Exp -> Q Exp
        fromWireArgument (ObjectArgument _) objIdE = [|getObject $objIdE|]
        fromWireArgument (NullableObjectArgument _) objIdE = [|getNullableObject $objIdE|]
        fromWireArgument (NewIdArgument _) objIdE = [|newObjectFromId Nothing $objIdE|]
        fromWireArgument _ x = [|pure $x|]

messageProxyInstanceDecs :: Side -> [MessageContext] -> Q [Dec]
messageProxyInstanceDecs side messageContexts = mapM messageProxyInstanceD messageContexts
  where
    messageProxyInstanceD :: MessageContext -> Q Dec
    messageProxyInstanceD msg = instanceD (pure []) instanceT [
      funD 'getField [clause ([varP objectName] <> msgProxyArgPats msg) (normalB [|enterObject object $actionE|]) []]
      ]
      where
        objectName = mkName "object"
        instanceT :: Q Type
        instanceT = [t|HasField $(litT (strTyLit msg.msgSpec.name)) $objectT $proxyT|]
        objectT :: Q Type
        objectT = [t|Object $(sideT side) $(msg.msgInterfaceT)|]
        proxyT :: Q Type
        proxyT = [t|$(applyArgTypes [t|STM $returnT|])|]
        returnT :: Q Type
        returnT = maybe [t|()|] (argumentType side) (proxyReturnArgument msg.msgSpec)
        applyArgTypes :: Q Type -> Q Type
        applyArgTypes xt = foldr (\x y -> [t|$x -> $y|]) xt (argumentType side <$> args)

        args :: [ArgumentSpec]
        args = proxyArguments msg.msgSpec

        actionE :: Q Exp
        actionE
          | msg.msgSpec.isConstructor = ctorE
          | msg.msgSpec.isDestructor = dtorE
          | otherwise = normalE

        -- Constructor: the first argument becomes the return value
        ctorE :: Q Exp
        ctorE = [|newObject Nothing >>= \(newObj, newId) -> newObj <$ (sendMessage object =<< $(msgE [|pure newId|]))|]
          where
            msgE :: Q Exp -> Q Exp
            msgE idArgE = mkWireMsgE (idArgE : (wireArgE <$> args))

        dtorE :: Q Exp
        dtorE = [|handleDestructor object >> $normalE|]

        -- Body for a normal (i.e. non-constructor) proxy
        normalE :: Q Exp
        normalE = [|sendMessage object =<< $(msgE)|]
          where
            msgE :: Q Exp
            msgE = mkWireMsgE (wireArgE <$> args)

        mkWireMsgE :: [Q Exp] -> Q Exp
        mkWireMsgE mkWireArgEs = applyA (conE msg.msgConName) mkWireArgEs

        wireArgE :: ArgumentSpec -> Q Exp
        wireArgE arg = toWireArgument arg.argType (msgArgE msg arg)

        toWireArgument :: ArgumentType -> Q Exp -> Q Exp
        toWireArgument (ObjectArgument _) objectE = [|objectWireArgument $objectE|]
        toWireArgument (NullableObjectArgument _) objectE = [|nullableObjectWireArgument $objectE|]
        toWireArgument (NewIdArgument _) _ = unreachableCodePath -- The specification parser has a check to prevent this
        toWireArgument _ x = [|pure $x|]

proxyArguments :: MessageSpec -> [ArgumentSpec]
proxyArguments msg = (if msg.isConstructor then drop 1 else id) msg.arguments

proxyReturnArgument :: MessageSpec -> Maybe ArgumentSpec
proxyReturnArgument msg@MessageSpec{arguments=(firstArg:_)} = if msg.isConstructor then Just firstArg else Nothing
proxyReturnArgument _ = Nothing


messageFieldName :: MessageContext -> Name
messageFieldName msg = mkName $ messageFieldNameString msg

messageFieldNameString :: MessageContext -> String
messageFieldNameString msg = msg.msgSpec.name

messageHandlerRecordD :: Side -> Name -> [MessageContext] -> Q Dec
messageHandlerRecordD side name messageContexts = dataD (cxt []) name [] Nothing [con] []
  where
    con = recC name (recField <$> messageContexts)
    recField :: MessageContext -> Q VarBangType
    recField msg = varDefaultBangType (messageFieldName msg) [t|$(applyArgTypes [t|STM ()|])|]
      where
        applyArgTypes :: Q Type -> Q Type
        applyArgTypes xt = foldr (\x y -> [t|$x -> $y|]) xt (argumentType side <$> msg.msgSpec.arguments)


sideT :: Side -> Q Type
sideT Client = [t|'Client|]
sideT Server = [t|'Server|]

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

orUnit :: Maybe (Q Type) -> Q Type
orUnit = fromMaybe [t|()|]



data MessageContext = MessageContext {
  msgInterfaceT :: Q Type,
  msgT :: Q Type,
  msgConName :: Name,
  msgInterfaceSpec :: InterfaceSpec,
  msgSpec :: MessageSpec
}

-- | Pattern to match a wire message. Arguments can then be accessed by using 'msgArgE'.
msgConP :: MessageContext -> Q Pat
msgConP msg = conP msg.msgConName (msgArgPats msg)

-- | Pattern to match all arguments of a message (wire/handler). Arguments can then be accessed by using e.g. 'msgArgE'.
msgArgPats :: MessageContext -> [Q Pat]
msgArgPats msg = varP . msgArgTempName <$> msg.msgSpec.arguments

-- | Pattern to match all arguments of a message (for a proxy). Arguments can then be accessed by using e.g. 'msgArgE'.
msgProxyArgPats :: MessageContext -> [Q Pat]
msgProxyArgPats msg = varP . msgArgTempName <$> proxyArguments msg.msgSpec

-- | Expression for accessing a message argument which has been matched from a request/event using 'msgArgConP'.
msgArgE :: MessageContext -> ArgumentSpec -> Q Exp
msgArgE _msg arg = varE (msgArgTempName arg)

-- | Helper for 'msgConP' and 'msgArgE'.
msgArgTempName :: ArgumentSpec -> Name
-- Adds a prefix to prevent name conflicts with exports from the Prelude; would be better to use `newName` instead.
msgArgTempName arg = mkName $ "arg_" <> arg.name


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
    showClause msg = clause [msgConP msg] (normalB bodyE) []
      where
        bodyE :: Q Exp
        bodyE = [|mconcat $(listE ([stringE (msg.msgSpec.name ++ "(")] <> mconcat (intersperse [stringE ", "] (showArgE <$> msg.msgSpec.arguments) <> [[stringE ")"]])))|]
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
        putMessageE args = [|buildMessage $(litE $ integerL $ fromIntegral msg.msgSpec.opcode) $(putMessageBodyE args)|]
        putMessageBodyE :: [ArgumentSpec] -> Q Exp
        putMessageBodyE args = [|$(listE ((\arg -> [|putArgument @($(argumentWireType arg)) $(msgArgE msg arg)|]) <$> args))|]


derivingEq :: Q DerivClause
derivingEq = derivClause (Just StockStrategy) [[t|Eq|]]

-- | Map an argument to its high-level api type
argumentType :: Side -> ArgumentSpec -> Q Type
argumentType side argSpec = liftArgumentType side argSpec.argType

liftArgumentType :: Side -> ArgumentType -> Q Type
liftArgumentType side (ObjectArgument iName) = [t|Object $(sideT side) $(interfaceTFromName iName)|]
liftArgumentType side (NullableObjectArgument iName) = [t|Maybe (Object $(sideT side) $(interfaceTFromName iName))|]
liftArgumentType side (NewIdArgument iName) = [t|NewObject $(sideT side) $(interfaceTFromName iName)|]
liftArgumentType _ x = liftArgumentWireType x


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
liftArgumentWireType (NullableObjectArgument iName) = [t|ObjectId $(litT (strTyLit iName))|]
liftArgumentWireType GenericObjectArgument = [t|GenericObjectId|]
liftArgumentWireType (NewIdArgument iName) = [t|NewId $(litT (strTyLit iName))|]
liftArgumentWireType GenericNewIdArgument = [t|GenericNewId|]
liftArgumentWireType FdArgument = [t|Fd|]


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
    _ -> fail $ "Cannot parse description xml: " <> show element
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
  version <- either fail pure . readEither =<< getAttr "version" element
  description <- findDescription element
  requests <- mapM (parseRequest name) $ zip [0..] $ findChildren (qname "request") element
  events <- mapM (parseEvent name) $ zip [0..] $ findChildren (qname "event") element
  enums <- mapM (parseEnum name) $ findChildren (qname "enum") element
  pure InterfaceSpec {
    name,
    version,
    description,
    requests,
    events,
    enums
  }

parseRequest :: MonadFail m => String -> (Opcode, Element) -> m RequestSpec
parseRequest x y = RequestSpec <$> parseMessage True x y

parseEvent :: MonadFail m => String -> (Opcode, Element) -> m EventSpec
parseEvent x y = EventSpec <$> parseMessage False x y

parseMessage :: MonadFail m => Bool -> String -> (Opcode, Element) -> m MessageSpec
parseMessage isRequest interface (opcode, element) = do
  let isEvent = not isRequest

  name <- getAttr "name" element

  let loc = interface <> "." <> name

  mtype <- peekAttr "type" element
  since <- mapM (either fail pure . readEither) =<< peekAttr "since" element
  description <- findDescription element
  arguments <- mapM (parseArgument loc) $ zip [0..] $ findChildren (qname "arg") element

  isDestructor <-
    case mtype of
      Nothing -> pure False
      Just "destructor" -> pure True
      Just messageType -> fail $ "Unknown message type: " <> messageType

  forM_ arguments \arg -> do
    when
      do arg.argType == GenericNewIdArgument && (interface /= "wl_registry" || name /= "bind")
      do fail $ "Invalid \"new_id\" argument without \"interface\" attribute encountered on " <> loc <> " (only valid on wl_registry.bind)"
    when
      do arg.argType == GenericObjectArgument && (interface /= "wl_display" || name /= "error")
      do fail $ "Invalid \"object\" argument without \"interface\" attribute encountered on " <> loc <> " (only valid on wl_display.error)"

  isConstructor <- case arguments of
    [] -> pure False
    (firstArg:otherArgs) -> do
      when
        do any (isNewId . (.argType)) otherArgs && not (interface == "wl_registry" && name == "bind")
        -- TODO incorrect assumption, needs to be supported for wp_presentation
        do fail $ "Message uses NewId in unsupported position on: " <> loc <> " (NewId currently has to be the first argument, which is a parser bug)"
      pure (isNewId firstArg.argType)

  pure MessageSpec  {
    name,
    since,
    description,
    opcode,
    arguments,
    isConstructor,
    isDestructor
  }


parseArgument :: forall m. MonadFail m => String -> (Integer, Element) -> m ArgumentSpec
parseArgument messageDescription (index, element) = do
  name <- getAttr "name" element
  summary <- peekAttr "summary" element
  argTypeStr <- getAttr "type" element
  interface <- peekAttr "interface" element

  let loc = messageDescription <> "." <> name

  nullable <- peekAttr "allow-null" element >>= \case
    Just "true" -> pure True
    Just "false" -> pure False
    Just x -> fail $ "Invalid value for attribute \"allow-null\" on " <> loc <> ": " <> x
    Nothing -> pure False

  argType <- parseArgumentType argTypeStr interface nullable

  pure ArgumentSpec {
    name,
    index,
    summary,
    argType,
    nullable
  }
  where
    parseArgumentType :: String -> Maybe String -> Bool -> m ArgumentType
    parseArgumentType "int" Nothing _ = pure IntArgument
    parseArgumentType "uint" Nothing _ = pure UIntArgument
    parseArgumentType "fixed" Nothing _ = pure FixedArgument
    parseArgumentType "string" Nothing _ = pure StringArgument
    parseArgumentType "array" Nothing _ = pure ArrayArgument
    parseArgumentType "object" (Just interface) False = pure (ObjectArgument interface)
    parseArgumentType "object" (Just interface) True = pure (NullableObjectArgument interface)
    parseArgumentType "object" Nothing _ = pure GenericObjectArgument
    parseArgumentType "new_id" (Just interface) _ = pure (NewIdArgument interface)
    parseArgumentType "new_id" Nothing _ = pure GenericNewIdArgument
    parseArgumentType "fd" Nothing _ = pure FdArgument
    parseArgumentType x Nothing _ = fail $ "Unknown argument type \"" <> x <> "\" encountered"
    parseArgumentType x _ _ = fail $ "Argument type \"" <> x <> "\" should not have \"interface\" attribute"


parseEnum :: MonadFail m => String -> Element -> m EnumSpec
parseEnum interface element = do
  name <- getAttr "name" element
  description <- findDescription element
  entries <- mapM parseEnumEntry $ findChildren (qname "entry") element

  let loc = interface <> "." <> name

  isBitfield <- peekAttr "bitfield" element >>= \case
    Just "true" -> pure True
    Just "false" -> pure False
    Just x -> fail $ "Invalid value for attribute \"bitfield\" on " <> loc <> ": " <> x
    Nothing -> pure False

  pure EnumSpec {
    name,
    description,
    entries,
    isBitfield
  }

parseEnumEntry :: MonadFail m => Element -> m EnumEntrySpec
parseEnumEntry element = do
  name <- getAttr "name" element
  value <- (either fail pure . readEither) =<< getAttr "value" element
  summary <- peekAttr "summary" element
  since <- mapM (either fail pure . readEither) =<< peekAttr "since" element
  pure EnumEntrySpec {
    name,
    value,
    summary,
    since
  }


qname :: String -> QName
qname name = blank_name { qName = name }

getAttr :: MonadFail m => String -> Element -> m String
getAttr name element = do
  (Just value) <- pure $ findAttr (qname name) element
  pure value

peekAttr :: Applicative m => String -> Element -> m (Maybe String)
peekAttr name element = pure $ findAttr (qname name) element
