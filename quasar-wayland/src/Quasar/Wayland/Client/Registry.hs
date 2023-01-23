module Quasar.Wayland.Client.Registry (
  Registry,
  createRegistry,
  bindSingleton,
  tryBindSingleton,
  registerGlobalHandler,
  Version,
  maxVersion,
) where

import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.String (IsString(..))
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client.Sync
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

-- * wl_registry

data Registry = Registry {
  wlRegistry :: Object 'Client Interface_wl_registry,
  globals :: TVar (HM.HashMap Word32 Global),
  initialSyncComplete :: FutureEx '[SomeException] (),
  bindCallbacks :: TVar (HashMap WlString [Global -> STM ()])
}

data Global = Global {
  name :: Word32,
  interface :: WlString,
  version :: Word32
}

createRegistry :: Object 'Client Interface_wl_display -> STM Registry
createRegistry wlDisplay = mfix \clientRegistry -> do
  globals <- newTVar mempty
  bindCallbacks <- newTVar mempty

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  -- Manual sync (without high-level wrapper) to prevent a dependency loop to the Client module
  initialSyncComplete <- lowLevelSyncFuture wlDisplay

  pure Registry {
    wlRegistry,
    globals,
    initialSyncComplete,
    bindCallbacks
  }
  where
    messageHandler :: Registry -> EventHandler_wl_registry
    messageHandler clientRegistry = EventHandler_wl_registry { global, global_remove }
      where
        global :: Word32 -> WlString -> Word32 -> STM ()
        global name interface version = do
          let global' = Global { name, interface, version }
          modifyTVar clientRegistry.globals $ HM.insert name global'
          HM.lookup interface <$> readTVar clientRegistry.bindCallbacks >>=
            mapM_ (mapM_ ($ global'))

        global_remove :: Word32 -> STM ()
        global_remove name = do
          result <- stateTVar clientRegistry.globals (lookupDelete name)
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just _ -> pure ()


-- | Bind a new client object to a compositor singleton. Throws an exception if the global is not available.
--
-- Will retry until the the registry has sent the initial list of globals.
bindSingleton :: IsInterfaceSide 'Client i => Registry -> Version -> STM (NewObject 'Client i)
bindSingleton registry version = either (throwM . ProtocolUsageError) pure =<< tryBindSingleton registry version

-- | Try to bind a new client object to a compositor singleton.
--
-- Will retry until the the registry has sent the initial list of globals.
tryBindSingleton :: forall i. IsInterfaceSide 'Client i => Registry -> Version -> STM (Either String (NewObject 'Client i))
tryBindSingleton registry version = do
  either throwM pure =<< awaitSTM registry.initialSyncComplete

  globals <- filterInterface @i . HM.elems <$> readTVar registry.globals

  case globals of
    [] -> pure $ Left $ mconcat ["No global named ", interfaceName @i, " is available"]
    [global] -> Right <$> bindGlobal registry version global
    _ -> pure $ Left $ mconcat ["Cannot bind singleton: multiple globals with type ", interfaceName @i, " are available"]

filterInterface :: forall i. IsInterfaceSide 'Client i => [Global] -> [Global]
filterInterface = filter \global -> global.interface == fromString (interfaceName @i)

bindGlobal :: forall i. IsInterfaceSide 'Client i => Registry -> Version -> Global -> STM (NewObject 'Client i)
bindGlobal registry version global = do
  let effectiveVersion = min (min global.version (interfaceVersion @i)) version
  (object, newId) <- bindNewObject registry.wlRegistry.objectProtocol effectiveVersion Nothing
  registry.wlRegistry.bind global.name newId
  pure object


registerGlobalHandler :: forall i. IsInterfaceSide 'Client i => Registry -> Version -> (NewObject 'Client i -> STMc NoRetry '[] Disposer) -> STM ()
registerGlobalHandler registry version callback = do
  either throwM pure =<< awaitSTM registry.initialSyncComplete

  let bindFn = bindGlobalToCallback registry version callback

  globals <- filterInterface @i . HM.elems <$> readTVar registry.globals
  mapM_ bindFn globals

  modifyTVar registry.bindCallbacks $ HM.insertWith (<>) (fromString (interfaceName @i)) [bindFn]


bindGlobalToCallback :: forall i. IsInterfaceSide 'Client i => Registry -> Version -> (NewObject 'Client i -> STMc NoRetry '[] Disposer) -> Global -> STM ()
bindGlobalToCallback registry version callback global = do
  wlObject <- bindGlobal registry version global
  disposer <- liftSTMc (callback wlObject)
  -- TODO store and use disposer
  pure ()
