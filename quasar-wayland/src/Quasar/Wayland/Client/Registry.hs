module Quasar.Wayland.Client.Registry (
  Registry,
  createRegistry,
  bindSingleton,
  tryBindSingleton,
  interfaceGlobals,
  Version,
  maxVersion,
) where

import Control.Monad.Catch
import Data.Map.Strict qualified as Map
import Data.String (IsString(..))
import Quasar
import Quasar.Observable.ObservableMap (ObservableMap, ObservableMapVar)
import Quasar.Observable.ObservableMap qualified as ObservableMap
import Quasar.Prelude
import Quasar.Wayland.Client.Sync
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

-- * wl_registry

data Registry = Registry {
  wlRegistry :: Object 'Client Interface_wl_registry,
  globals :: ObservableMapVar Word32 Global,
  initialSyncComplete :: FutureEx '[SomeException] ()
}

data Global = Global {
  name :: Word32,
  interface :: WlString,
  version :: Word32
}

createRegistry :: Object 'Client Interface_wl_display -> STM Registry
createRegistry wlDisplay = mfix \clientRegistry -> do
  globals <- ObservableMap.new

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  -- Manual sync (without high-level wrapper) to prevent a dependency loop to the Client module
  initialSyncComplete <- lowLevelSyncFuture wlDisplay

  pure Registry {
    wlRegistry,
    globals,
    initialSyncComplete
  }
  where
    messageHandler :: Registry -> EventHandler_wl_registry
    messageHandler clientRegistry = EventHandler_wl_registry { global, global_remove }
      where
        global :: Word32 -> WlString -> Word32 -> STM ()
        global name interface version = do
          let global' = Global { name, interface, version }
          ObservableMap.insert name global' clientRegistry.globals

        global_remove :: Word32 -> STM ()
        global_remove name = do
          result <- ObservableMap.lookupDelete name clientRegistry.globals
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

  globals <- readObservable registry.globals

  case filter (isInterface @i) (Map.elems globals) of
    [] -> pure $ Left $ mconcat ["No global named ", interfaceName @i, " is available"]
    [global] -> Right <$> bindGlobal registry global version
    _ -> pure $ Left $ mconcat ["Cannot bind singleton: multiple globals with type ", interfaceName @i, " are available"]

isInterface :: forall i. IsInterfaceSide 'Client i => Global -> Bool
isInterface global = global.interface == fromString (interfaceName @i)

bindGlobal :: forall i. IsInterfaceSide 'Client i => Registry -> Global -> Version -> STM (NewObject 'Client i)
bindGlobal registry global version = do
  let effectiveVersion = min (min global.version (interfaceVersion @i)) version
  (object, newId) <- bindNewObject registry.wlRegistry.objectProtocol effectiveVersion Nothing
  registry.wlRegistry.bind global.name newId
  pure object

interfaceGlobals :: forall i. IsInterfaceSide 'Client i => Registry -> ObservableMap Word32 (Version -> STM (NewObject 'Client i))
interfaceGlobals registry =
  bindGlobal registry <$> ObservableMap.filter (isInterface @i) registry.globals
