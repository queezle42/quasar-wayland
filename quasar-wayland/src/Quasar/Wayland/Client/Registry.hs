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
import Quasar.Observable.Map (ObservableMap, ObservableMapVar, toObservableMap, readObservableMap)
import Quasar.Observable.Map qualified as ObservableMap
import Quasar.Observable.Core (NoLoad)
import Quasar.Prelude
import Quasar.Wayland.Client.Sync
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Utils.Fix (mfixExtra)

-- * wl_registry

data Registry = Registry {
  wlRegistry :: Object 'Client Interface_wl_registry,
  globals :: ObservableMapVar Word32 Global
}

data Global = Global {
  name :: Word32,
  interface :: WlString,
  version :: Word32
}

createRegistry :: Object 'Client Interface_wl_display -> STMc NoRetry '[SomeException] (FutureEx '[SomeException] Registry)
createRegistry wlDisplay = mfixExtra \clientRegistry -> do
  globals <- ObservableMap.newVar mempty

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  -- Manual sync (without high-level wrapper) to prevent a dependency loop to the Client module
  initialSyncComplete <- lowLevelSyncFuture wlDisplay

  let registry = Registry {
    wlRegistry,
    globals
  }

  pure (registry <$ initialSyncComplete, registry)
  where
    messageHandler :: Registry -> EventHandler_wl_registry
    messageHandler clientRegistry = EventHandler_wl_registry { global, global_remove }
      where
        global :: Word32 -> WlString -> Word32 -> STMc NoRetry '[SomeException] ()
        global name interface version = do
          let global' = Global { name, interface, version }
          ObservableMap.insertVar clientRegistry.globals name global'

        global_remove :: Word32 -> STMc NoRetry '[SomeException] ()
        global_remove name = do
          result <- ObservableMap.lookupDeleteVar clientRegistry.globals name
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just _ -> pure ()


-- | Bind a new client object to a compositor singleton. Throws an exception if the global is not available.
bindSingleton :: IsInterfaceSide 'Client i => Registry -> Version -> STMc NoRetry '[SomeException] (NewObject 'Client i)
bindSingleton registry version = either (throwM . ProtocolUsageError) pure =<< liftSTMc (tryBindSingleton registry version)

-- | Try to bind a new client object to a compositor singleton.
tryBindSingleton :: forall i. IsInterfaceSide 'Client i => Registry -> Version -> STMc NoRetry '[SomeException] (Either String (NewObject 'Client i))
tryBindSingleton registry version = do
  globals <- readObservableMap (toObservableMap registry.globals)

  case filter (isInterface @i) (Map.elems globals) of
    [] -> pure $ Left $ mconcat ["No global named ", interfaceName @i, " is available"]
    [global] -> liftSTMc $ Right <$> bindGlobal registry global version
    _ -> pure $ Left $ mconcat ["Cannot bind singleton: multiple globals with type ", interfaceName @i, " are available"]

isInterface :: forall i. IsInterfaceSide 'Client i => Global -> Bool
isInterface global = global.interface == fromString (interfaceName @i)

bindGlobal :: forall i. IsInterfaceSide 'Client i => Registry -> Global -> Version -> STMc NoRetry '[SomeException] (NewObject 'Client i)
bindGlobal registry global version = do
  let effectiveVersion = min (min global.version (interfaceVersion @i)) version
  (object, newId) <- bindNewObject registry.wlRegistry.objectProtocol effectiveVersion Nothing
  registry.wlRegistry.bind global.name newId
  pure object

interfaceGlobals :: forall i. IsInterfaceSide 'Client i => Registry -> ObservableMap NoLoad '[] Word32 (Version -> STMc NoRetry '[SomeException] (NewObject 'Client i))
interfaceGlobals registry =
  bindGlobal registry <$> ObservableMap.filter (isInterface @i) (toObservableMap registry.globals)
