module Quasar.Wayland.Client.Registry (
  Registry,
  createRegistry,
  bindSingleton,
  tryBindSingleton,
) where

import Control.Monad.Catch
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
  globalsVar :: TVar (HM.HashMap Word32 Global),
  initialSyncComplete :: Future ()
}

data Global = Global {
  name :: Word32,
  interface :: WlString,
  version :: Word32
}

createRegistry :: Object 'Client Interface_wl_display -> STM Registry
createRegistry wlDisplay = mfix \clientRegistry -> do
  globalsVar <- newTVar HM.empty

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  -- Manual sync (without high-level wrapper) to prevent a dependency loop to the Client module
  initialSyncComplete <- lowLevelSyncFuture wlDisplay

  pure Registry {
    wlRegistry,
    globalsVar,
    initialSyncComplete
  }
  where
    messageHandler :: Registry -> EventHandler_wl_registry
    messageHandler clientRegistry = EventHandler_wl_registry { global, global_remove }
      where
        global :: Word32 -> WlString -> Word32 -> STM ()
        global name interface version =
          modifyTVar clientRegistry.globalsVar $ HM.insert name Global { name, interface, version }

        global_remove :: Word32 -> STM ()
        global_remove name = do
          result <- stateTVar clientRegistry.globalsVar (lookupDelete name)
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just _ -> pure ()


-- | Bind a new client object to a compositor singleton. Throws an exception if the global is not available.
--
-- Will retry until the the registry has sent the initial list of globals.
bindSingleton :: IsInterfaceSide 'Client i => Registry -> STM (Object 'Client i)
bindSingleton registry = either (throwM . ProtocolUsageError) pure =<< tryBindSingleton registry

-- | Try to bind a new client object to a compositor singleton.
--
-- Will retry until the the registry has sent the initial list of globals.
tryBindSingleton :: forall i. IsInterfaceSide 'Client i => Registry -> STM (Either String (Object 'Client i))
tryBindSingleton registry = do
  awaitSTM registry.initialSyncComplete

  globals <- filterInterface . HM.elems <$> readTVar registry.globalsVar

  case globals of
    [] -> pure $ Left $ mconcat ["No global named ", interfaceName @i, " is available"]
    (global:[]) -> do
      let version = min global.version (interfaceVersion @i)
      (object, newId) <- bindNewObject registry.wlRegistry.objectProtocol version Nothing
      registry.wlRegistry.bind global.name newId
      pure $ Right object
    _ -> pure $ Left $ mconcat ["Cannot bind singleton: multiple globals with type ", interfaceName @i, " are available"]

  where
    filterInterface :: [Global] -> [Global]
    filterInterface = filter \global -> global.interface == fromString (interfaceName @i)
