module Quasar.Wayland.Client.Registry (
  Registry,
  createRegistry
) where

import Control.Concurrent.STM
import Data.HashMap.Strict qualified as HM
import Data.Tuple (swap)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Display
import Quasar.Wayland.Protocol.Generated

-- * wl_registry

data Registry = Registry {
  wlRegistry :: Object 'Client Interface_wl_registry,
  globalsVar :: TVar (HM.HashMap Word32 (WlString, Word32)),
  initialSyncComplete :: Awaitable ()
}

createRegistry :: Object 'Client Interface_wl_display -> STM Registry
createRegistry wlDisplay = mfix \clientRegistry -> do
  globalsVar <- newTVar HM.empty

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  -- Manual sync (without high-level wrapper) to prevent a dependency loop to the client
  var <- newAsyncVarSTM
  lowLevelSync wlDisplay \_ -> putAsyncVarSTM_ var ()
  let initialSyncComplete = toAwaitable var

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
        global name interface version = do
          modifyTVar clientRegistry.globalsVar (HM.insert name (interface, version))

        global_remove :: Word32 -> STM ()
        global_remove name = do
          result <- stateTVar clientRegistry.globalsVar (swap . lookupDelete name)
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just (interface, version) -> pure ()
