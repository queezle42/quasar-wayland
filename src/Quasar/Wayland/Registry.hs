module Quasar.Wayland.Registry (
  ClientRegistry,
  createClientRegistry,
) where

import Control.Concurrent.STM
import Control.Monad.Fix (mfix)
import Control.Monad.Reader (lift)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Tuple (swap)
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

data ClientRegistry = ClientRegistry {
  wlRegistry :: Object 'Client Interface_wl_registry,
  globalsVar :: TVar (HM.HashMap Word32 (WlString, Word32))
}

createClientRegistry :: Object 'Client Interface_wl_display -> STM ClientRegistry
createClientRegistry wlDisplay = mfix \clientRegistry -> do
  globalsVar <- newTVar HM.empty

  wlRegistry <- wlDisplay.get_registry
  setMessageHandler wlRegistry (messageHandler clientRegistry)

  pure ClientRegistry {
    wlRegistry,
    globalsVar
  }
  where
    messageHandler :: ClientRegistry -> EventHandler_wl_registry
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
