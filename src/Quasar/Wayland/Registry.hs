module Quasar.Wayland.Registry (
  ClientRegistry,
  createClientRegistry,
) where

import Control.Concurrent.STM
import Control.Monad.Fix (mfix)
import Control.Monad.Reader (lift)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Tuple (swap)
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

data ClientRegistry = ClientRegistry {
  wlRegistry :: Object 'Client I_wl_registry,
  globalsVar :: TVar (HM.HashMap Word32 (BS.ByteString, Word32))
}

createClientRegistry :: Object 'Client I_wl_display -> ProtocolM 'Client ClientRegistry
createClientRegistry wlDisplay = mfix \clientRegistry -> do
  globalsVar <- lift $ newTVar HM.empty

  (wlRegistry, newId) <- newObject @'Client @I_wl_registry (traceCallback (callback clientRegistry))
  sendMessage wlDisplay $ R_wl_display_get_registry newId

  pure ClientRegistry {
    wlRegistry,
    globalsVar
  }
  where
    callback :: ClientRegistry -> IsInterfaceSide 'Client I_wl_registry => Callback 'Client I_wl_registry
    callback clientRegistry = internalFnCallback handler
      where
        -- | wl_registry is specified to never change, so manually specifying the callback is safe
        handler :: Object 'Client I_wl_registry -> E_wl_registry -> ProtocolM 'Client ()
        handler _ (E_wl_registry_global name interface version) = do
          lift $ modifyTVar clientRegistry.globalsVar (HM.insert name (interface, version))
        handler _ (E_wl_registry_global_remove name) = do
          result <- lift $ stateTVar clientRegistry.globalsVar (swap . lookupDelete name)
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just (interface, version) -> pure ()
