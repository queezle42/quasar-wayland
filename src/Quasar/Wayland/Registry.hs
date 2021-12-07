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

createClientRegistry :: Object 'Client Interface_wl_display -> ProtocolM 'Client ClientRegistry
createClientRegistry wlDisplay = mfix \clientRegistry -> do
  globalsVar <- lift $ newTVar HM.empty

  (wlRegistry, newId) <- newObject @'Client @Interface_wl_registry (callback clientRegistry)
  sendMessage wlDisplay $ WireRequest_wl_display_get_registry newId

  pure ClientRegistry {
    wlRegistry,
    globalsVar
  }
  where
    callback :: ClientRegistry -> IsInterfaceSide 'Client Interface_wl_registry => WireCallback 'Client Interface_wl_registry
    callback clientRegistry = internalFnWireCallback handler
      where
        -- | wl_registry is specified to never change, so manually specifying the callback is safe
        handler :: Object 'Client Interface_wl_registry -> WireEvent_wl_registry -> ProtocolM 'Client ()
        handler _ (WireEvent_wl_registry_global name interface version) = do
          lift $ modifyTVar clientRegistry.globalsVar (HM.insert name (interface, version))
        handler _ (WireEvent_wl_registry_global_remove name) = do
          result <- lift $ stateTVar clientRegistry.globalsVar (swap . lookupDelete name)
          case result of
            Nothing -> traceM $ "Invalid global removed by server: " <> show name
            Just (interface, version) -> pure ()
