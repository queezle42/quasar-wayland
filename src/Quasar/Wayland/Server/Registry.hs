module Quasar.Wayland.Server.Registry (
  Registry,
  newRegistry,
  addRegistryConnection,
) where

import Data.HashMap.Strict qualified as HM
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

-- TODO: send registry messages
-- TODO: remove connection when registry is destroyed

data Registry = Registry {
  connections :: TVar [RegistryConnection],
  globalsVar :: TVar (HM.HashMap Word32 Global)
}

newRegistry :: MonadIO m => m Registry
newRegistry = do
  connections <- newTVarIO mempty
  globalsVar <- newTVarIO mempty
  pure Registry { connections, globalsVar }

data RegistryConnection = RegistryConnection {
  registry :: Registry,
  wlRegistry :: Object 'Server Interface_wl_registry
}

addRegistryConnection :: Registry -> Object 'Server Interface_wl_registry -> STM ()
addRegistryConnection registry wlRegistry = do
  setMessageHandler wlRegistry messageHandler
  modifyTVar registry.connections (connection:)
  where
    connection = RegistryConnection { registry, wlRegistry }
    messageHandler :: RequestHandler_wl_registry
    messageHandler = RequestHandler_wl_registry {
      bind = \name id -> traceM "wl_registry.bind not implemented"
    }

data Global = Global {
  name :: Word32,
  interface :: WlString,
  version :: Word32
}
