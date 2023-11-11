module Quasar.Wayland.Server.Registry (
  Registry,
  Global,
  newRegistry,
  addRegistryConnection,
  createGlobal,
  Version,
  maxVersion,
) where

import Data.HashMap.Strict qualified as HM
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString(..))
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated

-- TODO: send registry messages
-- TODO: remove RegistryConnection when registry protocol connection is destroyed

data Registry = Registry {
  connections :: TVar [RegistryConnection],
  singletons :: Seq Global,
  globalsVar :: TVar (HM.HashMap Word32 Global)
}

newRegistry :: MonadIO m => [Global] -> m Registry
newRegistry singletons = do
  connections <- newTVarIO mempty
  globalsVar <- newTVarIO mempty
  pure Registry { connections, singletons = Seq.fromList singletons, globalsVar }

-- | An object that describes the connection from a @wl_registry@ object to a
-- `Registry`.
data RegistryConnection = RegistryConnection {
  registry :: Registry,
  wlRegistry :: Object 'Server Interface_wl_registry
}

createGlobal :: forall i. IsInterfaceSide 'Server i => Version -> (NewObject 'Server i -> STMc NoRetry '[SomeException] ()) -> Global
createGlobal supportedVersion bindFn =
  Global {
    interface = fromString (interfaceName @i),
    version = min supportedVersion (interfaceVersion @i),
    bindObject
  }
  where
    bindObject :: GenericNewId -> ProtocolM 'Server ()
    bindObject newId = do
      object <- bindObjectFromId Nothing supportedVersion newId
      liftSTMc $ bindFn object

addRegistryConnection :: Registry -> Object 'Server Interface_wl_registry -> STMc NoRetry '[SomeException] ()
addRegistryConnection registry wlRegistry = do
  setMessageHandler wlRegistry messageHandler
  modifyTVar registry.connections (connection:)
  forM_ (zip [0..] (toList registry.singletons)) (sendGlobal wlRegistry)
  where
    connection = RegistryConnection { registry, wlRegistry }
    messageHandler :: RequestHandler_wl_registry
    messageHandler = RequestHandler_wl_registry {
      bind = bindHandler registry (objectProtocol wlRegistry)
    }

sendGlobal :: Object 'Server Interface_wl_registry -> (Word32, Global) -> CallM ()
sendGlobal wlRegistry (name, global) = wlRegistry.global name global.interface global.version

bindHandler :: Registry -> ProtocolHandle 'Server -> Word32 -> GenericNewId -> CallbackM ()
bindHandler registry protocolHandle name newId = do
  case Seq.lookup (fromIntegral name) registry.singletons of
    Just global -> runProtocolM protocolHandle (global.bindObject newId)
    Nothing -> traceM $ "Invalid global " <> show name
  -- TODO dynamic globals

data Global = Global {
  interface :: WlString,
  version :: Word32,
  bindObject :: GenericNewId -> ProtocolM 'Server ()
}
