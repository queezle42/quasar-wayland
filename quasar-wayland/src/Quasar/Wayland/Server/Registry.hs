module Quasar.Wayland.Server.Registry (
  Registry,
  Global,
  newRegistry,
  addRegistryConnection,
  createGlobal,
  createBackendGlobal,
  Version,
  maxVersion,

  -- * Reexports
  Rc,
) where

import Data.HashMap.Strict qualified as HM
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String (IsString(..))
import Quasar.Disposer.Rc
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated

-- TODO: send registry messages
-- TODO: remove RegistryConnection when registry protocol connection is destroyed

data Registry b = Registry {
  backend :: Rc b,
  connections :: TVar [RegistryConnection b],
  singletons :: Seq (Global b),
  globalsVar :: TVar (HM.HashMap Word32 (Global b))
}

newRegistry :: forall b m. MonadIO m => Rc b -> [Global b] -> m (Registry b)
newRegistry backend singletons = do
  connections <- newTVarIO mempty
  globalsVar <- newTVarIO mempty
  pure Registry { backend, connections, singletons = Seq.fromList singletons, globalsVar }

-- | An object that describes the connection from a @wl_registry@ object to a
-- `Registry`.
data RegistryConnection b = RegistryConnection {
  registry :: Registry b,
  wlRegistry :: Object 'Server Interface_wl_registry
}

createGlobal ::
  forall i b. IsInterfaceSide 'Server i =>
  Version -> (NewObject 'Server i -> STMc NoRetry '[SomeException] ()) -> Global b
createGlobal supportedVersion bindFn = createBackendGlobal supportedVersion (const bindFn)

createBackendGlobal ::
  forall i b. IsInterfaceSide 'Server i =>
  Version -> (Rc b -> NewObject 'Server i -> STMc NoRetry '[SomeException] ()) -> Global b
createBackendGlobal supportedVersion bindFn =
  Global {
    interface = fromString (interfaceName @i),
    version = min supportedVersion (interfaceVersion @i),
    bindObject
  }
  where
    bindObject :: Rc b -> GenericNewId -> ProtocolM 'Server ()
    bindObject backend newId = do
      object <- bindObjectFromId Nothing supportedVersion newId
      liftSTMc $ bindFn backend object

addRegistryConnection ::
  Registry b -> Object 'Server Interface_wl_registry -> STMc NoRetry '[SomeException] ()
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

sendGlobal :: Object 'Server Interface_wl_registry -> (Word32, Global b) -> CallM ()
sendGlobal wlRegistry (name, global) = wlRegistry.global name global.interface global.version

bindHandler :: Registry b -> ProtocolHandle 'Server -> Word32 -> GenericNewId -> CallbackM ()
bindHandler registry protocolHandle name newId = do
  case Seq.lookup (fromIntegral name) registry.singletons of
    Just global -> runProtocolM protocolHandle (global.bindObject registry.backend newId)
    Nothing -> traceM $ "Invalid global " <> show name
  -- TODO dynamic globals

data Global b = Global {
  interface :: WlString,
  version :: Word32,
  bindObject :: Rc b -> GenericNewId -> ProtocolM 'Server ()
}
