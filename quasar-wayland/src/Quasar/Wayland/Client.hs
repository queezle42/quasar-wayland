module Quasar.Wayland.Client (
  WaylandClient(registry),
  connectWaylandClient,
  newWaylandClient,

  -- * wl_registry
  Registry,
  bindSingleton,
  tryBindSingleton,

  getClientComponent
) where

import Control.Monad.Catch
import Data.Dynamic
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy
import GHC.Records
import Network.Socket (Socket)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client.Sync
import Quasar.Wayland.Client.Registry
import Quasar.Wayland.Client.Socket
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Type.Reflection (SomeTypeRep, someTypeRep)


data WaylandClient b = WaylandClient {
  connection :: WaylandConnection 'Client,
  wlDisplay :: Object 'Client Interface_wl_display,
  registry :: Registry,
  globals :: TVar (Map SomeTypeRep Dynamic)
}

instance Disposable (WaylandClient b) where
  getDisposer client = getDisposer client.connection

connectWaylandClient ::
  forall b m. (MonadIO m, MonadQuasar m) => m (WaylandClient b)
connectWaylandClient = liftQuasarIO $ mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket

newWaylandClient :: (MonadIO m, MonadQuasar m) => Socket -> m (WaylandClient b)
newWaylandClient socket = do
  ((wlDisplay, registryFuture), connection) <- newWaylandConnection newClientDisplay socket

  registry <- liftIO $ await registryFuture

  globals <- newTVarIO mempty

  pure WaylandClient {
    connection,
    wlDisplay,
    registry,
    globals
  }
  where
    newClientDisplay :: STM ((Object 'Client Interface_wl_display, FutureEx '[SomeException] Registry), ProtocolHandle 'Client)
    newClientDisplay = initializeProtocol wlDisplayEventHandler (\_ _ -> unreachableCodePathM) initalize

    initalize :: Object 'Client Interface_wl_display -> STM (Object 'Client Interface_wl_display, FutureEx '[SomeException] Registry)
    initalize wlDisplay = do
      registry <- liftSTMc $ createRegistry wlDisplay
      pure (wlDisplay, registry)

    wlDisplayEventHandler :: ProtocolHandle 'Client -> EventHandler_wl_display
    wlDisplayEventHandler protocol =
      EventHandler_wl_display {
        error = handleWlDisplayError protocol,
        delete_id = handleWlDisplayDeleteId protocol
      }


instance HasField "sync" (WaylandClient b) (STMc NoRetry '[SomeException] (FutureEx '[SomeException] ())) where
  getField client = lowLevelSyncFuture client.wlDisplay


-- | Get or create a client component; only one component of the same type will be created.
getClientComponent ::
  forall a b m.
  (Typeable a, MonadSTMc NoRetry '[] m) =>
  m a ->
  WaylandClient b -> m a
getClientComponent initFn client = do
  globals <- readTVar client.globals
  case Map.lookup key globals of
    Just dyn ->
      case fromDynamic @a dyn of
        Just global -> pure global
        Nothing -> unreachableCodePath
    Nothing -> do
      global <- initFn
      writeTVar client.globals (Map.insert key (toDyn global) globals)
      pure global
  where
    key = someTypeRep (Proxy @a)
