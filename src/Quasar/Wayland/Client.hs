module Quasar.Wayland.Client (
  WaylandClient(display),
  connectWaylandClient,
  newWaylandClient,
  connectWaylandSocket,

  -- * wl_display
  ClientDisplay,
  newClientDisplay,

  -- * wl_registry
  ClientRegistry,
  createClientRegistry,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Data.HashMap.Strict qualified as HM
import Data.Tuple (swap)
import GHC.Records
import Network.Socket (Socket)
import Quasar
import Quasar.Prelude
import Quasar.Wayland.Client.Socket
import Quasar.Wayland.Connection
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Display
import Quasar.Wayland.Protocol.Generated


data WaylandClient = WaylandClient {
  connection :: WaylandConnection 'Client,
  display :: ClientDisplay
}

instance IsResourceManager WaylandClient where
  toResourceManager (WaylandClient connection _) = toResourceManager connection

instance IsDisposable WaylandClient where
  toDisposable (WaylandClient connection _) = toDisposable connection

newWaylandClient :: MonadResourceManager m => Socket -> m WaylandClient
newWaylandClient socket = do
  (display, connection) <- newWaylandConnection newClientDisplay socket
  pure WaylandClient {
    connection,
    display
  }

connectWaylandClient :: MonadResourceManager m => m WaylandClient
connectWaylandClient = mask_ do
  socket <- liftIO connectWaylandSocket
  newWaylandClient socket



-- * wl_display

data ClientDisplay = ClientDisplay {
  wlDisplay :: Object 'Client Interface_wl_display,
  registry :: ClientRegistry
}

newClientDisplay :: STM (ClientDisplay, ProtocolHandle 'Client)
newClientDisplay =
  initializeProtocol wlDisplayEventHandler \wlDisplay -> do
    registry <- createClientRegistry wlDisplay
    pure ClientDisplay {
      wlDisplay,
      registry
    }

instance HasField "sync" ClientDisplay (STM (Awaitable ())) where
  getField display = do
    var <- newAsyncVarSTM
    wlCallback <- display.wlDisplay.sync
    setEventHandler wlCallback EventHandler_wl_callback {
      done = const $ putAsyncVarSTM_ var ()
    }
    pure $ toAwaitable var



-- * wl_registry

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
