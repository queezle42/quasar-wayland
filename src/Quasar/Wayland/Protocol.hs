module Quasar.Wayland.Protocol (
  -- * A pure implementation of the Wayland wire protocol
  createClientStateWithRegistry
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.State (StateT, runStateT)
import Data.ByteString.UTF8 (toString)
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated


createClientStateWithRegistry :: STM (ProtocolHandle 'Client)
createClientStateWithRegistry = do
  (wlRegistry, protocolHandle) <- initializeProtocol wlDisplayCallback createRegistry
  pure protocolHandle
  where
    createRegistry :: Object 'Client I_wl_display -> ProtocolM 'Client (Object 'Client I_wl_registry)
    createRegistry wlDisplay = do
      (wlRegistry, newId) <- newObject @'Client @I_wl_registry (traceCallback ignoreMessage)
      sendMessage wlDisplay $ R_wl_display_get_registry newId

      pure wlRegistry

    wlDisplayCallback :: IsInterfaceSide 'Client I_wl_display => Callback 'Client I_wl_display
    wlDisplayCallback = internalFnCallback handler
      where
        -- | wl_display is specified to never change, so manually specifying the callback is safe
        handler :: Object 'Client I_wl_display -> E_wl_display -> ProtocolM 'Client ()
        -- TODO parse oId
        handler _ (E_wl_display_error oId code message) = throwM $ ServerError code (toString message)
        handler _ (E_wl_display_delete_id deletedId) = pure () -- TODO confirm delete
