module Quasar.Wayland.Protocol.Display (
  clientWlDisplayWireCallback,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated


-- | Default implementation for @wl_display@ that handles errors and confirms deleted object ids.
--
-- This is only required when manually managing the @wl_display@ interface (usually it's applied by
-- 'Quasar.Wayland.Display.newClientDisplay').
clientWlDisplayWireCallback :: IsInterfaceSide 'Client Interface_wl_display => WireCallback 'Client Interface_wl_display
clientWlDisplayWireCallback = internalFnWireCallback handler
  where
    -- | wl_display is specified to never change, so manually specifying the callback is safe
    handler :: Object 'Client Interface_wl_display -> WireEvent_wl_display -> ProtocolM 'Client ()
    -- TODO parse oId
    handler _ (WireEvent_wl_display_error oId code message) = throwM $ ServerError code (toString message)
    handler _ (WireEvent_wl_display_delete_id deletedId) = pure () -- TODO confirm delete
