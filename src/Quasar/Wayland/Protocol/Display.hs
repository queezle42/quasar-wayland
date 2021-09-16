module Quasar.Wayland.Protocol.Display (
  clientWlDisplayCallback,
) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Data.ByteString.UTF8 qualified as BS
import Data.HashMap.Strict qualified as HM
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Registry


-- | Default implementation for @wl_display@ that handles errors and confirms deleted object ids.
--
-- This is only required when manually managing the @wl_display@ interface (usually it's applied by
-- 'Quasar.Wayland.Display.newClientDisplay').
clientWlDisplayCallback :: IsInterfaceSide 'Client I_wl_display => Callback 'Client I_wl_display
clientWlDisplayCallback = internalFnCallback handler
  where
    -- | wl_display is specified to never change, so manually specifying the callback is safe
    handler :: Object 'Client I_wl_display -> E_wl_display -> ProtocolM 'Client ()
    -- TODO parse oId
    handler _ (E_wl_display_error oId code message) = throwM $ ServerError code (BS.toString message)
    handler _ (E_wl_display_delete_id deletedId) = pure () -- TODO confirm delete
