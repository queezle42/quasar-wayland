module Quasar.Wayland.Protocol.Display (
  wlDisplayEventHandler,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.Generated



-- | Default implementation for @wl_display@ that handles errors and confirms deleted object ids.
--
-- This is only required when manually managing the @wl_display@ interface (usually it's applied by
-- 'Quasar.Wayland.Display.newClientDisplay').
wlDisplayEventHandler :: EventHandler_wl_display
wlDisplayEventHandler = EventHandler_wl_display { error = waylandError, delete_id }
  where
    waylandError oId code message = throwM $ ServerError code (toString message)
    delete_id deletedId = pure () -- TODO confirm delete
