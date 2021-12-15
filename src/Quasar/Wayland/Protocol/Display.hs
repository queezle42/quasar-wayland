module Quasar.Wayland.Protocol.Display (
  lowLevelSync,
  wlDisplayEventHandler,
) where

import Control.Monad.Catch
import Control.Monad.STM
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


lowLevelSync :: Object 'Client Interface_wl_display -> (Word32 -> STM ()) -> STM ()
lowLevelSync wlDisplay callback = do
  wlCallback <- wlDisplay.sync
  setEventHandler wlCallback EventHandler_wl_callback {
    done = callback
  }
