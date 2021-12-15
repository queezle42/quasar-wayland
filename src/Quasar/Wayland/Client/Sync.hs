module Quasar.Wayland.Client.Sync (
  lowLevelSync,
) where

import Control.Monad.STM
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

lowLevelSync :: Object 'Client Interface_wl_display -> (Word32 -> STM ()) -> STM ()
lowLevelSync wlDisplay callback = do
  wlCallback <- wlDisplay.sync
  setEventHandler wlCallback EventHandler_wl_callback {
    done = callback
  }
