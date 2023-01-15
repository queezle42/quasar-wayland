module Quasar.Wayland.Client.Sync (
  lowLevelSync,
  lowLevelSyncFuture,
) where

import Control.Monad.STM
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

lowLevelSync :: Object 'Client Interface_wl_display -> (Word32 -> STM ()) -> STM ()
lowLevelSync wlDisplay callback = do
  wlCallback <- wlDisplay.sync
  setEventHandler wlCallback EventHandler_wl_callback {
    done = callback
  }

lowLevelSyncFuture :: Object 'Client Interface_wl_display -> STM (FutureEx '[SomeException] ())
lowLevelSyncFuture wlDisplay = do
  var <- newPromiseSTM
  -- TODO fulfill promise with exception on client disconnect
  lowLevelSync wlDisplay \_ -> fulfillPromiseSTM var (Right ())
  pure $ toFutureEx var
