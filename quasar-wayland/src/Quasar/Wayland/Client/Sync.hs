module Quasar.Wayland.Client.Sync (
  lowLevelSync,
  lowLevelSyncFuture,
) where

import Control.Monad.STM
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

lowLevelSync :: Object 'Client Interface_wl_display -> (Word32 -> STMc NoRetry '[SomeException] ()) -> STMc NoRetry '[SomeException] ()
lowLevelSync wlDisplay callback = do
  wlCallback <- wlDisplay.sync
  setEventHandler wlCallback EventHandler_wl_callback {
    done = callback
  }

lowLevelSyncFuture :: Object 'Client Interface_wl_display -> STMc NoRetry '[SomeException] (FutureEx '[SomeException] ())
lowLevelSyncFuture wlDisplay = do
  var <- newPromise
  -- TODO fulfill promise with exception on client disconnect
  lowLevelSync wlDisplay \_ -> fulfillPromise var (Right ())
  pure $ toFutureEx var
