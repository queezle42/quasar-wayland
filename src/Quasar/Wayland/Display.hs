module Quasar.Wayland.Display (
  ClientDisplay,
  newClientDisplay,
) where

import Control.Concurrent.STM
import GHC.Records
import Quasar.Awaitable
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Display
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Registry

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
