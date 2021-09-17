module Quasar.Wayland.Display (
  ClientDisplay,
  newClientDisplay,
) where

import Control.Concurrent.STM
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Display
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Registry

data ClientDisplay = ClientDisplay {
  wlDisplay :: Object 'Client I_wl_display,
  registry :: ClientRegistry
}

newClientDisplay
  :: (IsInterfaceSide 'Client I_wl_display)
  => STM (ClientDisplay, ProtocolHandle 'Client)
newClientDisplay =
  initializeProtocol clientWlDisplayCallback \wlDisplay -> do
    registry <- createClientRegistry wlDisplay
    pure ClientDisplay {
      wlDisplay,
      registry
    }
