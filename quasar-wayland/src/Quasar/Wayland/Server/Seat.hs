module Quasar.Wayland.Server.Seat (
  dummySeatGlobal,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry

dummySeatGlobal :: Global b
dummySeatGlobal = createGlobal maxVersion initializeDummySeat

initializeDummySeat :: NewObject 'Server Interface_wl_seat -> STMc NoRetry '[SomeException] ()
initializeDummySeat wlSeat = do
  wlSeat `setRequestHandler` RequestHandler_wl_seat {
    get_pointer = undefined,
    get_keyboard = undefined,
    get_touch = undefined,
    release = pure ()
  }
  wlSeat.name "dummy-seat"
  wlSeat.capabilities 0
