module Quasar.Wayland.Client.Seat (
  Seat(..),
  getSeats,
) where

import Quasar.Future
import Quasar.Observable.Core
import Quasar.Observable.List (ObservableList)
import Quasar.Observable.List qualified as ObservableList
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Registry
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

data Seat = Seat {
  wlSeat :: Object 'Client Interface_wl_seat,
  name :: Future '[] String
}

getSeats :: WaylandClient b -> STMc NoRetry '[] (ObservableList NoLoad '[] Seat)
getSeats client = getClientComponent (newSeatList client) client

newSeatList :: WaylandClient b -> STMc NoRetry '[] (ObservableList NoLoad '[] Seat)
newSeatList client = ObservableList.share (traverseGlobals client.registry 9 initializeSeat finalizeSeat)

initializeSeat ::
  NewObject 'Client Interface_wl_seat ->
  STMc NoRetry '[] Seat
initializeSeat wlSeat = do
  nameVar <- newPromise

  setEventHandler wlSeat EventHandler_wl_seat {
    capabilities = \_ -> pure (),
    name = \name -> tryFulfillPromise_ nameVar (toString name)
  }

  pure Seat {
    wlSeat,
    name = toFuture nameVar
  }

finalizeSeat :: Seat -> STMc NoRetry '[] ()
finalizeSeat seat = tryCall seat.wlSeat.release
