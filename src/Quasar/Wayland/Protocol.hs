module Quasar.Wayland.Protocol (
  ProtocolState,
  initialProtocolState,
  feedInput,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Quasar.Prelude
import Quasar.Wayland.TH

$(generateWaylandProcol "protocols/wayland.xml")


data ProtocolState = ProtocolState {
  bytesReceived :: Word64,
  bytesSent :: Word64
}

initialProtocolState :: ProtocolState
initialProtocolState = ProtocolState {
  bytesReceived = 0,
  bytesSent = 0
}

feedInput :: ByteString -> ProtocolState -> (ProtocolState)
feedInput bytes oldState = oldState {
  bytesReceived = oldState.bytesReceived + fromIntegral (BS.length bytes)
}
