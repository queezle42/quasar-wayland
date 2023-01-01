module Quasar.Wayland.Shared.WindowManagerApi (
  IsWindowManager(..),
  IsWindow(..),

  WindowConfiguration(..),
  defaultWindowConfiguration,
  ConfigureSerial,
  unsafeConfigureSerial,
) where

import Data.ByteString qualified as BS
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Surface

class IsWindow b (Window b a) => IsWindowManager b a | a -> b where
  type Window b a
  newWindow :: a -> (WindowConfiguration -> STM ()) -> STM (Window b a)

class BufferBackend b => IsWindow b a | a -> b where
  setTitle :: a -> WlString -> STM ()
  setAppId :: a -> WlString -> STM ()
  setFullscreen :: a -> Bool -> STM ()
  commitWindowContent :: a -> ConfigureSerial -> SurfaceCommit b -> STM ()
  ackWindowConfigure :: a -> ConfigureSerial -> STM ()


-- | NOTE Dummy implementation to encourage correct api design without actually implementing configure serials.
data ConfigureSerial = ConfigureSerial
  deriving (Eq, Ord)

unsafeConfigureSerial :: ConfigureSerial
unsafeConfigureSerial = ConfigureSerial


data WindowConfiguration = WindowConfiguration {
  configureSerial :: ConfigureSerial,
  width :: Int32,
  height :: Int32,
  states :: BS.ByteString
  --boundsWidth :: Int32,
  --boundsHeight :: Int32
}
  deriving Eq

-- Default values as defined by the xdg-shell protocol
defaultWindowConfiguration :: WindowConfiguration
defaultWindowConfiguration = WindowConfiguration {
  configureSerial = ConfigureSerial,
  width = 0,
  height = 0,
  states = ""
  --boundsWidth = 0,
  --boundsHeight = 0
}
