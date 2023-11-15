module Quasar.Wayland.Shared.WindowManagerApi (
  IsWindowManager(..),
  IsWindow(..),

  WindowConfiguration(..),
  WindowConfigurationCallback,
  defaultWindowConfiguration,
  WindowRequest(..),
  WindowRequestCallback,
  ConfigureSerial,
  unsafeConfigureSerial,
) where

import Data.ByteString qualified as BS
import Quasar.Prelude
import Quasar.Resources (Disposable)
import Quasar.Wayland.Protocol
import Quasar.Wayland.Surface

class IsWindow b (Window b a) => IsWindowManager b a | a -> b where
  type Window b a
  newWindow :: a -> WindowConfigurationCallback -> WindowRequestCallback -> STMc NoRetry '[SomeException] (Window b a)

class (BufferBackend b, Disposable a) => IsWindow b a | a -> b where
  setTitle :: a -> WlString -> STMc NoRetry '[SomeException] ()
  setAppId :: a -> WlString -> STMc NoRetry '[SomeException] ()
  setFullscreen :: a -> Bool -> STMc NoRetry '[SomeException] ()
  commitWindowContent :: a -> ConfigureSerial -> SurfaceCommit b -> STMc NoRetry '[SomeException] ()
  ackWindowConfigure :: a -> ConfigureSerial -> STMc NoRetry '[SomeException] ()


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

type WindowConfigurationCallback = WindowConfiguration -> STMc NoRetry '[SomeException] ()

data WindowRequest = WindowRequestClose

type WindowRequestCallback = WindowRequest -> STMc NoRetry '[SomeException] ()

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
