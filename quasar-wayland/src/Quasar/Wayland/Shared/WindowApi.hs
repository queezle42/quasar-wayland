module Quasar.Wayland.Shared.WindowApi (
  -- * Window
  IsWindowManager(..),
  IsWindow(..),
  Window(..),

  WindowProperties(..),
  defaultWindowProperties,
  WindowConfiguration(..),
  WindowConfigurationCallback,
  defaultWindowConfiguration,
  WindowRequest(..),
  WindowRequestCallback,
  ConfigureSerial,
  unsafeConfigureSerial,

  -- ** Window factory
  WindowFactory,
  toWindowFactory,
) where

import Data.ByteString qualified as BS
import Quasar.Observable.Core (Observable, NoLoad)
import Quasar.Prelude
import Quasar.Resources (Disposable (getDisposer))
import Quasar.Wayland.Protocol
import Quasar.Wayland.Shared.Surface

class IsWindow b w => IsWindowManager b w a | a -> b, a -> w where
  newWindow :: a -> WindowProperties -> WindowConfigurationCallback -> WindowRequestCallback -> STMc NoRetry '[SomeException] w

-- | Wrapper for an `IsWindowManager` that can only create windows of the `Window` type.
type WindowFactory b = WindowProperties -> WindowConfigurationCallback -> WindowRequestCallback -> STMc NoRetry '[SomeException] (Window b)

toWindowFactory :: IsWindowManager b w a => a -> WindowFactory b
toWindowFactory wm props conf req = toWindow <$> newWindow wm props conf req

class (BufferBackend b, Disposable a) => IsWindow b a | a -> b where
  toWindow :: a -> Window b
  toWindow = Window
  setFullscreen :: a -> Bool -> STMc NoRetry '[SomeException] ()
  commitWindowContent :: a -> ConfigureSerial -> SurfaceCommit b -> STMc NoRetry '[SomeException] ()
  ackWindowConfigure :: a -> ConfigureSerial -> STMc NoRetry '[SomeException] ()

-- | Quantification wrapper for `IsWindow`.
data Window b = forall a. IsWindow b a => Window a

instance (BufferBackend b, Disposable (Window b)) => IsWindow b (Window b) where
  toWindow = id
  setFullscreen (Window w) = setFullscreen w
  commitWindowContent (Window w) = commitWindowContent w
  ackWindowConfigure (Window w) = ackWindowConfigure w

instance Disposable (Window b) where
  getDisposer (Window w) = getDisposer w

data WindowProperties = WindowProperties {
  title :: Observable NoLoad '[] WlString,
  appId :: Observable NoLoad '[] WlString
  --maxSize :: Observable NoLoad '[] (Int, Int),
  --minSize :: Observable NoLoad '[] (Int, Int)
}

defaultWindowProperties :: WindowProperties
defaultWindowProperties = WindowProperties {
  title = "",
  appId = ""
}


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
