module Quasar.Wayland.Common.WindowManagerApi (
  IsWindowManager(..),
  IsWindow(..),

  WindowConfiguration(..),
  defaultWindowConfiguration,
  ConfigureSerial,
  unsafeConfigureSerial,
) where

import Data.ByteString qualified as BS
import Quasar.Prelude
import Quasar.Wayland.Surface

class IsWindow (Window a) => IsWindowManager a where
  type Window a
  newWindow :: a -> (WindowConfiguration -> STM ()) -> STM (Window a)

class IsWindow a where
  type WindowBackend a
  setTitle :: a -> String -> STM ()
  commitWindowContent :: a -> ConfigureSerial -> SurfaceCommit (WindowBackend a) -> STM ()
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
