module Quasar.Wayland.Shared.FnWindowManager (
  FnWindowManager(..),
  FnWindow(..),
  toFnWindowManager,
  toFnWindow,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Shared.WindowManagerApi
import Quasar.Wayland.Surface

newtype FnWindowManager b = FnWindowManager {
  newWindowFn :: (WindowConfiguration -> STM ()) -> STM (FnWindow b)
}

data FnWindow b = FnWindow {
  setTitleFn :: WlString -> STM (),
  setAppIdFn :: WlString -> STM (),
  setFullscreenFn :: Bool -> STM (),
  commitWindowContentFn :: ConfigureSerial -> SurfaceCommit b -> STM (),
  ackWindowConfigureFn :: ConfigureSerial -> STM ()
}

instance BufferBackend b => IsWindowManager b (FnWindowManager b) where
  type Window b (FnWindowManager b) = FnWindow b
  newWindow = (.newWindowFn)

instance BufferBackend b => IsWindow b (FnWindow b) where
  setTitle = (.setTitleFn)
  setAppId = (.setAppIdFn)
  setFullscreen = (.setFullscreenFn)
  commitWindowContent = (.commitWindowContentFn)
  ackWindowConfigure = (.ackWindowConfigureFn)

toFnWindowManager :: forall b a. IsWindowManager b a => a -> FnWindowManager b
toFnWindowManager upstream = FnWindowManager {
  newWindowFn = \windowConfiguration -> toFnWindow <$> newWindow upstream windowConfiguration
}

toFnWindow :: forall b a. IsWindow b a => a -> FnWindow b
toFnWindow upstream = FnWindow {
  setTitleFn = setTitle upstream,
  setAppIdFn = setAppId upstream,
  setFullscreenFn = setFullscreen upstream,
  commitWindowContentFn = commitWindowContent upstream,
  ackWindowConfigureFn = ackWindowConfigure upstream
}
