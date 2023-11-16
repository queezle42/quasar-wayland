module Quasar.Wayland.Shared.FnWindowManager (
  FnWindowManager(..),
  FnWindow(..),
  toFnWindowManager,
  toFnWindow,
) where

import Quasar.Prelude
import Quasar.Wayland.Shared.WindowApi
import Quasar.Wayland.Surface
import Quasar.Resources (Disposer, Disposable(getDisposer))

newtype FnWindowManager b = FnWindowManager {
  newWindowFn :: WindowProperties -> WindowConfigurationCallback -> WindowRequestCallback -> STMc NoRetry '[SomeException] (FnWindow b)
}

data FnWindow b = FnWindow {
  setFullscreenFn :: Bool -> STMc NoRetry '[SomeException] (),
  commitWindowContentFn :: ConfigureSerial -> SurfaceCommit b -> STMc NoRetry '[SomeException] (),
  ackWindowConfigureFn :: ConfigureSerial -> STMc NoRetry '[SomeException] (),
  disposer :: Disposer
}

instance BufferBackend b => IsWindowManager b (FnWindowManager b) where
  type Window b (FnWindowManager b) = FnWindow b
  newWindow = (.newWindowFn)

instance BufferBackend b => IsWindow b (FnWindow b) where
  setFullscreen = (.setFullscreenFn)
  commitWindowContent = (.commitWindowContentFn)
  ackWindowConfigure = (.ackWindowConfigureFn)

instance Disposable (FnWindow b) where
  getDisposer = (.disposer)

toFnWindowManager :: forall b a. IsWindowManager b a => a -> FnWindowManager b
toFnWindowManager upstream = FnWindowManager {
  newWindowFn = \props confCB reqCB -> toFnWindow <$> newWindow upstream props confCB reqCB
}

toFnWindow :: forall b a. IsWindow b a => a -> FnWindow b
toFnWindow upstream = FnWindow {
  setFullscreenFn = setFullscreen upstream,
  commitWindowContentFn = commitWindowContent upstream,
  ackWindowConfigureFn = ackWindowConfigure upstream,
  disposer = getDisposer upstream
}
