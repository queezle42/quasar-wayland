module Quasar.Wayland.Shared.FnWindowManager (
  FnWindowManager(..),
  FnWindow(..),
  toFnWindowManager,
  toFnWindow,
) where

import Quasar.Disposer
import Quasar.Future (Future)
import Quasar.Prelude
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi

newtype FnWindowManager b = FnWindowManager {
  newWindowFn :: WindowProperties -> WindowConfigurationCallback -> WindowRequestCallback -> STMc NoRetry '[SomeException] (FnWindow b)
}

data FnWindow b = FnWindow {
  setFullscreenFn :: Bool -> STMc NoRetry '[SomeException] (),
  commitWindowFn :: ConfigureSerial -> WindowCommit -> Owned (SurfaceCommit b) -> STMc NoRetry '[SomeException] (Future '[] ()),
  ackWindowConfigureFn :: ConfigureSerial -> STMc NoRetry '[SomeException] (),
  disposer :: Disposer
}

instance Backend b => IsWindowManager b (FnWindow b) (FnWindowManager b) where
  newWindow = (.newWindowFn)

instance Backend b => IsWindow b (FnWindow b) where
  setFullscreen = (.setFullscreenFn)
  commitWindow = (.commitWindowFn)
  ackWindowConfigure = (.ackWindowConfigureFn)

instance Disposable (FnWindow b) where
  getDisposer = (.disposer)

toFnWindowManager :: forall b w a. IsWindowManager b w a => a -> FnWindowManager b
toFnWindowManager upstream = FnWindowManager {
  newWindowFn = \props confCB reqCB -> toFnWindow <$> newWindow upstream props confCB reqCB
}

toFnWindow :: forall b a. IsWindow b a => a -> FnWindow b
toFnWindow upstream = FnWindow {
  setFullscreenFn = setFullscreen upstream,
  commitWindowFn = commitWindow upstream,
  ackWindowConfigureFn = ackWindowConfigure upstream,
  disposer = getDisposer upstream
}
