module Quasar.Wayland.Shared.DummyWindowManager (
  DummyWindowManager,
  newDummyWindowManager,
  DummyWindow,
) where

import Quasar.Disposer
import Quasar.Prelude
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi

data DummyWindowManager b = DummyWindowManager

newDummyWindowManager :: forall b m. Monad m => m (DummyWindowManager b)
newDummyWindowManager = pure DummyWindowManager

data DummyWindow b = DummyWindow

instance Backend b => IsWindowManager b (DummyWindow b) (DummyWindowManager b) where
  newWindow _wm _properties _commit configCallback _requestCallback = do
    traceM "New window created"
    configCallback defaultWindowConfiguration
    pure DummyWindow

instance Backend b => IsWindow b (DummyWindow b) where
  setFullscreen _window _fullscreen = pure ()
  commitWindow _window _configureSerial _windowCommit surfaceCommit = do
    disposeEventually_ surfaceCommit
    pure () <$ traceM "Window comitted"
  ackWindowConfigure _window _configureSerial = traceM "Window configure acked"

instance Disposable (DummyWindow b) where
  getDisposer _ = mempty
