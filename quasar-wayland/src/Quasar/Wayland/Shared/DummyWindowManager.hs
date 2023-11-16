module Quasar.Wayland.Shared.DummyWindowManager (
  DummyWindowManager,
  newDummyWindowManager,
  DummyWindow,
) where

import Quasar.Prelude
import Quasar.Wayland.Shared.WindowApi
import Quasar.Wayland.Surface
import Quasar.Resources (Disposable (getDisposer))

data DummyWindowManager b = DummyWindowManager

newDummyWindowManager :: forall b m. Monad m => m (DummyWindowManager b)
newDummyWindowManager = pure DummyWindowManager

data DummyWindow b = DummyWindow

instance BufferBackend b => IsWindowManager b (DummyWindowManager b) where
  type Window b (DummyWindowManager b) = DummyWindow b
  newWindow _wm _properties configCallback _requestCallback = do
    traceM "New window created"
    configCallback defaultWindowConfiguration
    pure DummyWindow

instance BufferBackend b => IsWindow b (DummyWindow b) where
  setFullscreen _window _fullscreen = pure ()
  commitWindowContent _window _configureSerial _commit = traceM "Window comitted"
  ackWindowConfigure _window _configureSerial = traceM "Window configure acked"

instance Disposable (DummyWindow b) where
  getDisposer _ = mempty
