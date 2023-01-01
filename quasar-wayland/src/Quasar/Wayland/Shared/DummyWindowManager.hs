module Quasar.Wayland.Shared.DummyWindowManager (
  DummyWindowManager,
  newDummyWindowManager,
  DummyWindow,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Shared.WindowManagerApi
import Quasar.Wayland.Surface

data DummyWindowManager b = DummyWindowManager

newDummyWindowManager :: forall b m. Monad m => m (DummyWindowManager b)
newDummyWindowManager = pure DummyWindowManager

data DummyWindow b = DummyWindow

instance BufferBackend b => IsWindowManager b (DummyWindowManager b) where
  type Window b (DummyWindowManager b) = DummyWindow b
  newWindow _wm configCallback = do
    traceM "New window created"
    configCallback defaultWindowConfiguration
    pure DummyWindow

instance BufferBackend b => IsWindow b (DummyWindow b) where
  setTitle _window title = traceM $ mconcat ["Window title: \"", toString title, "\""]
  setAppId _window appId = traceM $ mconcat ["App id: \"", toString appId, "\""]
  setFullscreen _window _fullscreen = pure ()
  commitWindowContent _window _configureSerial _commit = traceM "Window comitted"
  ackWindowConfigure _window _configureSerial = traceM "Window configure acked"
