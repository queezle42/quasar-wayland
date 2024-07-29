module Quasar.Wayland.Server.DummyOutput (
  dummyOutputGlobal,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry

dummyOutputGlobal :: Global b
dummyOutputGlobal = createGlobal maxVersion initializeDummyOutput

initializeDummyOutput :: NewObject 'Server Interface_wl_output -> STMc NoRetry '[SomeException] ()
initializeDummyOutput wlOutput = do
  wlOutput `setRequestHandler` RequestHandler_wl_output {
    release = pure ()
  }
  -- TODO name exists only from version 4
  --wlOutput.name ("Dummy" :: WlString)
  wlOutput.done
