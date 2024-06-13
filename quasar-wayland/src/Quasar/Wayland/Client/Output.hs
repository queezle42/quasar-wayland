module Quasar.Wayland.Client.Output (
  Output(..),
  getOutputs,
) where

import Quasar.Observable.Core
import Quasar.Observable.List (ObservableList)
import Quasar.Observable.List qualified as ObservableList
import Quasar.Observable.ObservableVar
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Registry
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated

data Output = Output {
  wlOutput :: Object 'Client Interface_wl_output,
  name :: Observable NoLoad '[] String,
  description :: Observable NoLoad '[] String
}

getOutputs :: WaylandClient -> STMc NoRetry '[] (ObservableList NoLoad '[] Output)
getOutputs client = getClientComponent (newOutputList client) client

newOutputList :: WaylandClient -> STMc NoRetry '[] (ObservableList NoLoad '[] Output)
newOutputList client = ObservableList.share (traverseGlobals client.registry 4 initializeOutput finalizeOutput)

initializeOutput ::
  NewObject 'Client Interface_wl_output ->
  STMc NoRetry '[] Output
initializeOutput wlOutput = do
  -- TODO abort if version == 1, mode handling is broken

  nameVar <- newObservableVar ""
  descriptionVar <- newObservableVar ""

  setEventHandler wlOutput EventHandler_wl_output {
    geometry = \_x _y _physical_width _physical_height _subpixel _make _model _transform -> pure (),
    mode = \_flags _width _height _refresh -> pure (),
    done = pure (),
    scale = \_factor -> pure (),
    name = \name -> writeObservableVar nameVar (toString name),
    description = \description -> writeObservableVar nameVar (toString description)
  }

  pure Output {
    wlOutput,
    name = toObservable nameVar,
    description = toObservable descriptionVar
  }

finalizeOutput :: Output -> STMc NoRetry '[] ()
finalizeOutput output = tryCall output.wlOutput.release
