module Quasar.Wayland.Server.DataTransfer (
  dataDeviceManagerGlobal,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry

dataDeviceManagerGlobal :: Global
dataDeviceManagerGlobal = createGlobal maxVersion initializeDataDeviceManager

initializeDataDeviceManager :: NewObject 'Server Interface_wl_data_device_manager -> STMc NoRetry '[SomeException] ()
initializeDataDeviceManager wlDataDeviceManager = do
  wlDataDeviceManager `setRequestHandler` RequestHandler_wl_data_device_manager {
    create_data_source = undefined,
    get_data_device = initializeDataDevice
  }

initializeDataDevice ::
  NewObject 'Server Interface_wl_data_device ->
  Object 'Server Interface_wl_seat ->
  STMc NoRetry '[SomeException] ()
initializeDataDevice wlDataDevice wlSeat = do
  wlDataDevice `setRequestHandler` RequestHandler_wl_data_device {
    start_drag = undefined,
    set_selection = undefined,
    release = pure ()
  }
