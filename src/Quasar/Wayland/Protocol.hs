module Quasar.Wayland.Protocol where

import Quasar.Wayland.Core
import Quasar.Wayland.TH

$(generateWaylandProcol "protocols/wayland.xml")
