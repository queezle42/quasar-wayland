{-# OPTIONS_GHC -Wno-missing-export-lists #-}
--{-# OPTIONS_GHC -ddump-splices #-}

module Quasar.Wayland.Protocol where

import Quasar.Wayland.Core
import Quasar.Wayland.TH

$(generateWaylandProcol "protocols/wayland.xml")
