{-# OPTIONS_GHC -Wno-missing-export-lists #-}
--{-# OPTIONS_GHC -ddump-splices #-}

module Quasar.Wayland.Protocol.Generated where

import Data.Binary
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.TH

$(generateWaylandProcol "protocols/wayland.xml")
