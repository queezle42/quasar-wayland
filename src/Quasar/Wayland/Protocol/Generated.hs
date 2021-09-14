{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Quasar.Wayland.Protocol.Generated where

-- Imports are here to improve readability when dumping splices
import Control.Monad.Catch
import Data.Binary
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.TH

$(generateWaylandProcol "protocols/wayland.xml")
