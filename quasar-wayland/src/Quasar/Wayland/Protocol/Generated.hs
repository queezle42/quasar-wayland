{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Quasar.Wayland.Protocol.Generated where

-- Imports are here to improve readability when dumping splices
import Control.Monad.Catch
import Control.Monad.STM
import Data.Binary
import Data.Void
import GHC.Records
import Quasar.Prelude
import Quasar.Wayland.Protocol.Core
import Quasar.Wayland.Protocol.TH

$(generateWaylandProcols [
  "protocols/wayland.xml",
  "protocols/xdg-shell.xml",
  "protocols/wlr-layer-shell-unstable-v1.xml",
  "protocols/linux-dmabuf-unstable-v1.xml",
  "protocols/linux-explicit-synchronization-unstable-v1.xml"
  ])
