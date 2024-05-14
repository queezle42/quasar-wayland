{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.GL.Gles (
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Skia.GL.Debug
import Quasar.Wayland.Skia.GL.Egl
import Quasar.Wayland.Skia.GL.Types
import Quasar.Wayland.Skia.Utils.InlineC

