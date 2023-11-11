module Quasar.Wayland.Utils.InlineC (
  ctx
) where

import Data.Map.Strict as Map
import Language.C.Inline.Context
import Language.C.Types
import Language.Haskell.TH
import Quasar.Prelude
import System.Posix.Types (COff(..))

ctx :: Context
ctx = baseCtx <> extraTypesCtx

extraTypesCtx :: Context
extraTypesCtx =
  mempty {
    ctxTypesTable = Map.fromList types
  }

types :: [(TypeSpecifier, TypeQ)]
types = [
  (TypeName "off_t", [t|COff|])
  ]
