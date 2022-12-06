{-# LANGUAGE TemplateHaskell #-}

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

emptyCtx :: Context
emptyCtx = Context {
  ctxTypesTable = mempty,
  ctxAntiQuoters = mempty,
  ctxOutput = mempty,
  ctxForeignSrcLang = Nothing,
  ctxEnableCpp = False
}

extraTypesCtx :: Context
extraTypesCtx =
  emptyCtx {
    ctxTypesTable = Map.fromList types
  }

types :: [(TypeSpecifier, TypeQ)]
types = [
  (TypeName "off_t", [t|COff|])
  ]
