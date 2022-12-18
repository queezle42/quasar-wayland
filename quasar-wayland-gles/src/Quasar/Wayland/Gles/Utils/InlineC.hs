module Quasar.Wayland.Gles.Utils.InlineC (
  ctx
) where

import Data.Map.Strict as Map
import Quasar.Wayland.Gles.Egl.Types
import Quasar.Wayland.Gles.Types
import Foreign
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
  (TypeName "off_t", [t|COff|]),

  (TypeName "EGLenum", [t|EGLenum|]),
  (TypeName "EGLint", [t|EGLint|]),
  (TypeName "EGLBoolean", [t|EGLBoolean|]),
  (TypeName "EGLConfig", [t|EGLConfig|]),
  (TypeName "EGLContext", [t|EGLContext|]),
  (TypeName "EGLDisplay", [t|EGLDisplay|]),
  (TypeName "EGLDeviceEXT", [t|EGLDeviceEXT|]),
  (TypeName "EGLImage", [t|EGLImage|]),
  (TypeName "EGLLabel", [t|EGLLabel|]),
  (TypeName "EGLDEBUGPROCKHR", [t|FunPtr EglDebugCallback|]),
  ]
