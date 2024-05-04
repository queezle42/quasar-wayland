module Quasar.Wayland.Skia.Utils.InlineC (
  ctx
) where

import Data.Map.Strict as Map
import Foreign
import Language.C.Inline.Context
import Language.C.Types
import Language.Haskell.TH
import Quasar.Prelude
import Quasar.Wayland.Skia.GL.Egl.Types
import Quasar.Wayland.Skia.GL.Types
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

  (TypeName "GLenum", [t|GLenum|]),
  (TypeName "GLsizei", [t|GLsizei|]),
  (TypeName "GLuint", [t|GLuint|]),
  (TypeName "GLint", [t|GLint|]),
  (TypeName "GLfloat", [t|GLfloat|]),
  (TypeName "GLchar", [t|GLchar|]),
  (TypeName "GLeglImageOES", [t|EGLImage|]),
  (TypeName "GLDEBUGPROCKHR", [t|FunPtr GlDebugCallback|])
  ]
