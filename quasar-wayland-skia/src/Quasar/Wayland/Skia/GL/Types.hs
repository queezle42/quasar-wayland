module Quasar.Wayland.Skia.GL.Types (
  GLenum,
  GLsizei,
  GLuint,
  GLint,
  GLfloat,
  GLchar,

  GlDebugCallback,
) where

import Foreign
import Foreign.C
import Quasar.Prelude

type GLenum = Word32
type GLsizei = Int32
type GLuint = Word32
type GLint = Int32
type GLfloat = Float
type GLchar = CChar

type GlDebugCallback = GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> CString -> Ptr () -> IO ()
