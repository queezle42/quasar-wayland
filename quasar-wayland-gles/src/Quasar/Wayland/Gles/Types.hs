module Quasar.Wayland.Gles.Types (
  GLenum,
  GLsizei,
  GLuint,
  GLint,
  GLfloat,
  GLchar,
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
