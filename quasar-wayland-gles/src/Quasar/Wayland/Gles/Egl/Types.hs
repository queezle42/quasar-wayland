module Quasar.Wayland.Gles.Egl.Types (
  EGLenum,
  EGLint,
  EGLBoolean,
  EGLDisplay,
  EGLConfig,
  EGLContext,
  EGLDeviceEXT,
  EGLImage,
  EGLLabel,
  EglDebugCallback,
) where

import Foreign
import Foreign.C
import Quasar.Prelude

type EGLenum = Word32
type EGLint = Int32
type EGLBoolean = CUInt
type EGLDisplay = Ptr ()
type EGLConfig = Ptr ()
type EGLContext = Ptr ()
type EGLDeviceEXT = Ptr ()
type EGLImage = Ptr ()
type EGLLabel = Ptr ()

type EglDebugCallback = EGLenum -> CString -> EGLint -> EGLLabel -> EGLLabel -> CString -> IO ()
