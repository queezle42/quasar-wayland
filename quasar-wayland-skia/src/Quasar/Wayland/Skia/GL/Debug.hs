{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.GL.Debug (
  initializeGlDebugHandler,
) where

import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Skia.GL.Types
import Quasar.Wayland.Skia.Utils.InlineC

C.context glContext

C.include "<stdint.h>"
C.include "<unistd.h>"

C.include "<EGL/egl.h>"

C.include "<GLES2/gl2.h>"
C.include "<GLES2/gl2ext.h>"

--C.verbatim "PFNGLDEBUGMESSAGECONTROLKHRPROC glDebugMessageControlKHR;"
C.verbatim "PFNGLDEBUGMESSAGECALLBACKKHRPROC glDebugMessageCallbackKHR;"

initializeGlDebugHandler :: IO ()
initializeGlDebugHandler = do
  -- requires GL_KHR_debug
  [CU.block|void {
    glDebugMessageCallbackKHR = (PFNGLDEBUGMESSAGECALLBACKKHRPROC)eglGetProcAddress("glDebugMessageCallbackKHR");
  }|]

  -- NOTE callbackPtr is never freed - the code currently assumes the debug handler is set up once and will not change
  callbackPtr <- makeDebugCallbackPtr
  [CU.block|void {
    glDebugMessageCallbackKHR($(GLDEBUGPROCKHR callbackPtr), NULL);
  }|]
  --when (result == [CU.pure|EGLint {EGL_BAD_ATTRIBUTE}|]) $ fail "eglDebugMessageControlKHR failed: EGL_BAD_ATTRIBUTE"
  --unless (result == [CU.pure|EGLint {EGL_SUCCESS}|]) $ fail "eglDebugMessageControlKHR failed (unknown error)"

  traceIO "GL debug initialized"

makeDebugCallbackPtr :: IO (FunPtr GlDebugCallback)
makeDebugCallbackPtr = $(C.mkFunPtr [t|GlDebugCallback|]) debugCallback

debugCallback :: GlDebugCallback
--debugCallback :: GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> CString -> Ptr () -> IO ()
debugCallback _source _debugType _id _severity messageLength messagePtr _userParam = do
  -- TODO other arguments
  message <- peekCStringLen (messagePtr, fromIntegral messageLength)
  traceIO $ "GL debug message: " <> message
