{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.GL.Egl.Debug (
  initializeEglDebugHandler,
  toErrorMessage,

  EglException,
  eglGetError,
  isEglSuccess,
) where

import Data.List (intersperse, singleton)
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Skia.GL.Egl.Types
import Quasar.Wayland.Skia.Utils.InlineC

C.context ctx

C.include "<stdint.h>"
C.include "<unistd.h>"
C.include "<EGL/egl.h>"
C.include "<EGL/eglext.h>"

C.verbatim "PFNEGLDEBUGMESSAGECONTROLKHRPROC eglDebugMessageControlKHR;"


data EglDebugMsgType
  = EglDebugMsgCritical -- ^ EGL_DEBUG_MSG_CRITICAL_KHR
  | EglDebugMsgError -- ^ EGL_DEBUG_MSG_ERROR_KHR
  | EglDebugMsgWarning -- ^ EGL_DEBUG_MSG_WARN_KHR
  | EglDebugMsgInfo -- ^ EGL_DEBUG_MSG_INFO_KHR
  deriving stock Show


data EglException = EglException EGLint
  deriving (Eq)

instance Exception EglException

instance Show EglException where
  show (EglException value) = toErrorMessage value

eglGetError :: IO EglException
eglGetError = EglException <$> [CU.exp| EGLint { eglGetError() } |]

toDebugMessageType :: EGLint -> IO EglDebugMsgType
toDebugMessageType 0x33B9 = pure EglDebugMsgCritical
toDebugMessageType 0x33BA = pure EglDebugMsgError
toDebugMessageType 0x33BB = pure EglDebugMsgWarning
toDebugMessageType 0x33BC = pure EglDebugMsgInfo
toDebugMessageType _ = fail "Invalid EGL debug message type value"

-- | Converts an EGLint or EGLenum error value to a string representation
toErrorMessage :: (Eq a, Num a, Show a) => a -> String
toErrorMessage 0x3000 = "EGL_SUCCESS"
toErrorMessage 0x3001 = "EGL_NOT_INITIALIZED"
toErrorMessage 0x3002 = "EGL_BAD_ACCESS"
toErrorMessage 0x3003 = "EGL_BAD_ALLOC"
toErrorMessage 0x3004 = "EGL_BAD_ATTRIBUTE"
toErrorMessage 0x3005 = "EGL_BAD_CONFIG"
toErrorMessage 0x3006 = "EGL_BAD_CONTEXT"
toErrorMessage 0x3007 = "EGL_BAD_CURRENT_SURFACE"
toErrorMessage 0x3008 = "EGL_BAD_DISPLAY"
toErrorMessage 0x3009 = "EGL_BAD_MATCH"
toErrorMessage 0x300a = "EGL_BAD_NATIVE_PIXMAP"
toErrorMessage 0x300b = "EGL_BAD_NATIVE_WINDOW"
toErrorMessage 0x300c = "EGL_BAD_PARAMETER"
toErrorMessage 0x300d = "EGL_BAD_SURFACE"
toErrorMessage 0x300e = "EGL_CONTEXT_LOST"
toErrorMessage value = mconcat ["Invalid EGL error enum value (", show value, ")"]

isEglSuccess :: EglException -> Bool
isEglSuccess (EglException 0x3000) = True
isEglSuccess _ = False


initializeEglDebugHandler :: IO ()
initializeEglDebugHandler = do
  -- Requires EGL_KHR_debug
  [CU.block|
    void {
      eglDebugMessageControlKHR = (PFNEGLDEBUGMESSAGECONTROLKHRPROC)eglGetProcAddress("eglDebugMessageControlKHR");
    }
  |]

  -- NOTE callbackPtr is never freed - the code currently assumes the debug handler is set up once and will not change
  callbackPtr <- makeDebugCallbackPtr
  result <- [CU.block|
    EGLint {
      const EGLAttrib attributes[] = {
        // DEBUG_MSG_ERROR and CRITICAL are enabled by default
        EGL_DEBUG_MSG_WARN_KHR, EGL_TRUE,
        // terminate list
        EGL_NONE
      };
      return eglDebugMessageControlKHR($(EGLDEBUGPROCKHR callbackPtr), attributes);
    }
  |]
  when (result == [CU.pure|EGLint {EGL_BAD_ATTRIBUTE}|]) $ fail "eglDebugMessageControlKHR failed: EGL_BAD_ATTRIBUTE"
  unless (result == [CU.pure|EGLint {EGL_SUCCESS}|]) $ fail "eglDebugMessageControlKHR failed (unknown error)"

  traceIO $ "EGL debug initialized"

makeDebugCallbackPtr :: IO (FunPtr EglDebugCallback)
makeDebugCallbackPtr = $(C.mkFunPtr [t| EglDebugCallback |]) debugCallback

debugCallback :: EglDebugCallback
debugCallback err command messageType _threadLabel _objectLabel message = do

  parsedMessageType <- toDebugMessageType messageType
  (header, items) <- pure case parsedMessageType of
    EglDebugMsgCritical -> ("EGL critical: ", [toErrorMessage err])
    EglDebugMsgError -> ("EGL error: ", [toErrorMessage err])
    EglDebugMsgWarning -> ("EGL warning: ", [])
    EglDebugMsgInfo -> ("EGL info: ", [])

  messageItem <- if message == nullPtr then pure [] else singleton . ("message = " <>) . show <$> peekCString message

  commandItem <- singleton . ("command = " <>) <$> peekCString command

  traceIO $ mconcat (header : intersperse ", " (items <> messageItem <> commandItem))
