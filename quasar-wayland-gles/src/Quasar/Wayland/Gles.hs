{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles (
  Egl,
  initializeGles,

  -- * Client
  ClientDmabufSingleton,
  newClientDmabufSingleton,
  getClientDmabufSingleton,
  awaitSupportedFormats,

  -- * Internal for skia
  glGenTexture,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Resources (Disposable, TDisposable)
import Quasar.Resources.DisposableVar
import Quasar.Wayland.Gles.Debug
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Gles.Types
import Quasar.Wayland.Gles.Utils.InlineC

C.context ctx

C.include "<stdint.h>"
C.include "<unistd.h>"

C.include "<EGL/egl.h>"
C.include "<GLES2/gl2.h>"
C.include "<GLES2/gl2ext.h>"

C.verbatim "PFNGLEGLIMAGETARGETTEXTURE2DOESPROC glEGLImageTargetTexture2DOES;"

initializeGles :: IO Egl
initializeGles = do
  egl <- initializeEgl
  vendor <- glGetString [CU.pure|GLenum { GL_VENDOR }|]
  renderer <- glGetString [CU.pure|GLenum { GL_RENDERER }|]
  version <- glGetString [CU.pure|GLenum { GL_VERSION }|]
  shadingLanguageVersion <- glGetString [CU.pure|GLenum { GL_SHADING_LANGUAGE_VERSION }|]
  extensionsString <- glGetString [CU.pure|GLenum { GL_EXTENSIONS }|]
  traceIO $ "GL vendor: " <> vendor
  traceIO $ "GL renderer: " <> renderer
  traceIO $ "GL version: " <> version
  traceIO $ "GL shading language version: " <> shadingLanguageVersion
  traceIO $ "GL extensions: " <> extensionsString

  let
    extensions = Set.fromList (words extensionsString)
    requiredExtensions :: Set String = Set.fromList [
      "GL_KHR_debug",
      "GL_OES_required_internalformat",
      "GL_OES_EGL_image"
      --"GL_OES_EGL_image_external"
      ]
    missingExtensions = Set.difference requiredExtensions extensions

  unless (Set.null missingExtensions) $
    fail $ "Missing GL extensions: " <> intercalate " " missingExtensions

  initializeGlDebugHandler

  [CU.block|void {
    glEGLImageTargetTexture2DOES = (PFNGLEGLIMAGETARGETTEXTURE2DOESPROC)eglGetProcAddress("glEGLImageTargetTexture2DOES");
  }|]

  pure egl


glGetString :: GLenum -> IO String
glGetString name = do
  peekCString =<< throwErrnoIfNull "eglQueryString"
    [CU.exp|char const * { glGetString($(GLenum name)) }|]


glGenTexture :: IO GLuint
glGenTexture =
  alloca \ptr -> do
    [CU.exp| void { glGenTextures(1, $(GLuint* ptr)) } |]
    peek ptr

glDeleteTexture :: GLuint -> IO ()
glDeleteTexture texture =
  [CU.exp|void { glDeleteTextures(1, &$(GLuint texture)) }|]


newtype GlesRenderedFrame = GlesRenderedFrame (TDisposableVar Dmabuf)
  deriving (Eq, Hashable, Disposable, TDisposable)


--instance ClientBufferBackend GlesBackend where
--  type ClientBufferManager GlesBackend = ClientDmabufSingleton
--  type RenderedFrame GlesBackend = GlesRenderedFrame
--  type ExportBufferId GlesBackend = Unique
--  newClientBufferManager = newClientDmabufSingleton
--
--  renderFrame frame = atomicallyC do
--    tryReadRc frame >>= \case
--      Nothing -> undefined
--      Just (GlesFrame dmabuf) -> do
--        var <- newTDisposableVar dmabuf undefined
--        newRc (GlesRenderedFrame var)
--
--  getExportBufferId = undefined
--
--  exportWlBuffer dmabufSingleton (GlesRenderedFrame var) = atomically do
--    tryReadTDisposableVar var >>= \case
--      Nothing -> undefined
--      Just dmabuf -> liftSTMc $ sharedDmabufExportWlBuffer dmabufSingleton dmabuf
--
--  syncExportBuffer = undefined
--
--  getExportBufferDestroyedFuture = pure . isDisposed
