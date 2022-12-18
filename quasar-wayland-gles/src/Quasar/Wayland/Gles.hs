{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles (
  initializeGles,
  renderDemo,
) where

import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Gles.Types
import Quasar.Wayland.Gles.Utils.InlineC
import Quasar.Wayland.Surface

C.context ctx

C.include "<stdint.h>"
C.include "<unistd.h>"

C.include "<GLES2/gl2.h>"


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

  pure egl


renderDemo :: Egl -> IO (Buffer GlesBackend)
renderDemo egl = do
  texture <- genTexture
  let
    width = 512
    height = 512
  [CU.block|void {
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, $(GLsizei width), $(GLsizei height), 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }|]

  eglImage <- eglCreateGLImage egl texture

  framebuffer <- genFramebuffer

  [CU.block|void {
    glBindFramebuffer(GL_FRAMEBUFFER, $(GLuint framebuffer));
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, $(GLuint texture), 0);

    glClearColor(1, 0, 1, 1);
    glClear(GL_COLOR_BUFFER_BIT);
    glFinish();
  }|]

  dmabuf <- exportDmabuf egl eglImage
  let glesBuffer = GlesBuffer dmabuf (fromIntegral width) (fromIntegral height)
  atomically $ newBuffer glesBuffer (traceM "Should destroy dmabuf?")


glGetString :: GLenum -> IO String
glGetString name = do
  peekCString =<< throwErrnoIfNull "eglQueryString"
    [CU.exp|char const * { glGetString($(GLenum name)) }|]


genTexture :: IO GLuint
genTexture =
  alloca \ptr -> do
    [CU.exp| void { glGenTextures(1, $(GLuint* ptr)) } |]
    peek ptr

genFramebuffer :: IO GLuint
genFramebuffer =
  alloca \ptr -> do
    [CU.exp|void { glGenFramebuffers(1, $(GLuint* ptr)) }|]
    peek ptr
