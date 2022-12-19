{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles (
  initializeGles,
  renderDemo,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
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
import System.IO (stderr)

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

  let glesBuffer = GlesBuffer dmabuf (fromIntegral width) (fromIntegral height)
  atomically $ newBuffer glesBuffer (traceM "Should destroy dmabuf?")
  dmabuf <- eglExportDmabuf egl eglImage width height


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
glCompileNewShader :: GLenum -> ByteString -> IO (Maybe GLuint, ByteString)
glCompileNewShader shaderType source = do
  shader <- [CU.exp|GLuint { glCreateShader($(GLenum shaderType)) }|]
  when (shader == 0) $ fail "Failed to create shader"

  BS.useAsCString source \ptr -> with ptr \ptrPtr ->
    [CU.block|void {
      glShaderSource($(GLuint shader), 1, $(const GLchar* const* ptrPtr), NULL);
      glCompileShader($(GLuint shader));
    }|]

  success <- alloca \successPtr -> do
    [CU.block|void {
      glGetShaderiv($(GLuint shader), GL_COMPILE_STATUS, $(GLint* successPtr));
    }|]
    peek successPtr

  infoLogLength <- alloca \infoLogLengthPtr -> do
    [CU.block|void {
      glGetShaderiv($(GLuint shader), GL_INFO_LOG_LENGTH, $(GLint* infoLogLengthPtr));
    }|]
    peek infoLogLengthPtr

  infoLog <- allocaBytes (fromIntegral infoLogLength) \infoLogPtr -> do
    [CU.block|void {
      glGetShaderInfoLog($(GLuint shader), $(GLsizei infoLogLength), NULL, $(GLchar* infoLogPtr));
    }|]
    BS.packCString infoLogPtr

  if success > 0
    then pure (Just shader, infoLog)
    else do
      glDeleteShader shader
      pure (Nothing, infoLog)

glDeleteShader :: GLuint -> IO ()
glDeleteShader shader =
  [CU.block|void {
    glDeleteShader($(GLuint shader));
  }|]

glLinkNewProgram :: GLuint -> GLuint -> IO (Maybe GLuint, ByteString)
glLinkNewProgram vertexShader fragmentShader = do
  program <- [CU.block|GLuint { glCreateProgram(); }|]
  when (program == 0) $ fail "Failed to create shader program"

  [CU.block|void {
    glAttachShader($(GLuint program), $(GLuint vertexShader));
    glAttachShader($(GLuint program), $(GLuint fragmentShader));
    glLinkProgram($(GLuint program));
  }|]

  success <- alloca \successPtr -> do
    [CU.block|void {
      glGetProgramiv($(GLuint program), GL_LINK_STATUS, $(GLint* successPtr));
    }|]
    peek successPtr

  infoLog <- glGetProgramInfoLog program

  if success > 0
    then pure (Just program, infoLog)
    else do
      glDeleteProgram program
      pure (Nothing, infoLog)

glDeleteProgram :: GLuint -> IO ()
glDeleteProgram program =
  [CU.block|void {
    glDeleteProgram($(GLuint program));
  }|]

glValidateProgram :: GLuint -> IO (Bool, ByteString)
glValidateProgram program = do
  [CU.block|void { glValidateProgram($(GLuint program)); }|]

  validateStatus <- alloca \validateStatusPtr -> do
    [CU.block|void {
      glGetProgramiv($(GLuint program), GL_LINK_STATUS, $(GLint* validateStatusPtr));
    }|]
    peek validateStatusPtr

  infoLog <- glGetProgramInfoLog program

  let isValid = validateStatus == [CU.pure|GLint { GL_TRUE }|]

  pure (isValid, infoLog)

glGetProgramInfoLog :: GLuint -> IO ByteString
glGetProgramInfoLog program = do
  infoLogLength <- alloca \infoLogLengthPtr -> do
    [CU.block|void {
      glGetProgramiv($(GLuint program), GL_INFO_LOG_LENGTH, $(GLint* infoLogLengthPtr));
    }|]
    peek infoLogLengthPtr

  allocaBytes (fromIntegral infoLogLength) \infoLogPtr -> do
    [CU.block|void {
      glGetProgramInfoLog($(GLuint program), $(GLsizei infoLogLength), NULL, $(GLchar* infoLogPtr));
    }|]
    BS.packCString infoLogPtr

-- | All-in-one wrapper to compile a shader program from vertex- and fragment
-- shader sources. Logs messages to stderr.
compileShaderProgram :: ByteString -> ByteString -> IO (Maybe GLuint)
compileShaderProgram vertexShaderSource fragmentShaderSource = do
  traceIO "Compiling vertex shader..."
  (vertexShader, vertexShaderMessages) <- glCompileNewShader [CU.pure|GLenum { GL_VERTEX_SHADER }|] vertexShaderSource
  BS.hPutStr stderr vertexShaderMessages

  traceIO "Compiling fragment shader..."
  (fragmentShader, fragmentShaderMessages) <- glCompileNewShader [CU.pure|GLenum { GL_FRAGMENT_SHADER }|] fragmentShaderSource
  BS.hPutStr stderr fragmentShaderMessages

  shaderProgram <-
    join <$> forM vertexShader \vs ->
    join <$> forM fragmentShader \fs -> do
      traceIO "Linking shader program..."
      (shaderProgram, shaderProgramMessages) <- glLinkNewProgram vs fs
      BS.hPutStr stderr shaderProgramMessages
      pure shaderProgram

  mapM_ glDeleteShader vertexShader
  mapM_ glDeleteShader fragmentShader

  pure shaderProgram


glGetUniformLocation :: GLuint -> String -> IO GLint
glGetUniformLocation program name =
  withCString name \namePtr ->
    [CU.block|GLint {
      glGetUniformLocation($(GLuint program), $(const GLchar* namePtr));
    }|]
