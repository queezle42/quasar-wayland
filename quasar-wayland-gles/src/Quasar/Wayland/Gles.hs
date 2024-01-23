{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles (
  initializeGles,

  RenderDemo,
  setupRenderDemo,
  renderDemo,

  ProxyDemo,
  setupProxyDemo,
  setupProxyDemoShader,
  proxyDemo,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Foreign.C
import GHC.Float
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Paths_quasar_wayland_gles (getDataFileName)
import Quasar.Prelude
import Quasar.Wayland.Gles.Backend
import Quasar.Wayland.Gles.Debug
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Gles.Types
import Quasar.Wayland.Gles.Utils.InlineC
import Quasar.Wayland.Shared.Surface
import System.IO (stderr)

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


data RenderDemo = RenderDemo {
  egl :: Egl,
  framebuffer :: GLuint,
  shaderProgram :: GLuint
}

data ProxyDemo = ProxyDemo {
  egl :: Egl,
  framebuffer :: GLuint,
  shaderProgram :: GLuint
}

setupRenderDemo :: Egl -> IO RenderDemo
setupRenderDemo egl = do
  framebuffer <- glGenFramebuffer

  vertexShaderSource <- BS.readFile =<< getDataFileName "shader/basic.vert"
  fragmentShaderSource <- BS.readFile =<< getDataFileName "shader/basic.frag"

  shaderProgram <- maybe (fail "Failed to compile shaders") pure =<<
    compileShaderProgram vertexShaderSource fragmentShaderSource

  [CU.block|void { glReleaseShaderCompiler(); }|]

  pure RenderDemo {
    egl,
    framebuffer,
    shaderProgram
  }

setupProxyDemo :: Egl -> IO ProxyDemo
setupProxyDemo egl = do
  vertexShader <- BS.readFile =<< getDataFileName "shader/copy.vert"
  fragmentShader <- BS.readFile =<< getDataFileName "shader/copy.frag"
  setupProxyDemoShader egl vertexShader fragmentShader

setupProxyDemoShader :: Egl -> ByteString -> ByteString -> IO ProxyDemo
setupProxyDemoShader egl vertexShader fragmentShader = do
  framebuffer <- glGenFramebuffer

  shaderProgram <- maybe (fail "Failed to compile shaders") pure =<<
    compileShaderProgram vertexShader fragmentShader

  [CU.block|void { glReleaseShaderCompiler(); }|]

  pure ProxyDemo {
    egl,
    framebuffer,
    shaderProgram
  }

renderDemo :: RenderDemo -> Int32 -> Int32 -> Double -> IO (Buffer GlesBackend)
renderDemo RenderDemo{egl, framebuffer, shaderProgram} width height time = do
  -- Create buffer texture
  texture <- glGenTexture

  -- Initialize wl_buffer texture
  [CU.block|void {
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, $(GLsizei width), $(GLsizei height), 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
  }|]

  -- Map wl_buffer texture to EGL image
  eglImage <- eglCreateGLImage egl texture

  -- Begin rendering
  [CU.block|void {
    glBindFramebuffer(GL_FRAMEBUFFER, $(GLuint framebuffer));
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, $(GLuint texture), 0);

    glClearColor(0.1, 0.1, 0.1, 1);
    glClear(GL_COLOR_BUFFER_BIT);
  }|]
  -- Framebuffer is still bound

  buffer <- glGenBuffer

  [CU.block|void {
    float vertices[] = {
      0, -1,
      1, 1,
      -1, 1,
    };
    //float vertices[] = {
    //  -1, -1,
    //  1, -1,
    //  -1, 1,
    //  1, 1,
    //};

    glBindBuffer(GL_ARRAY_BUFFER, $(GLuint buffer));

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STREAM_DRAW);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
  }|]

  scaleUniform <- glGetUniformLocation shaderProgram "scale"
  windowUniform <- glGetUniformLocation shaderProgram "window"

  let
    scale = CFloat $ double2Float ((sin time + 3) / 6)
    fwidth = fromIntegral width
    fheight = fromIntegral height
  [CU.block|void {
    glViewport(0, 0, $(GLint width), $(GLint height));
    glUseProgram($(GLuint shaderProgram));
    // should read uniform locations from the shader
    glUniform1f($(GLint scaleUniform), $(float scale));
    glUniform2f($(GLint windowUniform), $(GLfloat fwidth), $(GLfloat fheight));
  }|]

  (valid, validationInfo) <- glValidateProgram shaderProgram
  BS.hPutStr stderr validationInfo
  unless valid $ fail "Shader program is in an invalid state"

  [CU.block|void {
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 3);
    glFinish();
  }|]

  glDeleteTexture texture
  glDeleteBuffer buffer

  dmabuf <- eglExportDmabuf egl eglImage width height
  eglDestroyImage egl eglImage
  atomicallyC $ newBuffer (GlesBuffer dmabuf)


proxyDemo :: ProxyDemo -> Dmabuf -> IO (Buffer GlesBackend)
proxyDemo ProxyDemo{egl, framebuffer, shaderProgram} inputDmabuf = do
  let
    width = inputDmabuf.width
    height = inputDmabuf.height

  -- Import dmabuf to texture
  inputImage <- eglImportDmabuf egl inputDmabuf
  traceM "Mapping imported dmabuf to texture"
  inputTexture <- glGenTexture
  [CU.block|void {
    glBindTexture(GL_TEXTURE_2D, $(GLuint inputTexture));

    // load external image
    glEGLImageTargetTexture2DOES(GL_TEXTURE_2D, $(GLeglImageOES inputImage));
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    // load external image (alternative using GL_TEXTURE_EXTERNAL_OES)
    //glEGLImageTargetTexture2DOES(GL_TEXTURE_EXTERNAL_OES, $(GLeglImageOES inputImage));

    glBindTexture(GL_TEXTURE_2D, 0);
  }|]
  traceM "Mapped imported dmabuf to texture"

  -- Create buffer texture
  texture <- glGenTexture

  -- Initialize wl_buffer texture
  [CU.block|void {
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, $(GLsizei width), $(GLsizei height), 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
  }|]

  -- Map wl_buffer texture to EGL image
  eglImage <- eglCreateGLImage egl texture

  -- Begin rendering
  [CU.block|void {
    glBindFramebuffer(GL_FRAMEBUFFER, $(GLuint framebuffer));
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, $(GLuint texture), 0);

    glClearColor(0.1, 0.1, 0.1, 1);
    glClear(GL_COLOR_BUFFER_BIT);
  }|]
  -- Framebuffer is still bound

  buffer <- glGenBuffer

  [CU.block|void {
    float vertices[] = {
      -1, -1,
      1, -1,
      -1, 1,
      1, 1,
    };

    glBindBuffer(GL_ARRAY_BUFFER, $(GLuint buffer));

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STREAM_DRAW);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
  }|]

  windowUniform <- glGetUniformLocation shaderProgram "window"

  samplerUniform <- glGetUniformLocation shaderProgram "sampler"

  let
    fwidth = fromIntegral width
    fheight = fromIntegral height
  [CU.block|void {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, $(GLuint inputTexture));

    glViewport(0, 0, $(GLint width), $(GLint height));

    glUseProgram($(GLuint shaderProgram));

    glUniform2f($(GLint windowUniform), $(GLfloat fwidth), $(GLfloat fheight));
    glUniform1i($(GLint samplerUniform), 0);
  }|]

  (valid, validationInfo) <- glValidateProgram shaderProgram
  BS.hPutStr stderr validationInfo
  unless valid $ fail "Shader program is in an invalid state"

  [CU.block|void {
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  }|]

  eglDestroyImage egl inputImage
  glDeleteTexture texture
  glDeleteBuffer buffer

  [CU.block|void {
    glFinish();
  }|]

  dmabuf <- eglExportDmabuf egl eglImage width height
  eglDestroyImage egl eglImage
  atomicallyC $ newBuffer (GlesBuffer dmabuf)


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

glGenFramebuffer :: IO GLuint
glGenFramebuffer =
  alloca \ptr -> do
    [CU.exp|void { glGenFramebuffers(1, $(GLuint* ptr)) }|]
    peek ptr

glDeleteFramebuffer :: GLuint -> IO ()
glDeleteFramebuffer framebuffer =
  [CU.exp|void { glDeleteFramebuffers(1, &$(GLuint framebuffer)) }|]


glGenBuffer :: IO GLuint
glGenBuffer =
  alloca \ptr -> do
    [CU.exp|void { glGenBuffers(1, $(GLuint* ptr)) }|]
    peek ptr

glDeleteBuffer :: GLuint -> IO ()
glDeleteBuffer buffer =
  [CU.exp|void { glDeleteBuffers(1, &$(GLuint buffer)) }|]

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
