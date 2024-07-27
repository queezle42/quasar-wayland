{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.GL (
  GL,
) where

import Data.Map.Strict as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CPP
import Language.C.Inline.Cpp.Unsafe qualified as CPPU
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Types qualified as C
import Quasar.Prelude
import Quasar.Wayland.Dmabuf
import Quasar.Wayland.Skia
import Quasar.Wayland.Skia.CTypes
import Quasar.Wayland.Skia.GL.Debug
import Quasar.Wayland.Skia.GL.Egl
import Quasar.Wayland.Skia.GL.Egl.Types (EGLImage)
import Quasar.Wayland.Skia.GL.Types
import Quasar.Wayland.Skia.Thread
import Quasar.Wayland.Skia.Utils.InlineC (glContext)
import Text.Interpolation.Nyan (int)



C.context (glContext <> CPP.cppCtx <> C.fptrCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
    (C.TypeName "GrDirectContext", [t|GrDirectContext|]),
    (C.TypeName "SkSurface", [t|SkSurface|]),
    (C.TypeName "SkImage", [t|SkImage|])
  ]
})

C.include "<stdint.h>"
C.include "<unistd.h>"

C.include "<iostream>"

C.include "<EGL/egl.h>"
C.include "<EGL/eglext.h>"

C.include "<GLES2/gl2.h>"
C.include "<GLES2/gl2ext.h>"

C.include "include/core/SkGraphics.h"
C.include "include/core/SkSurface.h"
C.include "include/core/SkCanvas.h"
C.include "include/core/SkColorSpace.h"
C.include "include/gpu/gl/GrGLInterface.h"
C.include "include/gpu/gl/GrGLTypes.h"
--C.include "include/gpu/gl/egl/GrGLMakeEGLInterface.h"
C.include "include/gpu/gl/GrGLAssembleInterface.h"

C.include "include/gpu/GrDirectContext.h"
C.include "include/gpu/GrBackendSurface.h"
C.include "include/gpu/ganesh/gl/GrGLDirectContext.h"
C.include "include/gpu/ganesh/gl/GrGLBackendSurface.h"
-- Provides SkSurfaces namespace
C.include "include/gpu/ganesh/SkSurfaceGanesh.h"
-- Provides SkImages namespace
C.include "include/gpu/ganesh/SkImageGanesh.h"

C.verbatim "PFNGLEGLIMAGETARGETTEXTURE2DOESPROC glEGLImageTargetTexture2DOES;"

C.verbatim [int||
static GrGLFuncPtr egl_get_gl_proc(void* ctx, const char name[]) {
    SkASSERT(nullptr == ctx);
    // https://www.khronos.org/registry/EGL/extensions/KHR/EGL_KHR_get_all_proc_addresses.txt
    // eglGetProcAddress() is not guaranteed to support the querying of non-extension EGL functions.
    #define M(X) if (0 == strcmp(#X, name)) { return (GrGLFuncPtr) X; }
    M(eglGetCurrentDisplay)
    M(eglQueryString)
    M(glActiveTexture)
    M(glAttachShader)
    M(glBindAttribLocation)
    M(glBindBuffer)
    M(glBindFramebuffer)
    M(glBindRenderbuffer)
    M(glBindTexture)
    M(glBlendColor)
    M(glBlendEquation)
    M(glBlendFunc)
    M(glBufferData)
    M(glBufferSubData)
    M(glCheckFramebufferStatus)
    M(glClear)
    M(glClearColor)
    M(glClearStencil)
    M(glColorMask)
    M(glCompileShader)
    M(glCompressedTexImage2D)
    M(glCompressedTexSubImage2D)
    M(glCopyTexSubImage2D)
    M(glCreateProgram)
    M(glCreateShader)
    M(glCullFace)
    M(glDeleteBuffers)
    M(glDeleteFramebuffers)
    M(glDeleteProgram)
    M(glDeleteRenderbuffers)
    M(glDeleteShader)
    M(glDeleteTextures)
    M(glDepthMask)
    M(glDisable)
    M(glDisableVertexAttribArray)
    M(glDrawArrays)
    M(glDrawElements)
    M(glEnable)
    M(glEnableVertexAttribArray)
    M(glFinish)
    M(glFlush)
    M(glFramebufferRenderbuffer)
    M(glFramebufferTexture2D)
    M(glFrontFace)
    M(glGenBuffers)
    M(glGenFramebuffers)
    M(glGenRenderbuffers)
    M(glGenTextures)
    M(glGenerateMipmap)
    M(glGetBufferParameteriv)
    M(glGetError)
    M(glGetFramebufferAttachmentParameteriv)
    M(glGetIntegerv)
    M(glGetProgramInfoLog)
    M(glGetProgramiv)
    M(glGetRenderbufferParameteriv)
    M(glGetShaderInfoLog)
    M(glGetShaderPrecisionFormat)
    M(glGetShaderiv)
    M(glGetString)
    M(glGetUniformLocation)
    M(glIsTexture)
    M(glLineWidth)
    M(glLinkProgram)
    M(glPixelStorei)
    M(glReadPixels)
    M(glRenderbufferStorage)
    M(glScissor)
    M(glShaderSource)
    M(glStencilFunc)
    M(glStencilFuncSeparate)
    M(glStencilMask)
    M(glStencilMaskSeparate)
    M(glStencilOp)
    M(glStencilOpSeparate)
    M(glTexImage2D)
    M(glTexParameterf)
    M(glTexParameterfv)
    M(glTexParameteri)
    M(glTexParameteriv)
    M(glTexSubImage2D)
    M(glUniform1f)
    M(glUniform1fv)
    M(glUniform1i)
    M(glUniform1iv)
    M(glUniform2f)
    M(glUniform2fv)
    M(glUniform2i)
    M(glUniform2iv)
    M(glUniform3f)
    M(glUniform3fv)
    M(glUniform3i)
    M(glUniform3iv)
    M(glUniform4f)
    M(glUniform4fv)
    M(glUniform4i)
    M(glUniform4iv)
    M(glUniformMatrix2fv)
    M(glUniformMatrix3fv)
    M(glUniformMatrix4fv)
    M(glUseProgram)
    M(glVertexAttrib1f)
    M(glVertexAttrib2fv)
    M(glVertexAttrib3fv)
    M(glVertexAttrib4fv)
    M(glVertexAttribPointer)
    M(glViewport)
    #undef M
    return eglGetProcAddress(name);
}
|]


data GL

newtype GlContext = GlContext {
  egl :: Egl
}

data GlTextureStorage = GlTextureStorage {
  eglImage :: EGLImage,
  glTexture :: GLuint
}

instance IsSkiaBackend GL where
  type SkiaBackendContext GL = GlContext
  type SkiaTextureStorage GL = GlTextureStorage
  initializeSkiaBackend = initializeSkiaGles
  destroySkiaBackendContext = destroyGles
  newSkiaBackendTexture = newSkiaGLTexture
  destroySkiaTextureStorage = destroySkiaGLTexture
  exportSkiaSurfaceDmabuf = exportDmabuf
  skiaImportDmabuf = skiaGLImportDmabuf

exportDmabuf :: SkiaSurfaceState GL -> SkiaIO Dmabuf
exportDmabuf surfaceState = liftIO do
  eglExportDmabuf surfaceState.skia.context.egl surfaceState.storage.eglImage surfaceState.width surfaceState.height

-- | Initialize Skia using the OpenGL ES backend.
initializeSkiaGles :: SkiaIO (Ptr GrDirectContext, GlContext, SkiaDmabufProperties)
initializeSkiaGles = liftIO do
  egl <- initializeGles

  grDirectContext <- [CPPU.throwBlock|GrDirectContext* {
    SkGraphics::Init();

    auto grGLInterface = GrGLMakeAssembledInterface(nullptr, egl_get_gl_proc);

    // GrGLInterfaces::MakeEGL would be preferred, but it isn't exported from
    // skia when building as a shared library:
    //auto skiaGrGLInterface = GrGLInterfaces::MakeEGL();

    if (!grGLInterface) {
      std::clog << "Could not create skia GrGLInterface\n";
      return nullptr;
    }

    if (grGLInterface->validate()) {
      std::clog << "Skia GrGLInterface valid\n";
    }
    else {
      std::clog << "Skia GrGLInterface invalid\n";
      return nullptr;
    }

    sk_sp<GrDirectContext> grDirectContext = GrDirectContexts::MakeGL(std::move(grGLInterface));
    if (!grDirectContext) {
      std::clog << "GrDirectContexts::MakeGL failed\n";
      // TODO abort
      return nullptr;
    }

    std::clog << "Skia GrDirectContext initialized\n";

    // Release the base pointer from the shared pointer, since we will track the
    // lifetime by using a Haskell ForeignPtr.
    return grDirectContext.release();
  }|]

  (dmabufFormats, dmabufModifiers) <- eglQueryDmabufFormats egl
  feedback <- getDmabufFeedback egl

  let context = GlContext {
    egl
  }

  let dmabuf = SkiaDmabufProperties {
    version1Formats = dmabufFormats,
    version3FormatTable = dmabufModifiers,
    feedback
  }

  pure (grDirectContext, context, dmabuf)

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

destroyGles :: GlContext -> SkiaIO ()
destroyGles _context = pure () -- TODO


glGetString :: GLenum -> IO String
glGetString name = do
  peekCString =<< throwErrnoIfNull "eglQueryString"
    [CU.exp|const char* { (const char*) glGetString($(GLenum name)) }|]


glGenTexture :: IO GLuint
glGenTexture =
  alloca \ptr -> do
    [CU.exp| void { glGenTextures(1, $(GLuint* ptr)) } |]
    peek ptr

glDeleteTexture :: GLuint -> IO ()
glDeleteTexture texture =
  [CU.exp|void { glDeleteTextures(1, &$(GLuint texture)) }|]


newSkiaGLTexture :: Skia GL -> Int32 -> Int32 -> SkiaIO (Ptr SkSurface, SkiaTextureStorage GL)
newSkiaGLTexture Skia{grDirectContext, context} width height = liftIO do
  texture <- glGenTexture

  let
    cWidth = fromIntegral width :: CInt
    cHeight = fromIntegral height :: CInt

  [CU.block|void {
    GLuint oldTexture;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, (GLint*)&oldTexture);
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8_OES, (GLsizei)$(int cWidth), (GLsizei)$(int cHeight), 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
    glBindTexture(GL_TEXTURE_2D, oldTexture);
  }|]

  -- Map texture to EGL image
  eglImage <- eglCreateGLImage context.egl texture

  skSurface <- [CPPU.throwBlock|SkSurface* {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);

    GrGLTextureInfo textureInfo;
    textureInfo.fTarget = GL_TEXTURE_2D;
    textureInfo.fID = $(GLuint texture);
    textureInfo.fFormat = GL_RGBA8_OES;

    GrBackendTexture backendTexture = GrBackendTextures::MakeGL($(int cWidth), $(int cHeight), skgpu::Mipmapped::kNo, textureInfo);

    std::clog << "Skia GrBackendTexture created from GL texture\n";

    GrSurfaceOrigin origin = GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin;
    int sampleCnt = 0;
    SkColorType colorType = kRGBA_8888_SkColorType;
    sk_sp<SkColorSpace> colorSpace = nullptr; //SkColorSpace::MakeSRGB();
    SkSurfaceProps* surfaceProps = nullptr; //new SkSurfaceProps();
    sk_sp<SkSurface> skSurface = SkSurfaces::WrapBackendTexture(grDirectContext, backendTexture, origin, sampleCnt, colorType, colorSpace, surfaceProps);
    if (!skSurface) {
      std::clog << "SkSurfaces::WrapBackendTexture failed\n";
      return nullptr;
    }

    // Release the base pointer from the shared pointer, since we will track the
    // lifetime by using a Haskell ForeignPtr.
    return skSurface.release();
  }|]

  when (skSurface == nullPtr) do
    throwIO $ userError "Failed to create skia surface"

  pure (skSurface, GlTextureStorage {eglImage, glTexture = texture})

destroySkiaGLTexture :: Skia GL -> SkiaSurfaceState GL -> SkiaIO ()
destroySkiaGLTexture skia surfaceState = liftIO do
  eglDestroyImage surfaceState.skia.context.egl surfaceState.storage.eglImage
  glDeleteTexture surfaceState.storage.glTexture


skiaGLImportDmabuf :: Skia GL -> Dmabuf -> SkiaIO (Ptr SkImage)
skiaGLImportDmabuf skia dmabuf = liftIO do
  let grDirectContext = skia.grDirectContext

  let
    width = dmabuf.width
    cWidth = fromIntegral width
    height = dmabuf.height
    cHeight = fromIntegral height

  -- Import dmabuf to texture
  eglImage <- eglImportDmabuf skia.context.egl dmabuf

  traceIO "Mapping imported dmabuf to texture"
  texture <- glGenTexture
  [CU.block|void {
    GLuint oldTexture;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, (GLint*)&oldTexture);

    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));

    // load external image
    glEGLImageTargetTexture2DOES(GL_TEXTURE_2D, $(GLeglImageOES eglImage));

    // load external image (alternative using GL_TEXTURE_EXTERNAL_OES)
    //glEGLImageTargetTexture2DOES(GL_TEXTURE_EXTERNAL_OES, $(GLeglImageOES eglImage));

    glBindTexture(GL_TEXTURE_2D, oldTexture);
  }|]
  traceIO "Mapped imported dmabuf to texture"

  eglDestroyImage skia.context.egl eglImage

  skImage <- [CPPU.throwBlock|SkImage* {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);

    GrGLTextureInfo textureInfo;
    textureInfo.fTarget = GL_TEXTURE_2D;
    textureInfo.fID = $(GLuint texture);
    textureInfo.fFormat = GL_RGB8_OES;

    GrBackendTexture backendTexture = GrBackendTextures::MakeGL($(int cWidth), $(int cHeight), skgpu::Mipmapped::kNo, textureInfo);
    std::clog << "Skia GrBackendTexture created from GL texture\n";


    GrSurfaceOrigin origin = GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin;
    SkColorType colorType = kRGB_888x_SkColorType;

    sk_sp<SkImage> skImage = SkImages::AdoptTextureFrom(grDirectContext, backendTexture, origin, colorType);

    std::clog << "Skia image created from external texture\n";

    // Release the base pointer from the shared pointer, since we will track the
    // lifetime by using a Haskell ForeignPtr.
    return skImage.release();
  }|]

  when (skImage == nullPtr) do
    throwIO $ userError "Failed to create skia image from external texture"

  pure skImage
