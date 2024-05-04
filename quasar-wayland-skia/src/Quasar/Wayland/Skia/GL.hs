{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.GL (
  GL,
) where

import Data.Map.Strict as Map
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CPP
import Language.C.Inline.Cpp.Unsafe qualified as CPPU
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Types qualified as C
import Quasar.Prelude
import Quasar.Wayland.Skia
import Quasar.Wayland.Skia.CTypes
import Quasar.Wayland.Skia.Dmabuf
import Quasar.Wayland.Skia.GL.Egl
import Quasar.Wayland.Skia.GL.Egl.Types (EGLImage)
import Quasar.Wayland.Skia.GL.Types
import Quasar.Wayland.Skia.GL.Gles (initializeGles, glGenTexture)
import Quasar.Wayland.Skia.Thread
import Text.Interpolation.Nyan (int)


C.context (CPP.cppCtx <> C.fptrCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
    (C.TypeName "GLuint", [t|GLuint|]),
    (C.TypeName "GLsizei", [t|GLsizei|]),
    (C.TypeName "GrDirectContext", [t|GrDirectContext|]),
    (C.TypeName "SkSurface", [t|SkSurface|])
  ]
})

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
C.include "include/gpu/ganesh/SkSurfaceGanesh.h"

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

data GlContext = GlContext {
  egl :: Egl
}

instance IsSkiaBackend GL where
  type SkiaBackendContext GL = GlContext
  type SkiaTextureStorage GL = EGLImage
  initializeSkiaBackend = initializeSkiaGles
  newSkiaBackendTexture = newSkiaGLTexture
  destroySkiaTextureStorage = destroySkiaGLTexture
  exportSkiaSurfaceDmabuf = exportDmabuf

exportDmabuf :: SkiaSurfaceState GL -> SkiaIO Dmabuf
exportDmabuf surfaceState = liftIO do
  eglExportDmabuf surfaceState.skia.context.egl surfaceState.storage surfaceState.width surfaceState.height

destroySkiaGLTexture :: SkiaSurfaceState GL -> SkiaIO ()
destroySkiaGLTexture surfaceState = liftIO $ eglDestroyImage surfaceState.skia.context.egl surfaceState.storage

-- | Initialize Skia using the OpenGL ES backend.
initializeSkiaGles :: SkiaIO (ForeignPtr GrDirectContext, GlContext, SkiaDmabufProperties)
initializeSkiaGles = liftIO do
  egl <- initializeGles

  rawSkiaContext <- [CPPU.throwBlock|GrDirectContext* {
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

  when (rawSkiaContext == nullPtr) do
    throwIO $ userError "Failed to initialize skia"

  let finalizerFunPtr = [C.funPtr|void deleteGrDirectContext(GrDirectContext* ctx) {
    ctx->releaseResourcesAndAbandonContext();
    delete ctx;
  }|]

  grDirectContext <- newForeignPtr finalizerFunPtr rawSkiaContext

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



newSkiaGLTexture :: Skia GL -> Int32 -> Int32 -> SkiaIO (ForeignPtr SkSurface, SkiaTextureStorage GL)
newSkiaGLTexture Skia{grDirectContext, context} width height = liftIO do
  texture <- glGenTexture

  let
    cWidth = fromIntegral width :: CInt
    cHeight = fromIntegral height :: CInt

  [CU.block|void {
    GLuint oldTexture;
    glGetIntegerv(GL_TEXTURE_BINDING_2D, (GLint*)&oldTexture);
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8_OES, (GLsizei)$(int cWidth), (GLsizei)$(int cHeight), 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
    glBindTexture(GL_TEXTURE_2D, oldTexture);
  }|]

  -- Map texture to EGL image
  eglImage <- eglCreateGLImage context.egl texture

  rawSkSurface <- [CPPU.throwBlock|SkSurface* {
    GrDirectContext* grDirectContext = $fptr-ptr:(GrDirectContext* grDirectContext);

    GrGLTextureInfo textureInfo;
    textureInfo.fTarget = GL_TEXTURE_2D;
    textureInfo.fID = $(GLuint texture);
    textureInfo.fFormat = GL_RGB8_OES;

    GrBackendTexture backendTexture = GrBackendTextures::MakeGL($(int cWidth), $(int cHeight), skgpu::Mipmapped::kNo, textureInfo);

    std::clog << "Skia GrBackendTexture created from GL texture\n";

    GrSurfaceOrigin origin = GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin;
    int sampleCnt = 0;
    SkColorType colorType = kRGB_888x_SkColorType;
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

  when (rawSkSurface == nullPtr) do
    throwIO $ userError "Failed to initialize skia"

  -- TODO validate if this also deletes the glTexture
  let finalizerFunPtr = [C.funPtr|void deleteSkSurface(SkSurface* skSurface) {
    delete skSurface;
  }|]

  skSurface <- newForeignPtr finalizerFunPtr rawSkSurface

  pure (skSurface, eglImage)
