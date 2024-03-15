{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia (
  test,
) where

import Data.Map.Strict as Map
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CPP
import Language.C.Inline.Unsafe qualified as CU
import Language.C.Types qualified as C
import Quasar.Prelude
import Quasar.Wayland.Gles (glGenTexture)
import Quasar.Wayland.Gles.Egl
import Quasar.Wayland.Gles.Types
import Text.Interpolation.Nyan (int)


C.context (CPP.cppCtx {
  C.ctxTypesTable = CPP.cppCtx.ctxTypesTable <> Map.fromList [
    (C.TypeName "GLuint", [t|GLuint|]),
    (C.TypeName "GLsizei", [t|GLsizei|])
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

test :: Egl -> IO GLuint
test egl = do
  texture <- glGenTexture

  let
    width = 512 :: CInt
    height = 512 :: CInt

  [CU.block|void {
    glBindTexture(GL_TEXTURE_2D, $(GLuint texture));
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8_OES, (GLsizei)$(int width), (GLsizei)$(int height), 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glBindTexture(GL_TEXTURE_2D, 0);
  }|]

  [C.block|void {
    SkGraphics::Init();

    auto grGLInterface = GrGLMakeAssembledInterface(nullptr, egl_get_gl_proc);

    // GrGLInterfaces::MakeEGL would be preferred, but it isn't exported from
    // skia when building as a shared library:
    //auto skiaGrGLInterface = GrGLInterfaces::MakeEGL();

    if (!grGLInterface) {
      std::clog << "Could not create skia GrGLInterface\n";
      return;
    }

    if (grGLInterface->validate()) {
      std::clog << "Skia GrGLInterface valid\n";
    }
    else {
      std::clog << "Skia GrGLInterface invalid\n";
      return;
    }

    sk_sp<GrDirectContext> grDirectContext = GrDirectContexts::MakeGL(std::move(grGLInterface));
    if (!grDirectContext) {
      std::clog << "GrDirectContexts::MakeGL failed\n";
      // TODO abort
      return;
    }

    std::clog << "Skia GrDirectContext initialized\n";

    GrGLTextureInfo textureInfo;
    textureInfo.fTarget = GL_TEXTURE_2D;
    textureInfo.fID = $(GLuint texture);
    textureInfo.fFormat = GL_RGB8_OES;

    GrBackendTexture backendTexture = GrBackendTextures::MakeGL($(int width), $(int height), skgpu::Mipmapped::kNo, textureInfo);

    std::clog << "Skia GrBackendTexture created from GL texture\n";

    GrSurfaceOrigin origin = GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin;
    int sampleCnt = 0;
    SkColorType colorType = kRGB_888x_SkColorType;
    sk_sp<SkColorSpace> colorSpace = nullptr; //SkColorSpace::MakeSRGB();
    SkSurfaceProps* surfaceProps = nullptr; //new SkSurfaceProps();
    sk_sp<SkSurface> skSurface = SkSurfaces::WrapBackendTexture(grDirectContext.get(), backendTexture, origin, sampleCnt, colorType, colorSpace, surfaceProps);
    if (!skSurface) {
      std::clog << "SkSurfaces::WrapBackendTexture failed\n";
      return;
    }

    std::clog << "Skia SkSurfaces created from backend texture\n";


    //SkImageInfo imageInfo = SkImageInfo::Make(16, 16, kRGBA_8888_SkColorType, kPremul_SkAlphaType);
    //sk_sp<SkSurface> skSurface = SkSurfaces::RenderTarget(grDirectContext.get(), skgpu::Budgeted::kYes, imageInfo);
    //if (!skSurface) {
    //  std::clog << "SkSurfaces::RenderTarget failed\n";
    //  return;
    //}
    //std::clog << "Skia SkSurfaces created\n";

    SkCanvas* skCanvas = skSurface->getCanvas();
    skCanvas->clear(SK_ColorRED);

    std::clog << "Got skia SkCanvas\n";

    grDirectContext->flush(skSurface.get());
    grDirectContext->submit();

    std::clog << "Flushed and submitted\n";

    grDirectContext->releaseResourcesAndAbandonContext();

    std::clog << "Cleaning up...\n";
  }|]

  putStrLn "hi skia"
  pure texture
