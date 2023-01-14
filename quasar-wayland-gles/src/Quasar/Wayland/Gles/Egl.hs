{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles.Egl (
  Egl,
  Dmabuf(..),
  DmabufPlane(..),
  initializeEgl,
  eglCreateGLImage,
  eglDestroyImage,
  eglExportDmabuf,
  eglQueryDmabufFormats,
  eglImportDmabuf,
) where

import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Foreign.C
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Gles.Egl.Debug
import Quasar.Wayland.Gles.Egl.Types
import Quasar.Wayland.Gles.Types
import Quasar.Wayland.Gles.Utils.InlineC
import Quasar.Wayland.Gles.Utils.Stat
import Quasar.Wayland.Utils.SharedFd
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import System.Posix.Types (Fd(Fd))


C.context ctx

C.include "<stdint.h>"
C.include "<unistd.h>"
C.include "<EGL/egl.h>"
C.include "<EGL/eglext.h>"

C.include "<GLES2/gl2.h>"

C.verbatim "PFNEGLQUERYDEVICESEXTPROC eglQueryDevicesEXT;"
C.verbatim "PFNEGLQUERYDEVICESTRINGEXTPROC eglQueryDeviceStringEXT;"

C.verbatim "PFNEGLEXPORTDMABUFIMAGEQUERYMESAPROC eglExportDMABUFImageQueryMESA;"
C.verbatim "PFNEGLEXPORTDMABUFIMAGEMESAPROC eglExportDMABUFImageMESA;"

C.verbatim "PFNEGLQUERYDMABUFFORMATSEXTPROC eglQueryDmaBufFormatsEXT;"
C.verbatim "PFNEGLQUERYDMABUFMODIFIERSEXTPROC eglQueryDmaBufModifiersEXT;"

C.verbatim "PFNEGLCREATEIMAGEKHRPROC eglCreateImageKHR;"
C.verbatim "PFNEGLDESTROYIMAGEKHRPROC eglDestroyImageKHR;"

data Egl = Egl {
  display :: EGLDisplay,
  context :: EGLContext
}

initializeEgl :: IO Egl
initializeEgl = do
  clientExtensionsString <-
    eglQueryString
      [CU.pure|EGLDisplay { EGL_NO_DISPLAY }|]
      [CU.pure|EGLint { EGL_EXTENSIONS }|]

  traceIO $ mconcat ["EGL client extensions: ", clientExtensionsString]

  let
    clientExtensions = Set.fromList (words clientExtensionsString)
    requiredClientExtensions :: Set String = Set.fromList [
      "EGL_KHR_debug"
      ]
    missingClientExtensions = Set.difference requiredClientExtensions clientExtensions

  unless (Set.null missingClientExtensions) $
    fail $ "Missing EGL client extensions: " <> intercalate " " missingClientExtensions

  -- EGL_KHR_debug is available, so the debug callback should be attached immediately
  initializeEglDebugHandler

  unless ("EGL_EXT_device_enumeration" `elem` clientExtensions && "EGL_EXT_device_query" `elem` clientExtensions) do
    fail "Missing extensions for device enumeration"
  display <- getEglDisplayDevice

  --when ("EGL_MESA_platform_surfaceless" `elem` clientExtensions) do
  --  traceIO "Surfaceless platform available"
  --display <- getEglDisplaySurfaceless

  (major :: EGLint, minor :: EGLint) <- C.withPtrs_ \(majorPtr, minorPtr) ->
    throwErrnoIf_ (== 0) "eglInitialize"
      [CU.exp|EGLBoolean { eglInitialize($(EGLDisplay display), $(EGLint* majorPtr), $(EGLint* minorPtr)) }|]

  traceIO $ mconcat ["EGL ", show major, ".", show minor, " initialized"]

  when (major == 1 && minor < 4) $ fail "Insufficient EGL version: EGL 1.4 is required"

  traceIO . ("EGL version: " <>) =<< eglQueryString display [CU.pure|EGLint { EGL_VERSION }|]
  traceIO . ("EGL vendor: " <>) =<< eglQueryString display [CU.pure|EGLint { EGL_VENDOR }|]
  traceIO . ("EGL client apis: " <>) =<< eglQueryString display [CU.pure|EGLint { EGL_CLIENT_APIS }|]

  eglExtensionString <- eglQueryString display [CU.pure|EGLint { EGL_EXTENSIONS }|]
  traceIO $ mconcat ["EGL extensions: ", eglExtensionString]

  let
    eglExtensions = Set.fromList (words eglExtensionString)
    requiredEglExtensions :: Set String = Set.fromList [
      "EGL_KHR_no_config_context",
      "EGL_MESA_image_dma_buf_export",
      -- From 1.5 the non-KHR versions of the functions _could_ be used instead
      "EGL_KHR_image_base",
      "EGL_EXT_image_dma_buf_import",
      "EGL_EXT_image_dma_buf_import_modifiers"
      ]
    missingEglExtensions = Set.difference requiredEglExtensions eglExtensions

  unless (Set.null missingEglExtensions) $
    fail $ "Missing EGL extensions: " <> intercalate " " missingEglExtensions

  throwErrnoIf_ (== 0) "eglBindAPI"
    [CU.exp| EGLBoolean { eglBindAPI(EGL_OPENGL_ES_API) } |]

  -- Requires EGL_KHR_no_config_context
  context <- throwErrnoIfNull "eglCreateContext"
    [CU.block|
      EGLContext {
        const EGLint attributes[] = {
          EGL_CONTEXT_MAJOR_VERSION, 2,
          EGL_CONTEXT_OPENGL_DEBUG, EGL_TRUE,
          // terminate list
          EGL_NONE
        };
        return eglCreateContext($(EGLDisplay display), NULL, EGL_NO_CONTEXT, attributes);
      }
    |]

  throwErrnoIf_ (== 0) "eglMakeCurrent"
    [CU.exp|EGLBoolean { eglMakeCurrent($(EGLDisplay display), EGL_NO_SURFACE, EGL_NO_SURFACE, $(EGLContext context)) }|]

  [CU.block|
    void {
      // Requires EGL_MESA_image_dma_buf_export
      eglExportDMABUFImageQueryMESA = (PFNEGLEXPORTDMABUFIMAGEQUERYMESAPROC)eglGetProcAddress("eglExportDMABUFImageQueryMESA");
      eglExportDMABUFImageMESA = (PFNEGLEXPORTDMABUFIMAGEMESAPROC)eglGetProcAddress("eglExportDMABUFImageMESA");

      // Requires EGL_EXT_image_dma_buf_import_modifiers
      eglQueryDmaBufFormatsEXT = (PFNEGLQUERYDMABUFFORMATSEXTPROC)eglGetProcAddress("eglQueryDmaBufFormatsEXT");
      eglQueryDmaBufModifiersEXT = (PFNEGLQUERYDMABUFMODIFIERSEXTPROC)eglGetProcAddress("eglQueryDmaBufModifiersEXT");

      // Requires EGL_KHR_image_base
      eglCreateImageKHR = (PFNEGLCREATEIMAGEKHRPROC)eglGetProcAddress("eglCreateImageKHR");
      eglDestroyImageKHR = (PFNEGLDESTROYIMAGEKHRPROC)eglGetProcAddress("eglDestroyImageKHR");
    }
  |]

  pure Egl { display, context }


getEglDisplayDevice :: IO EGLDisplay
getEglDisplayDevice = do
  -- Requires EGL_EXT_device_enumeration and EGL_EXT_device_query
  [CU.block|
    void {
      eglQueryDevicesEXT = (PFNEGLQUERYDEVICESEXTPROC)eglGetProcAddress("eglQueryDevicesEXT");
      eglQueryDeviceStringEXT = (PFNEGLQUERYDEVICESTRINGEXTPROC)eglGetProcAddress("eglQueryDeviceStringEXT");
    }
  |]

  deviceCount <- throwErrnoIfMinus1 "eglQueryDevicesEXT"
    [CU.block|
      EGLint {
        EGLint count;
        EGLBoolean result = eglQueryDevicesEXT(0, NULL, &count);
        if (!result) {
          return -1;
        }
        return count;
      }
    |]
  traceIO $ "Available EGL devices: " <> show deviceCount

  (devices :: [EGLDeviceEXT]) <- allocaArray (fromIntegral deviceCount) \ptr -> do
    throwErrnoIf_ (== 0) "eglQueryDevicesEXT"
      [CU.block|
        EGLBoolean {
          EGLint count;
          EGLBoolean result = eglQueryDevicesEXT($(EGLint deviceCount), $(EGLDeviceEXT* ptr), &count);
          if (count < $(EGLint deviceCount)) {
            return EGL_FALSE;
          }
          return result;
        }
      |]
    peekArray (fromIntegral deviceCount) ptr

  forM_ (zip [0,1..] devices) \(i :: Int, device) -> do
    deviceExtensionString <- eglQueryDeviceString device [CU.pure|EGLint { EGL_EXTENSIONS }|]
    traceIO $ mconcat ["Device ", show i, ": device extensions: ", deviceExtensionString]
    let deviceExtensions = Set.fromList (words deviceExtensionString)

    when (Set.member "EGL_EXT_device_drm" deviceExtensions) do
      deviceNode <- eglTryQueryDeviceString device [CU.pure|EGLint { EGL_DRM_DEVICE_FILE_EXT }|]
      forM_ deviceNode \path -> do
        traceIO $ mconcat ["Device ", show i, ": drm device: ", path]
        devT <- statDevT path
        traceIO $ mconcat ["Device ", show i, ": drm device dev_t: ", show devT]

    when (Set.member "EGL_EXT_device_drm_render_node" deviceExtensions) do
      renderNode <- eglTryQueryDeviceString device [CU.pure|EGLint { EGL_DRM_RENDER_NODE_FILE_EXT }|]
      forM_ renderNode \path -> do
        traceIO $ mconcat ["Device ", show i, ": render node: ", path]
        devT <- statDevT path
        traceIO $ mconcat ["Device ", show i, ": render node dev_t: ", show devT]

  -- NOTE If required the drm master fd can be accessed by querying EGL_DRM_MASTER_FD_EXT (requires EGL_EXT_device_drm)

  traceIO "Using device 0"
  (device:_) <- pure devices

  throwErrnoIfNull "eglGetPlatformDisplay"
    [CU.exp|EGLDisplay { eglGetPlatformDisplay(EGL_PLATFORM_DEVICE_EXT, $(EGLDeviceEXT device), NULL) }|]

--getEglDisplaySurfaceless :: IO EGLDisplay
--getEglDisplaySurfaceless = do
--  -- Requires EGL_MESA_platform_surfaceless
--  throwErrnoIfNull "eglGetPlatformDisplay"
--    [CU.exp|EGLDisplay { eglGetPlatformDisplay(EGL_PLATFORM_SURFACELESS_MESA, EGL_DEFAULT_DISPLAY, NULL) }|]


eglQueryString :: EGLDisplay -> EGLint -> IO String
eglQueryString display name =
  peekCString =<< throwErrnoIfNull "eglQueryString"
    [CU.exp|char const * { eglQueryString($(EGLDisplay display), $(EGLint name)) }|]

eglQueryDeviceString :: EGLDeviceEXT -> EGLint -> IO String
eglQueryDeviceString device name =
  peekCString =<< throwErrnoIfNull "eglQueryDeviceStringEXT"
    [CU.exp|char const * { eglQueryDeviceStringEXT($(EGLDeviceEXT device), $(EGLint name)) }|]

eglTryQueryDeviceString :: EGLDeviceEXT -> EGLint -> IO (Maybe String)
eglTryQueryDeviceString device name = do
  cstr <- [CU.exp|char const * { eglQueryDeviceStringEXT($(EGLDeviceEXT device), $(EGLint name)) }|]
  if cstr /= nullPtr
    then Just <$> peekCString cstr
    else pure Nothing


eglCreateGLImage :: Egl -> GLuint -> IO EGLImage
eglCreateGLImage Egl{display, context} glTexture = do
  -- Requires EGL_MESA_image_dma_buf_export
  -- eglCreateImage and EGLImage require EGL 1.5
  -- eglCreateImageKHR and EGLImage are available in EGL 1.4 with EGL_KHR_image_base
  throwErrnoIfNull "eglCreateImageKHR"
    [CU.exp| EGLImage { eglCreateImageKHR($(EGLDisplay display), $(EGLContext context), EGL_GL_TEXTURE_2D, (EGLClientBuffer)(intptr_t) $(GLuint glTexture), NULL) } |]

eglDestroyImage :: Egl -> EGLImage -> IO ()
eglDestroyImage Egl{display} image = do
  throwErrnoIf_ (== 0) "eglDestroyImage"
    [CU.exp|EGLBoolean { eglDestroyImage($(EGLDisplay display), $(EGLImage image)) }|]

eglExportDmabuf :: Egl -> EGLImage -> Int32 -> Int32 -> IO Dmabuf
eglExportDmabuf Egl{display} image width height = do
  (DrmFormat -> format, DrmModifier -> modifier, fromIntegral -> numPlanes) <-
    C.withPtrs_ \(fourccPtr, modifierPtr, numPlanesPtr) ->
      throwErrnoIf_ (== 0) "eglExportDMABUFImageQueryMESA"
        [CU.block|
          EGLBoolean {
            return eglExportDMABUFImageQueryMESA(
              $(EGLDisplay display),
              $(EGLImage image),
              $(uint32_t* fourccPtr),
              $(int* numPlanesPtr),
              $(uint64_t* modifierPtr));
          }
        |]

  planes <-
    allocaArray numPlanes \fdsPtr ->
    allocaArray numPlanes \stridesPtr ->
    allocaArray numPlanes \offsetsPtr -> do
      throwErrnoIf_ (== 0) "eglExportDMABUFImageMESA"
        [CU.block|
          EGLBoolean {
            return eglExportDMABUFImageMESA(
              $(EGLDisplay display),
              $(EGLImage image),
              $(int* fdsPtr),
              $(uint32_t* stridesPtr),
              $(uint32_t* offsetsPtr));
          }
        |]
      rawFds <- Fd <<$>> peekArray numPlanes fdsPtr
      fds <- mapM newSharedFd rawFds
      strides <- peekArray numPlanes stridesPtr
      offsets <- peekArray numPlanes offsetsPtr
      pure (zipPlanes modifier fds strides offsets)

  pure Dmabuf { width, height, format, planes }

zipPlanes :: DrmModifier -> [SharedFd] -> [Word32] -> [Word32] -> [DmabufPlane]
zipPlanes modifier fds strides offsets = packPlane <$> zipped
  where
    zipped = zip3 fds strides offsets
    packPlane (fd, stride, offset) = DmabufPlane {fd, stride, offset, modifier}


eglQueryDmabufFormats :: Egl -> IO ([DrmFormat], [(DrmFormat, DrmModifier)])
eglQueryDmabufFormats egl@Egl{display} = do
  -- Requires EGL_EXT_image_dma_buf_import_modifiers

  -- Query format count
  maxCount <- alloca \ptr -> do
    throwErrnoIf_ (== 0) "eglQueryDmaBufFormatsEXT"
      [CU.exp|EGLBoolean {
        eglQueryDmaBufFormatsEXT($(EGLDisplay display), 0, NULL, $(EGLint* ptr))
      }|]
    peek ptr

  -- Query formats
  formats <- allocaArray (fromIntegral maxCount) \ptr -> do
    alloca \countPtr -> do
      throwErrnoIf_ (== 0) "eglQueryDmaBufFormatsEXT"
        [CU.exp|EGLBoolean {
          eglQueryDmaBufFormatsEXT($(EGLDisplay display), $(EGLint maxCount), $(uint32_t* ptr), $(EGLint* countPtr))
        }|]
      count <- peek countPtr
      DrmFormat <<$>> peekArray (fromIntegral count) ptr

  modifiers <- forM formats \format -> do
    (format,) <<$>> queryDmabufModifiers egl format

  pure (formats, mconcat modifiers)

queryDmabufModifiers :: Egl -> DrmFormat -> IO [DrmModifier]
queryDmabufModifiers Egl{display} DrmFormat{fourcc} = do
  -- Requires EGL_EXT_image_dma_buf_import_modifiers

  -- Query modifier count
  maxCount <- alloca \countPtr -> do
    throwErrnoIf_ (== 0) "eglQueryDmaBufModifiersEXT"
      [CU.exp|EGLBoolean {
        eglQueryDmaBufModifiersEXT($(EGLDisplay display), $(uint32_t fourcc), 0, NULL, NULL, $(EGLint* countPtr))
      }|]
    peek countPtr

  -- Query modifiers
  allocaArray (fromIntegral maxCount) \modifiersPtr ->
    alloca \countPtr -> do
      throwErrnoIf_ (== 0) "eglQueryDmaBufModifiersEXT"
        [CU.exp|EGLBoolean {
          eglQueryDmaBufModifiersEXT(
            $(EGLDisplay display),
            $(uint32_t fourcc),
            $(EGLint maxCount),
            $(uint64_t* modifiersPtr),
            // The external_only data is not useful, since all textures are
            // imported by using GL_OES_EGL_image_external
            NULL,
            $(EGLint* countPtr))
        }|]
      count <- peek countPtr
      DrmModifier <<$>> peekArray (fromIntegral count) modifiersPtr


eglImportDmabuf :: Egl -> Dmabuf -> IO EGLImage
eglImportDmabuf Egl{display} dmabuf = do
  -- Requires:
  -- - EGL_KHR_image_base
  -- - EGL_EXT_image_dma_buf_import
  -- - EGL_EXT_image_dma_buf_import_modifiers
  importDmabufPlanes dmabuf.planes
  where
    width = dmabuf.width
    height = dmabuf.height
    fourcc = dmabuf.format.fourcc

    importDmabufPlanes :: [DmabufPlane] -> IO EGLImage
    importDmabufPlanes [p0] = do
      (Fd fd0) <- unshareSharedFd p0.fd
      let
        offset0 = p0.offset
        stride0 = p0.stride
        hi0 = p0.modifier.hi
        lo0 = p0.modifier.lo
      traceM $ mconcat ["Importing dmabuf to EGLImage: ", show dmabuf]
      image <- [CU.block|EGLImage {
        const EGLAttrib attributes[] = {
          EGL_WIDTH, $(EGLint width),
          EGL_HEIGHT, $(EGLint height),
          EGL_LINUX_DRM_FOURCC_EXT, $(uint32_t fourcc),
          EGL_DMA_BUF_PLANE0_FD_EXT, $(int fd0),
          EGL_DMA_BUF_PLANE0_OFFSET_EXT, $(uint32_t offset0),
          EGL_DMA_BUF_PLANE0_PITCH_EXT, $(uint32_t stride0),
          EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT, $(uint32_t hi0),
          EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT, $(uint32_t lo0),
          // terminate list
          EGL_NONE
        };
        return eglCreateImage(
          $(EGLDisplay display),
          EGL_NO_CONTEXT,
          EGL_LINUX_DMA_BUF_EXT,
          (EGLClientBuffer)NULL,
          attributes);
      }|]
      result <- eglGetError
      unless (isEglSuccess result) $ throwIO result
      pure image
    importDmabufPlanes [p0, p1] = do
      (Fd fd0) <- unshareSharedFd p0.fd
      (Fd fd1) <- unshareSharedFd p1.fd
      let
        offset0 = p0.offset
        stride0 = p0.stride
        hi0 = p0.modifier.hi
        lo0 = p0.modifier.lo
        offset1 = p1.offset
        stride1 = p1.stride
        hi1 = p1.modifier.hi
        lo1 = p1.modifier.lo
      traceM $ mconcat ["Importing dmabuf to EGLImage: ", show dmabuf]
      image <- [CU.block|EGLImage {
        const EGLint attributes[] = {
          EGL_WIDTH, $(EGLint width),
          EGL_HEIGHT, $(EGLint height),
          EGL_LINUX_DRM_FOURCC_EXT, $(uint32_t fourcc),
          EGL_DMA_BUF_PLANE0_FD_EXT, $(int fd0),
          EGL_DMA_BUF_PLANE0_OFFSET_EXT, $(uint32_t offset0),
          EGL_DMA_BUF_PLANE0_PITCH_EXT, $(uint32_t stride0),
          EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT, $(uint32_t hi0),
          EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT, $(uint32_t lo0),
          EGL_DMA_BUF_PLANE1_FD_EXT, $(int fd1),
          EGL_DMA_BUF_PLANE1_OFFSET_EXT, $(uint32_t offset1),
          EGL_DMA_BUF_PLANE1_PITCH_EXT, $(uint32_t stride1),
          EGL_DMA_BUF_PLANE1_MODIFIER_HI_EXT, $(uint32_t hi1),
          EGL_DMA_BUF_PLANE1_MODIFIER_LO_EXT, $(uint32_t lo1),
          // terminate list
          EGL_NONE
        };
        return eglCreateImageKHR(
          $(EGLDisplay display),
          EGL_NO_CONTEXT,
          EGL_LINUX_DMA_BUF_EXT,
          (EGLClientBuffer)NULL,
          attributes);
      }|]
      result <- eglGetError
      unless (isEglSuccess result) $ throwIO result
      pure image
    importDmabufPlanes [p0, p1, p2] = do
      (Fd fd0) <- unshareSharedFd p0.fd
      (Fd fd1) <- unshareSharedFd p1.fd
      (Fd fd2) <- unshareSharedFd p2.fd
      let
        offset0 = p0.offset
        stride0 = p0.stride
        hi0 = p0.modifier.hi
        lo0 = p0.modifier.lo
        offset1 = p1.offset
        stride1 = p1.stride
        hi1 = p1.modifier.hi
        lo1 = p1.modifier.lo
        offset2 = p2.offset
        stride2 = p2.stride
        hi2 = p2.modifier.hi
        lo2 = p2.modifier.lo
      traceM $ mconcat ["Importing dmabuf to EGLImage: ", show dmabuf]
      image <- [CU.block|EGLImage {
        const EGLint attributes[] = {
          EGL_WIDTH, $(EGLint width),
          EGL_HEIGHT, $(EGLint height),
          EGL_LINUX_DRM_FOURCC_EXT, $(uint32_t fourcc),
          EGL_DMA_BUF_PLANE0_FD_EXT, $(int fd0),
          EGL_DMA_BUF_PLANE0_OFFSET_EXT, $(uint32_t offset0),
          EGL_DMA_BUF_PLANE0_PITCH_EXT, $(uint32_t stride0),
          EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT, $(uint32_t hi0),
          EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT, $(uint32_t lo0),
          EGL_DMA_BUF_PLANE1_FD_EXT, $(int fd1),
          EGL_DMA_BUF_PLANE1_OFFSET_EXT, $(uint32_t offset1),
          EGL_DMA_BUF_PLANE1_PITCH_EXT, $(uint32_t stride1),
          EGL_DMA_BUF_PLANE1_MODIFIER_HI_EXT, $(uint32_t hi1),
          EGL_DMA_BUF_PLANE1_MODIFIER_LO_EXT, $(uint32_t lo1),
          EGL_DMA_BUF_PLANE2_FD_EXT, $(int fd2),
          EGL_DMA_BUF_PLANE2_OFFSET_EXT, $(uint32_t offset2),
          EGL_DMA_BUF_PLANE2_PITCH_EXT, $(uint32_t stride2),
          EGL_DMA_BUF_PLANE2_MODIFIER_HI_EXT, $(uint32_t hi2),
          EGL_DMA_BUF_PLANE2_MODIFIER_LO_EXT, $(uint32_t lo2),
          // terminate list
          EGL_NONE
        };
        return eglCreateImageKHR(
          $(EGLDisplay display),
          EGL_NO_CONTEXT,
          EGL_LINUX_DMA_BUF_EXT,
          (EGLClientBuffer)NULL,
          attributes);
      }|]
      result <- eglGetError
      unless (isEglSuccess result) $ throwIO result
      pure image
    importDmabufPlanes [p0, p1, p2, p3] = do
      (Fd fd0) <- unshareSharedFd p0.fd
      (Fd fd1) <- unshareSharedFd p1.fd
      (Fd fd2) <- unshareSharedFd p2.fd
      (Fd fd3) <- unshareSharedFd p3.fd
      let
        offset0 = p0.offset
        stride0 = p0.stride
        hi0 = p0.modifier.hi
        lo0 = p0.modifier.lo
        offset1 = p1.offset
        stride1 = p1.stride
        hi1 = p1.modifier.hi
        lo1 = p1.modifier.lo
        offset2 = p2.offset
        stride2 = p2.stride
        hi2 = p2.modifier.hi
        lo2 = p2.modifier.lo
        offset3 = p3.offset
        stride3 = p3.stride
        hi3 = p3.modifier.hi
        lo3 = p3.modifier.lo
      traceM $ mconcat ["Importing dmabuf to EGLImage: ", show dmabuf]
      image <- [CU.block|EGLImage {
        const EGLint attributes[] = {
          EGL_WIDTH, $(EGLint width),
          EGL_HEIGHT, $(EGLint height),
          EGL_LINUX_DRM_FOURCC_EXT, $(uint32_t fourcc),
          EGL_DMA_BUF_PLANE0_FD_EXT, $(int fd0),
          EGL_DMA_BUF_PLANE0_OFFSET_EXT, $(uint32_t offset0),
          EGL_DMA_BUF_PLANE0_PITCH_EXT, $(uint32_t stride0),
          EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT, $(uint32_t hi0),
          EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT, $(uint32_t lo0),
          EGL_DMA_BUF_PLANE1_FD_EXT, $(int fd1),
          EGL_DMA_BUF_PLANE1_OFFSET_EXT, $(uint32_t offset1),
          EGL_DMA_BUF_PLANE1_PITCH_EXT, $(uint32_t stride1),
          EGL_DMA_BUF_PLANE1_MODIFIER_HI_EXT, $(uint32_t hi1),
          EGL_DMA_BUF_PLANE1_MODIFIER_LO_EXT, $(uint32_t lo1),
          EGL_DMA_BUF_PLANE2_FD_EXT, $(int fd2),
          EGL_DMA_BUF_PLANE2_OFFSET_EXT, $(uint32_t offset2),
          EGL_DMA_BUF_PLANE2_PITCH_EXT, $(uint32_t stride2),
          EGL_DMA_BUF_PLANE2_MODIFIER_HI_EXT, $(uint32_t hi2),
          EGL_DMA_BUF_PLANE2_MODIFIER_LO_EXT, $(uint32_t lo2),
          EGL_DMA_BUF_PLANE3_FD_EXT, $(int fd3),
          EGL_DMA_BUF_PLANE3_OFFSET_EXT, $(uint32_t offset3),
          EGL_DMA_BUF_PLANE3_PITCH_EXT, $(uint32_t stride3),
          EGL_DMA_BUF_PLANE3_MODIFIER_HI_EXT, $(uint32_t hi3),
          EGL_DMA_BUF_PLANE3_MODIFIER_LO_EXT, $(uint32_t lo3),
          // terminate list
          EGL_NONE
        };
        return eglCreateImageKHR(
          $(EGLDisplay display),
          EGL_NO_CONTEXT,
          EGL_LINUX_DMA_BUF_EXT,
          (EGLClientBuffer)NULL,
          attributes);
      }|]
      result <- eglGetError
      unless (isEglSuccess result) $ throwIO result
      pure image
    importDmabufPlanes planes = throwIO $ userError $ mconcat ["Unexpected number of dmabuf planes (", show (length planes), " planes)"]
