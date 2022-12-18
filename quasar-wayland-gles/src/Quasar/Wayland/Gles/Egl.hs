{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Gles.Egl (
  Egl,
  Dmabuf(..),
  DmabufPlane(..),
  initializeEgl,
  eglCreateGLImage,
  exportDmabuf,
  queryDmabufFormats,
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
    requiredEglExtensions :: Set String = Set.fromList $ [
      "EGL_KHR_no_config_context",
      "EGL_MESA_image_dma_buf_export",
      "EGL_EXT_image_dma_buf_import",
      "EGL_EXT_image_dma_buf_import_modifiers"
      ] <> egl14RequiredExtensions
    egl14RequiredExtensions
      | major == 1 && minor == 4 = [
        "EGL_KHR_image_base",
        "EGL_KHR_gl_image"
        ]
      | otherwise = []
    missingEglExtensions = Set.difference requiredEglExtensions eglExtensions

  unless (Set.null missingEglExtensions) $
    fail $ "Missing EGL extensions: " <> intercalate " " missingEglExtensions

  throwErrnoIf_ (== 0) "eglBindAPI"
    [CU.exp| EGLBoolean { eglBindAPI(EGL_OPENGL_ES_API) } |]

  -- Requires EGL_KHR_no_config_context
  context <- throwErrnoIfNull "eglCreateContext"
    [CU.block|
      EGLContext {
        static const EGLint attributes[] = {
          EGL_CONTEXT_MAJOR_VERSION, 2,
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
    }
  |]

  let egl = Egl { display, context }

  traceShowIO =<< queryDmabufFormats egl

  pure egl


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
    traceIO $ mconcat ["Device ", show i, ": ", deviceExtensionString]
    let deviceExtensions = Set.fromList (words deviceExtensionString)

    when (Set.member "EGL_EXT_device_drm" deviceExtensions) do
      deviceNode <- eglTryQueryDeviceString device [CU.pure|EGLint { EGL_DRM_DEVICE_FILE_EXT }|]
      traceIO $ mconcat ["Device ", show i, ": ", show deviceNode]

    when (Set.member "EGL_EXT_device_drm_render_node" deviceExtensions) do
      renderNode <- eglTryQueryDeviceString device [CU.pure|EGLint { EGL_DRM_RENDER_NODE_FILE_EXT }|]
      traceIO $ mconcat ["Device ", show i, ": ", show renderNode]

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
  -- EGLImage requires EGL 1.5
  -- EGLImage is available in EGL 1.4 with EGL_KHR_image_base and EGL_KHR_gl_image
  throwErrnoIfNull "eglCreateImage"
    [CU.exp| EGLImage { eglCreateImage($(EGLDisplay display), $(EGLContext context), EGL_GL_TEXTURE_2D, (EGLClientBuffer)(intptr_t) $(GLuint glTexture), NULL) } |]

exportDmabuf :: Egl -> EGLImage -> IO Dmabuf
exportDmabuf Egl{display} image = do
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
      fds <- Fd <<$>> peekArray numPlanes fdsPtr
      strides <- peekArray numPlanes stridesPtr
      offsets <- peekArray numPlanes offsetsPtr
      pure (zipPlanes modifier fds strides offsets)

  pure Dmabuf { format, planes }

zipPlanes :: DrmModifier -> [Fd] -> [Word32] -> [Word32] -> [DmabufPlane]
zipPlanes modifier fds strides offsets = packPlane <$> zipped
  where
    zipped = zip3 fds strides offsets
    packPlane (fd, stride, offset) = DmabufPlane {fd, stride, offset, modifier}


queryDmabufFormats :: Egl -> IO [(DrmFormat, [DrmModifier])]
queryDmabufFormats egl@Egl{display} = do
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

  forM formats \format -> do
    modifiers <- queryDmabufModifiers egl format
    pure (format, modifiers)

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
            // The external_only modifier can be ignored, since all textures
            // are imported by using GL_OES_EGL_image_external
            NULL,
            $(EGLint* countPtr))
        }|]
      count <- peek countPtr
      DrmModifier <<$>> peekArray (fromIntegral count) modifiersPtr
