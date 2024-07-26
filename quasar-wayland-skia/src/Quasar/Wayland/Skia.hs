{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia (
  Skia(..),
  SkiaDmabufProperties(..),
  initializeSkia,
  SkiaSurface(..),
  SkiaSurfaceState(..),
  readSkiaSurfaceState,
  newSkiaSurface,
  newFrameConsumeSurface,

  clearSkiaSurface,
  flushAndSubmit,

  -- * Wayland server
  skiaGlobals,
  skiaShmGlobal,
  skiaDmabufGlobal,

  -- * Internal
  IsSkiaBackend(..),
  readSkiaSurfaceStateIO,
  SkiaImage(..),
  --ManagedSkiaSurface,
) where

import Data.Map.Strict qualified as Map
import Data.Typeable (Typeable)
import Foreign.Ptr
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CPP
import Language.C.Inline.Cpp.Unsafe qualified as CPPU
import Language.C.Types qualified as C
import Quasar.Disposer
import Quasar.Disposer.DisposableVar
import Quasar.Disposer.Rc
import Quasar.Exceptions (AsyncException(..), mkDisposedException, ExceptionSink, DisposedException)
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future
import Quasar.Observable.Core (readObservable)
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Dmabuf
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry (Global)
import Quasar.Wayland.Server.Shm (shmGlobal)
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shm
import Quasar.Wayland.SinglePixelBuffer
import Quasar.Wayland.Skia.CTypes
import Quasar.Wayland.Skia.Thread
import Quasar.Wayland.Utils.SharedMemory


C.context (CPP.cppCtx <> C.fptrCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
    (C.TypeName "GrDirectContext", [t|GrDirectContext|]),
    (C.TypeName "SkSurface", [t|SkSurface|]),
    (C.TypeName "SkImage", [t|SkImage|]),
    (C.TypeName "SkImages::RasterReleaseProc", [t|FunPtr (Ptr () -> Ptr () -> IO ())|])
  ]
})

C.include "<iostream>"

C.include "include/core/SkGraphics.h"
C.include "include/core/SkCanvas.h"
C.include "include/core/SkColorSpace.h"
C.include "include/core/SkImage.h"
C.include "include/core/SkSurface.h"

C.include "include/gpu/GrDirectContext.h"
C.include "include/gpu/GrBackendSurface.h"
C.include "include/gpu/ganesh/SkSurfaceGanesh.h"


-- | Implementation interface for a native skia backend, e.g. OpenGL or Vulkan.
type IsSkiaBackend :: Type -> Constraint
class Typeable s => IsSkiaBackend s where
  type SkiaBackendContext s
  type SkiaTextureStorage s
  initializeSkiaBackend :: SkiaIO (Ptr GrDirectContext, SkiaBackendContext s, SkiaDmabufProperties)
  destroySkiaBackendContext :: SkiaBackendContext s -> SkiaIO ()
  newSkiaBackendTexture :: Skia s -> Int32 -> Int32 -> SkiaIO (Ptr SkSurface, SkiaTextureStorage s)
  destroySkiaTextureStorage :: Skia s -> SkiaSurfaceState s -> SkiaIO ()
  exportSkiaSurfaceDmabuf :: SkiaSurfaceState s -> SkiaIO Dmabuf
  skiaImportDmabuf :: Skia s -> Dmabuf -> SkiaIO (Ptr SkImage)

data Skia s = Skia {
  disposer :: Disposer,
  exceptionSink :: ExceptionSink,
  thread :: SkiaThread,
  grDirectContext :: Ptr GrDirectContext,
  context :: SkiaBackendContext s,
  dmabuf :: SkiaDmabufProperties
}

initializeSkia :: forall s. IsSkiaBackend s => IO (Skia s)
initializeSkia = do
  let exceptionSink = loggingExceptionSink
  thread <- newSkiaThread
  future <- atomically (queueSkiaIO thread (initializeSkiaBackend @s))
  (grDirectContext, context, dmabuf) <- await future
  disposer <- atomicallyC $ newDisposer (destroySkia thread grDirectContext context) exceptionSink
  pure Skia {
    disposer,
    exceptionSink,
    thread,
    grDirectContext,
    context,
    dmabuf
  }
  where
    destroySkia :: SkiaThread -> Ptr GrDirectContext -> SkiaBackendContext s -> IO ()
    destroySkia thread grDirectContext backendContext = runSkiaIO thread do
      liftIO [C.block|void {
        delete $(GrDirectContext* grDirectContext);
      }|]
      destroySkiaBackendContext @s backendContext

-- | High-level wrapper for a skia surface. This type of surface is backed by
-- native rendering backend storage.
newtype SkiaSurface s = SkiaSurface (DisposableVar (SkiaSurfaceState s))
  deriving (Eq, Hashable)

instance ToFuture '[] () (SkiaSurface s) where
  toFuture (SkiaSurface var) = toFuture var

data SkiaSurfaceState s = SkiaSurfaceState {
  skia :: Skia s,
  skSurface :: Ptr SkSurface,
  storage :: SkiaTextureStorage s,
  width :: Int32,
  height :: Int32
}

newSkiaSurface ::
  forall s.
  IsSkiaBackend s =>
  Skia s ->
  Int32 ->
  Int32 ->
  IO (Owned (SkiaSurface s))
newSkiaSurface skia width height =
  runSkiaIO skia.thread do
    newSkiaSurfaceInternal skia width height

newSkiaSurfaceInternal ::
  forall s.
  IsSkiaBackend s =>
  Skia s ->
  Int32 ->
  Int32 ->
  SkiaIO (Owned (SkiaSurface s))
newSkiaSurfaceInternal skia width height = do
  (skSurface, storage) <- newSkiaBackendTexture skia width height
  var <- newFnDisposableVarIO skia.exceptionSink destroySurface SkiaSurfaceState {
    skia,
    skSurface,
    storage,
    width,
    height
  }
  pure (Owned (getDisposer var) (SkiaSurface var))
  where
    destroySurface :: SkiaSurfaceState s -> IO ()
    destroySurface state = runSkiaIO skia.thread do
      let skSurface = state.skSurface
      liftIO [C.block|void {
        delete $(SkSurface* skSurface);
      }|]
      destroySkiaTextureStorage @s skia state

skiaSurfaceKey :: SkiaSurface s -> Unique
skiaSurfaceKey (SkiaSurface var) = disposerElementKey var

readSkiaSurfaceState :: SkiaSurface s -> STMc NoRetry '[DisposedException] (SkiaSurfaceState s)
readSkiaSurfaceState (SkiaSurface var) = readDisposableVar var

readSkiaSurfaceStateIO :: MonadIO m => SkiaSurface s -> m (SkiaSurfaceState s)
readSkiaSurfaceStateIO (SkiaSurface var) =
  tryReadDisposableVarIO var >>= \case
    Nothing -> undefined
    Just surfaceState -> pure surfaceState

newtype SkiaImage s = SkiaImage (DisposableVar (Ptr SkImage))
  deriving (Eq, Hashable)

--newtype ManagedSkiaSurface = ManagedSkiaSurface {
--  skSurface :: ForeignPtr SkSurface
--}



newtype SkiaFrame s = SkiaFrame (DisposableVar (Owned (SkiaFrameState s)))

newSkiaFrame :: Owned (SkiaFrameState s) -> STMc NoRetry '[] (Owned (SkiaFrame s))
newSkiaFrame state = do
  var <- newDisposableVar state
  pure (Owned (getDisposer var) (SkiaFrame var))

newSkiaFrameIO :: Owned (SkiaFrameState s) -> IO (Owned (SkiaFrame s))
newSkiaFrameIO state = do
  var <- newDisposableVarIO state
  pure (Owned (getDisposer var) (SkiaFrame var))


data SkiaFrameState s
  = SkiaFrameLazy (SkiaFrameOp s)
  | SkiaFrameSparked (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))

data SkiaFrameOp s
  = SkiaFrameExternalDmabuf (ExternalFrame Dmabuf (Skia s))
  | SkiaFrameExternalShm (ExternalFrame ShmBuffer (Skia s))
  --- | SkiaFrameShaderOp
  --- | SkiaFrameExported (Rc (SkiaExportBuffer s))
  | SkiaFrameSinglePixel SinglePixelBuffer


data SkiaRenderedFrame s
  = SkiaRenderedFrameSurface (SkiaSurface s)
  | SkiaRenderedFrameImportedDmabuf (SkiaImage s) (ExternalDmabuf s)
  | SkiaRenderedFrameSinglePixel SinglePixelBuffer

newtype SkiaClientBufferManager s = SkiaClientBufferManager {
  dmabufSingleton :: ClientDmabufSingleton
}


data SkiaExportBuffer s
  = SkiaExportBufferSurface (SkiaSurface s)
  | SkiaExportBufferImportedDmabuf (ExternalDmabuf s)
  | SkiaExportBufferImportedShmBuffer (ExternalShmBuffer s)
  | SkiaExportBufferSinglePixel SinglePixelBuffer

data SkiaExportBufferId
  = SkiaExportBufferIdUnique Unique
  | SkiaExportBufferIdSinglePixel SinglePixelBuffer
  deriving (Eq, Generic)

instance Hashable SkiaExportBufferId

instance IsSkiaBackend s => RenderBackend (Skia s) where
  type Frame (Skia s) = SkiaFrame s

instance IsSkiaBackend s => ClientBufferBackend (Skia s) where
  type ClientBufferManager (Skia s) = SkiaClientBufferManager s
  type RenderedFrame (Skia s) = SkiaRenderedFrame s
  type ExportBufferId (Skia s) = SkiaExportBufferId

  newClientBufferManager :: WaylandClient -> STMc NoRetry '[SomeException] (SkiaClientBufferManager s)
  newClientBufferManager client = do
    dmabufSingleton <- getClientDmabufSingleton client
    pure SkiaClientBufferManager {
      dmabufSingleton
    }

  renderFrame :: Rc (SkiaFrame s) -> IO (Rc (SkiaRenderedFrame s))
  renderFrame frame = renderFrameInternal frame

  getExportBufferId ::
    SkiaRenderedFrame s -> STMc NoRetry '[DisposedException] SkiaExportBufferId
  getExportBufferId (SkiaRenderedFrameSurface surface) =
    pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameImportedDmabuf _ (ExternalDmabuf _ key _)) =
    pure (SkiaExportBufferIdUnique key)
  getExportBufferId (SkiaRenderedFrameSinglePixel pixel) =
    pure (SkiaExportBufferIdSinglePixel pixel)

  exportWlBuffer :: SkiaClientBufferManager s -> SkiaRenderedFrame s -> IO (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer manager renderedFrame =
    atomicallyC (getExportBuffer renderedFrame) >>= \case
      SkiaExportBufferSurface surface -> exportSkiaSurface manager surface
      SkiaExportBufferImportedShmBuffer (ExternalShmBuffer _ _ _) -> undefined
      SkiaExportBufferImportedDmabuf (ExternalDmabuf _ _ dmabuf) ->
        atomicallyC $ sharedDmabufExportWlBuffer manager.dmabufSingleton dmabuf
      SkiaExportBufferSinglePixel pixel -> undefined

  syncExportBuffer :: SkiaRenderedFrame s -> IO ()
  syncExportBuffer renderedFrame =
    atomicallyC (getExportBuffer renderedFrame) >>= \case
      SkiaExportBufferSurface surface ->
        -- TODO use fence instead of full cpu sync if available
        flushAndSync surface
      SkiaExportBufferImportedShmBuffer _ ->
        pure () -- upstream is responsible for sync
      SkiaExportBufferImportedDmabuf _ ->
        pure () -- upstream is responsible for sync
      SkiaExportBufferSinglePixel _ ->
        pure () -- trivial buffer, no sync required

  getExportBufferDestroyedFuture :: SkiaRenderedFrame s -> STMc NoRetry '[] (Future '[] ())
  getExportBufferDestroyedFuture renderedFrame = do
    getExportBuffer renderedFrame >>= \case
      SkiaExportBufferSurface surface -> pure (toFuture surface)
      SkiaExportBufferImportedShmBuffer importedShmBuffer -> pure (toFuture importedShmBuffer)
      SkiaExportBufferImportedDmabuf importedDmabuf -> pure (toFuture importedDmabuf)
      SkiaExportBufferSinglePixel pixel -> undefined

getExportBuffer :: SkiaRenderedFrame s -> STMc NoRetry '[] (SkiaExportBuffer s)
getExportBuffer (SkiaRenderedFrameSurface surface) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameImportedDmabuf _ externalDmabuf) = do
  pure (SkiaExportBufferImportedDmabuf externalDmabuf)
getExportBuffer (SkiaRenderedFrameSinglePixel pixel) =
  pure (SkiaExportBufferSinglePixel pixel)

exportSkiaSurface :: forall s. IsSkiaBackend s => SkiaClientBufferManager s -> SkiaSurface s -> IO (NewObject 'Client Interface_wl_buffer)
exportSkiaSurface manager surface = do
  surfaceState <- readSkiaSurfaceStateIO surface
  dmabuf <- runSkiaIO surfaceState.skia.thread $ exportSkiaSurfaceDmabuf surfaceState
  atomicallyC $ consumeDmabufExportWlBuffer manager.dmabufSingleton dmabuf

renderFrameInternal :: IsSkiaBackend s => Rc (SkiaFrame s) -> IO (Rc (SkiaRenderedFrame s))
renderFrameInternal frameRc = do
  -- `consumeRc` holds ownership of the `SkiaFrame` until `cloneRc` of the
  -- `RenderedFrame` is cloned.
  consumeRc frameRc \(SkiaFrame var) -> do
    frc <- atomically do
      tryReadDisposableVar var >>= \case
        Nothing -> throwC mkDisposedException
        Just (Owned _ (SkiaFrameSparked frc)) -> pure frc
        Just (Owned operationDisposer (SkiaFrameLazy op)) -> do
          frc <- cacheFuture =<< sparkFrameOp (Owned operationDisposer op)
          disposer <- futureDisposerGeneric frc
          -- Prevents the SkiaFrameOp from being disposed with the frame, which
          -- is correct since ownership of the SkiaFrameOp has been passed to
          -- `sparkFrameOp`.
          tryWriteDisposableVar var (Owned disposer (SkiaFrameSparked frc))
          pure frc
    renderedFrameRc <- await frc
    atomically do
      -- We are holding an Rc (via `consumeRc`), which holds the SkiaFrame,
      -- which holds the Rc for the SkiaRenderedFrame. This path therefore
      -- _should_ never throw an DisposedException. If this exception is
      -- encountered, someone somewhere disposed the SkiaFrame directly
      -- (ignoring the Rc-based shared ownership), which would be a bug.
      cloneRc renderedFrameRc

-- Takes ownership of the SkiaFrameOp.
sparkFrameOp :: IsSkiaBackend s => Owned (SkiaFrameOp s) -> STM (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))
sparkFrameOp (Owned disposer (SkiaFrameExternalShm externalFrame)) = do
  (ExternalShmBuffer skia _ shmBuffer) <- readExternalFrame externalFrame
  queueSkiaIO skia.thread do
    skiaSurface <- copyShmBuffer skia (Owned disposer shmBuffer)
    newRcIO (SkiaRenderedFrameSurface <$> skiaSurface)
sparkFrameOp (Owned disposer (SkiaFrameExternalDmabuf externalFrame)) = do
  (ExternalDmabuf skia _ _) <- readExternalFrame externalFrame
  queueSkiaIO skia.thread (importDmabuf (Owned disposer externalFrame))
sparkFrameOp (Owned disposer (SkiaFrameSinglePixel singlePixelBuffer)) = do
  disposeEventually_ disposer -- Should be a no-op
  pure <$> newRc (Owned mempty (SkiaRenderedFrameSinglePixel singlePixelBuffer))

-- | Takes ownership of a skia surface to create a frame. The surface will
-- be destroyed later.
newFrameConsumeSurface :: Owned (SkiaSurface s) -> IO (Owned (SkiaFrame s))
newFrameConsumeSurface surface = do
  renderedFrameRc <- newRcIO (SkiaRenderedFrameSurface <$> surface)
  newFrameFromRenderedFrameIO renderedFrameRc

newFrameFromRenderedFrame :: Rc (SkiaRenderedFrame s) -> STMc NoRetry '[] (Owned (SkiaFrame s))
newFrameFromRenderedFrame renderedFrameRc =
  newSkiaFrame (Owned (getDisposer renderedFrameRc) (SkiaFrameSparked (pure renderedFrameRc)))

newFrameFromRenderedFrameIO :: Rc (SkiaRenderedFrame s) -> IO (Owned (SkiaFrame s))
newFrameFromRenderedFrameIO renderedFrameRc =
  newSkiaFrameIO (Owned (getDisposer renderedFrameRc) (SkiaFrameSparked (pure renderedFrameRc)))

makeExportable :: Rc (SkiaRenderedFrame x) -> IO (SkiaFrame s)
makeExportable = undefined


instance IsSkiaBackend s => IsSinglePixelBufferBackend (Skia s) where
  createSinglePixelBufferFrame ::
    Skia s -> SinglePixelBuffer -> STMc NoRetry '[] (Owned (SkiaFrame s))
  createSinglePixelBufferFrame _skia pixel =
    newSkiaFrame (pure (SkiaFrameLazy (SkiaFrameSinglePixel pixel)))



data ExternalShmBuffer s = ExternalShmBuffer (Skia s) Unique ShmBuffer

instance ToFuture '[] () (ExternalShmBuffer s) where
  toFuture (ExternalShmBuffer _ _ borrowedBuffer) = toFuture borrowedBuffer

instance IsSkiaBackend s => IsBufferBackend ShmBuffer (Skia s) where
  type ExternalBuffer ShmBuffer (Skia s) = (ExternalShmBuffer s)

  newExternalBuffer :: Skia s -> Owned ShmBuffer -> STMc NoRetry '[] (Owned (ExternalShmBuffer s))
  newExternalBuffer skia (Owned disposer shmBuffer) = do
    key <- newUniqueSTM
    pure (Owned disposer (ExternalShmBuffer skia key shmBuffer))

  wrapExternalFrame :: Owned (ExternalFrame ShmBuffer (Skia s)) -> STMc NoRetry '[DisposedException] (Owned (SkiaFrame s))
  wrapExternalFrame externalFrame = liftSTMc do
    newSkiaFrame (SkiaFrameLazy . SkiaFrameExternalShm <$> externalFrame)

skiaShmGlobal :: IsSkiaBackend s => Skia s -> Global
skiaShmGlobal = shmGlobal

importShmBuffer :: Skia s -> Owned ShmBuffer -> SkiaIO (Owned (SkiaImage s))
importShmBuffer skia (Owned frameDisposer shmBuffer) = liftIO do
  let offset = fromIntegral shmBuffer.offset
  let rowBytes = fromIntegral shmBuffer.stride
  let height = fromIntegral shmBuffer.height
  let width = fromIntegral shmBuffer.width
  let format = shmBuffer.format
  let grDirectContext = skia.grDirectContext
  let pool = shmBuffer.pool

  size <- atomicallyC $ readObservable pool.size
  (ptrDisposer, ptr) <- mmapDisposer MmapReadOnly pool.fd (fromIntegral size)

  rasterReleaseProc <- $(C.mkFunPtr [t|Ptr () -> Ptr () -> IO ()|]) \_ _ -> do
    traceIO $ "Callback from C: releasing shm memory"
    dispose (ptrDisposer <> frameDisposer)

  skImage <- [CPPU.throwBlock|SkImage* {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);

    auto colorType = $(uint32_t format) == 0 ? kRGBA_8888_SkColorType : kRGB_888x_SkColorType;
    SkImageInfo info = SkImageInfo::Make($(int width), $(int height), colorType, kPremul_SkAlphaType);
    SkPixmap pixmap(info, (void*)($(uint8_t* ptr) + $(size_t offset)), $(size_t rowBytes));

    auto skImage = SkImages::RasterFromPixmap(pixmap, $(SkImages::RasterReleaseProc rasterReleaseProc), nullptr);
    return skImage.release();
  }|]
  liftIO $ newSkiaImage skia skImage

copyShmBuffer :: IsSkiaBackend s => Skia s -> Owned ShmBuffer -> SkiaIO (Owned (SkiaSurface s))
copyShmBuffer skia (Owned bufferDisposer shmBuffer) = do
  let offset = fromIntegral shmBuffer.offset
  let rowBytes = fromIntegral shmBuffer.stride
  let height = fromIntegral shmBuffer.height
  let width = fromIntegral shmBuffer.width
  let format = shmBuffer.format
  let grDirectContext = skia.grDirectContext
  let pool = shmBuffer.pool

  size <- atomicallyC $ readObservable pool.size
  (ptrDisposer, ptr) <- liftIO $ mmapDisposer MmapReadOnly pool.fd (fromIntegral size)

  skiaSurface <- newSkiaSurfaceInternal skia shmBuffer.height shmBuffer.width
  state <- readSkiaSurfaceStateIO (fromOwned skiaSurface)
  let skSurface = state.skSurface

  liftIO [CPPU.throwBlock|void {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);

    auto colorType = $(uint32_t format) == 0 ? kRGBA_8888_SkColorType : kRGB_888x_SkColorType;
    SkImageInfo info = SkImageInfo::Make($(int width), $(int height), colorType, kPremul_SkAlphaType);

    SkPixmap pixmap(info, (void*)($(uint8_t* ptr) + $(size_t offset)), $(size_t rowBytes));

    SkSurface* skSurface = $(SkSurface* skSurface);
    skSurface->writePixels(pixmap, 0, 0);
  }|]

  dispose (ptrDisposer <> bufferDisposer)

  pure skiaSurface



data SkiaDmabufProperties = SkiaDmabufProperties {
  version1Formats :: [DrmFormat],
  version3FormatTable :: DmabufFormatTable,
  feedback :: CompiledDmabufFeedback
}

data ExternalDmabuf s = ExternalDmabuf (Skia s) Unique Dmabuf

instance ToFuture '[] () (ExternalDmabuf s) where
  toFuture (ExternalDmabuf _ _ dmabuf) = toFuture dmabuf

instance IsSkiaBackend s => IsBufferBackend Dmabuf (Skia s) where
  type ExternalBuffer Dmabuf (Skia s) = (ExternalDmabuf s)

  newExternalBuffer :: Skia s -> Owned Dmabuf -> STMc NoRetry '[] (Owned (ExternalDmabuf s))
  newExternalBuffer skia ownedDmabuf = do
    key <- newUniqueSTM
    pure $ (\dmabuf -> ExternalDmabuf skia key dmabuf) <$> ownedDmabuf

  wrapExternalFrame :: Owned (ExternalFrame Dmabuf (Skia s)) -> STMc NoRetry '[DisposedException] (Owned (SkiaFrame s))
  wrapExternalFrame externalFrame = liftSTMc do
    newSkiaFrame (SkiaFrameLazy . SkiaFrameExternalDmabuf <$> externalFrame)

importDmabuf :: IsSkiaBackend s => Owned (ExternalFrame Dmabuf (Skia s)) -> SkiaIO (Rc (SkiaRenderedFrame s))
importDmabuf (Owned extDisposer externalFrame) = do
  externalDmabuf@(ExternalDmabuf skia _ dmabuf) <- readExternalFrameIO externalFrame

  skImage <- skiaImportDmabuf skia dmabuf
  Owned imageDisposer skiaImage <- liftIO $ newSkiaImage skia skImage

  let disposer = extDisposer <> imageDisposer

  liftIO $ newRcIO (Owned disposer (SkiaRenderedFrameImportedDmabuf skiaImage externalDmabuf))


newSkiaImage :: Skia s -> Ptr SkImage -> IO (Owned (SkiaImage s))
newSkiaImage skia skImage = do
  var <- newFnDisposableVarIO skia.exceptionSink destroyImage skImage
  pure (Owned (getDisposer var) (SkiaImage var))
  where
    destroyImage :: Ptr SkImage -> IO ()
    destroyImage img = runSkiaIO skia.thread $ liftIO do
      [CPPU.throwBlock|void {
        delete $(SkImage* img);
      }|]

skiaDmabufGlobal :: IsSkiaBackend s => Skia s -> Global
skiaDmabufGlobal skia =
  dmabufGlobal
    skia
    skia.dmabuf.version1Formats
    skia.dmabuf.version3FormatTable
    skia.dmabuf.feedback


-- | All @wl_buffer@ globals supported by the skia renderer backend.
skiaGlobals :: IsSkiaBackend s => Skia s -> [Global]
skiaGlobals skia = [skiaShmGlobal skia, skiaDmabufGlobal skia]


--newManagedSkiaSurface :: Skia s -> Int -> Int -> IO ManagedSkiaSurface
--newManagedSkiaSurface Skia{grDirectContext} width height = do
--  let
--    cWidth = fromIntegral width
--    cHeight = fromIntegral height
--
--  rawSkSurface <- [CPPU.throwBlock|SkSurface* {
--    GrDirectContext* grDirectContext = $fptr-ptr:(GrDirectContext* grDirectContext);
--
--    SkImageInfo imageInfo = SkImageInfo::Make($(int cWidth), $(int cHeight), kRGBA_8888_SkColorType, kPremul_SkAlphaType);
--    sk_sp<SkSurface> skSurface = SkSurfaces::RenderTarget(grDirectContext, skgpu::Budgeted::kYes, imageInfo);
--    if (!skSurface) {
--      std::clog << "SkSurfaces::RenderTarget failed\n";
--      return nullptr;
--    }
--
--    // Release the base pointer from the shared pointer, since we will track the
--    // lifetime by using a Haskell ForeignPtr.
--    return skSurface.release();
--  }|]
--
--  when (rawSkSurface == nullPtr) do
--    throwIO $ userError "Failed to initialize skia"
--
--  let finalizerFunPtr = [C.funPtr|void deleteSkSurface(SkSurface* skSurface) {
--    delete skSurface;
--  }|]
--
--  skSurface <- newForeignPtr finalizerFunPtr rawSkSurface
--
--  pure ManagedSkiaSurface { skSurface }


clearSkiaSurface :: SkiaSurface s -> IO ()
clearSkiaSurface surface = do
  surfaceState <- readSkiaSurfaceStateIO surface
  runSkiaIO surfaceState.skia.thread (clearSkiaSurfaceInternal surfaceState.skSurface)


clearSkiaSurfaceInternal :: Ptr SkSurface -> SkiaIO ()
clearSkiaSurfaceInternal skSurface = liftIO do
  [CPPU.throwBlock|void {
    SkCanvas* skCanvas = ($(SkSurface* skSurface))->getCanvas();
    skCanvas->clear(SK_ColorRED);
  }|]

flushAndSubmit :: SkiaSurface s -> IO ()
flushAndSubmit surface = do
  surfaceState <- readSkiaSurfaceStateIO surface
  runSkiaIO surfaceState.skia.thread (flushAndSubmitInternal surfaceState)

flushAndSubmitInternal :: SkiaSurfaceState s -> SkiaIO ()
flushAndSubmitInternal surfaceState = liftIO do
  let grDirectContext = surfaceState.skia.grDirectContext
  let skSurface = surfaceState.skSurface
  [CPPU.throwBlock|void {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);
    grDirectContext->flushAndSubmit($(SkSurface* skSurface));
  }|]

flushAndSync :: SkiaSurface s -> IO ()
flushAndSync surface = do
  surfaceState <- readSkiaSurfaceStateIO surface
  runSkiaIO surfaceState.skia.thread (flushAndSyncInternal surfaceState)

flushAndSyncInternal :: SkiaSurfaceState s -> SkiaIO ()
flushAndSyncInternal surfaceState = liftIO do
  let grDirectContext = surfaceState.skia.grDirectContext
  let skSurface = surfaceState.skSurface
  [CPPU.throwBlock|void {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);
    grDirectContext->flushAndSubmit($(SkSurface* skSurface), GrSyncCpu::kYes);
  }|]
