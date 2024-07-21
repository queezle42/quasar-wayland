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
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shm
import Quasar.Wayland.SinglePixelBuffer
import Quasar.Wayland.Skia.CTypes
import Quasar.Wayland.Skia.Thread
import Quasar.Wayland.Utils.Disposer
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
  deriving (Eq, Hashable, Disposable)

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
  IO (SkiaSurface s)
newSkiaSurface skia width height = runSkiaIO skia.thread do
  (skSurface, storage) <- newSkiaBackendTexture skia width height
  SkiaSurface <$> newFnDisposableVarIO skia.exceptionSink destroySurface SkiaSurfaceState {
    skia,
    skSurface,
    storage,
    width,
    height
  }
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

readSkiaSurfaceStateIO :: SkiaSurface s -> IO (SkiaSurfaceState s)
readSkiaSurfaceStateIO (SkiaSurface var) =
  tryReadDisposableVarIO var >>= \case
    Nothing -> undefined
    Just surfaceState -> pure surfaceState

newtype SkiaImage s = SkiaImage (DisposableVar (Ptr SkImage))
  deriving Disposable

--newtype ManagedSkiaSurface = ManagedSkiaSurface {
--  skSurface :: ForeignPtr SkSurface
--}



newtype SkiaFrame s = SkiaFrame (DisposableVar (SkiaFrameState s))

instance Disposable (SkiaFrame s) where
  getDisposer (SkiaFrame var) = getDisposer var

newSkiaFrame :: SkiaFrameOp s -> STMc NoRetry '[] (SkiaFrame s)
newSkiaFrame op = SkiaFrame <$> newDisposableVar (SkiaFrameLazy op)


data SkiaFrameState s
  = SkiaFrameLazy (SkiaFrameOp s)
  | SkiaFrameSparked Disposer (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))

instance Disposable (SkiaFrameState s) where
  getDisposer (SkiaFrameLazy op) = getDisposer op
  getDisposer (SkiaFrameSparked disposer _) = disposer

data SkiaFrameOp s
  = SkiaFrameExternalDmabuf (ExternalFrame Dmabuf (Skia s))
  | SkiaFrameExternalShm (ExternalFrame ShmBuffer (Skia s))
  --- | SkiaFrameShaderOp
  --- | SkiaFrameExported (Rc (SkiaExportBuffer s))
  | SkiaFrameSinglePixel SinglePixelBuffer

instance Disposable (SkiaFrameOp s) where
  getDisposer (SkiaFrameExternalDmabuf ext) = getDisposer ext
  getDisposer (SkiaFrameExternalShm ext) = getDisposer ext
  getDisposer (SkiaFrameSinglePixel _) = mempty


data SkiaRenderedFrame s
  -- | Owned surface, i.e. the surface belongs to the frame and will be
  -- destroyed with the frame.
  = SkiaRenderedFrameOwnedSurface (SkiaSurface s)
  -- Borrowed surface, i.e. a surface that is used by the frame and will be
  -- "returned" to the owner once the frame is destroyed.
  | SkiaRenderedFrameBorrowedSurface (Borrowed (SkiaSurface s))
  | SkiaRenderedFrameImportedDmabuf (SkiaImage s) (Borrowed (ExternalDmabuf s))
  | SkiaRenderedFrameImportedShm (SkiaImage s) (Borrowed (ExternalShmBuffer s))
  | SkiaRenderedFrameSinglePixel SinglePixelBuffer

instance Disposable (SkiaRenderedFrame s) where
  getDisposer (SkiaRenderedFrameOwnedSurface surface) = getDisposer surface
  getDisposer (SkiaRenderedFrameBorrowedSurface rc) = getDisposer rc
  getDisposer (SkiaRenderedFrameImportedDmabuf image borrowed) = getDisposer image <> getDisposer borrowed
  getDisposer (SkiaRenderedFrameImportedShm image borrowed) = getDisposer image <> getDisposer borrowed
  getDisposer (SkiaRenderedFrameSinglePixel _) = mempty

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
  getExportBufferId (SkiaRenderedFrameOwnedSurface surface) =
    pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameBorrowedSurface (Borrowed _ surface)) =
    pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameImportedShm _ (Borrowed _ (ExternalShmBuffer _ key _))) = do
    pure (SkiaExportBufferIdUnique key)
  getExportBufferId (SkiaRenderedFrameImportedDmabuf _ (Borrowed _ (ExternalDmabuf _ key _))) = do
    pure (SkiaExportBufferIdUnique key)
  getExportBufferId (SkiaRenderedFrameSinglePixel pixel) = pure (SkiaExportBufferIdSinglePixel pixel)

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
      SkiaExportBufferSurface surface -> pure (isDisposed surface)
      SkiaExportBufferImportedShmBuffer importedShmBuffer -> pure (toFuture (getDisposer importedShmBuffer))
      SkiaExportBufferImportedDmabuf importedDmabuf -> pure (toFuture (getDisposer importedDmabuf))
      SkiaExportBufferSinglePixel pixel -> undefined

getExportBuffer :: SkiaRenderedFrame s -> STMc NoRetry '[] (SkiaExportBuffer s)
getExportBuffer (SkiaRenderedFrameOwnedSurface surface) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameBorrowedSurface (Borrowed _ surface)) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameImportedShm _ (Borrowed _ externalShmBuffer)) = do
  pure (SkiaExportBufferImportedShmBuffer externalShmBuffer)
getExportBuffer (SkiaRenderedFrameImportedDmabuf _ (Borrowed _ externalDmabuf)) = do
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
  consumeRc frameRc \(SkiaFrame var) -> do
    frc <- atomically do
      tryReadDisposableVar var >>= \case
        Nothing -> throwC mkDisposedException
        Just (SkiaFrameSparked _ frc) -> pure frc
        Just (SkiaFrameLazy op) -> do
          frc <- cacheFuture =<< sparkFrameOp op
          disposer <- futureDisposerGeneric frc
          -- Prevents the SkiaFrameOp from being disposed with the frame, which
          -- is correct since ownership of the SkiaFrameOp has been passed to
          -- `sparkFrameOp`.
          tryWriteDisposableVar var (SkiaFrameSparked disposer frc)
          pure frc
    renderedFrameRc <- await frc
    atomically do
      -- We are holding an Rc (via `consumeRc`), which holds the SkiaFrame,
      -- which holds the Rc for the SkiaRenderedFrame. This path therefore
      -- _should_ never throw an DisposedException. If this exception is
      -- encountered, someone somewhere disposed the SkiaFrame directly
      -- (ignoring the Rc-based shared ownership), which would be a bug.
      duplicateRc renderedFrameRc

-- Takes ownership of the SkiaFrameOp.
sparkFrameOp :: IsSkiaBackend s => SkiaFrameOp s -> STM (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))
sparkFrameOp (SkiaFrameExternalShm externalFrame@(ExternalFrame _ rc)) = do
  (ExternalShmBuffer skia _ _) <- readRc rc
  queueSkiaIO skia.thread (importShmBuffer externalFrame)
sparkFrameOp (SkiaFrameExternalDmabuf externalFrame@(ExternalFrame _ rc)) = do
  (ExternalDmabuf skia _ _) <- readRc rc
  queueSkiaIO skia.thread (importDmabuf externalFrame)
sparkFrameOp (SkiaFrameSinglePixel singlePixelBuffer) =
  pure <$> newRc (SkiaRenderedFrameSinglePixel singlePixelBuffer)

-- | Takes ownership of a skia surface to create a frame. The surface will
-- be destroyed later.
newFrameConsumeSurface :: SkiaSurface s -> IO (SkiaFrame s)
newFrameConsumeSurface surface = do
  renderedFrameRc <- newRcIO (SkiaRenderedFrameOwnedSurface surface)
  newFrameFromRenderedFrameIO renderedFrameRc

newFrameFromRenderedFrame :: Rc (SkiaRenderedFrame s) -> STMc NoRetry '[] (SkiaFrame s)
newFrameFromRenderedFrame renderedFrameRc =
  SkiaFrame <$> newDisposableVar (SkiaFrameSparked (getDisposer renderedFrameRc) (pure renderedFrameRc))

newFrameFromRenderedFrameIO :: Rc (SkiaRenderedFrame s) -> IO (SkiaFrame s)
newFrameFromRenderedFrameIO renderedFrameRc =
  SkiaFrame <$> newDisposableVarIO (SkiaFrameSparked (getDisposer renderedFrameRc) (pure renderedFrameRc))

makeExportable :: Rc (SkiaRenderedFrame x) -> IO (SkiaFrame s)
makeExportable = undefined


instance IsSkiaBackend s => IsSinglePixelBufferBackend (Skia s) where
  createSinglePixelBufferFrame :: Skia s -> SinglePixelBuffer -> STMc NoRetry '[] (SkiaFrame s)
  createSinglePixelBufferFrame _skia pixel =
    newSkiaFrame (SkiaFrameSinglePixel pixel)



data ExternalShmBuffer s = ExternalShmBuffer (Skia s) Unique ShmBuffer

instance Disposable (ExternalShmBuffer s) where
  getDisposer (ExternalShmBuffer _ _ borrowedBuffer) = getDisposer borrowedBuffer

instance IsSkiaBackend s => IsBufferBackend ShmBuffer (Skia s) where
  type ExternalBuffer ShmBuffer (Skia s) = (ExternalShmBuffer s)

  newExternalBuffer :: Skia s -> ShmBuffer -> STMc NoRetry '[] (ExternalShmBuffer s)
  newExternalBuffer skia shmBuffer = do
    key <- newUniqueSTM
    pure (ExternalShmBuffer skia key shmBuffer)

  wrapExternalFrame :: ExternalFrame ShmBuffer (Skia s) -> STMc NoRetry '[DisposedException] (SkiaFrame s)
  wrapExternalFrame externalFrame = liftSTMc do
    newSkiaFrame (SkiaFrameExternalShm externalFrame)

importShmBuffer :: ExternalFrame ShmBuffer (Skia s) -> SkiaIO (Rc (SkiaRenderedFrame s))
importShmBuffer (ExternalFrame _ rc) = liftIO do
  x@(ExternalShmBuffer skia _ shmBuffer) <- readRcIO rc

  let offset = fromIntegral shmBuffer.offset
  let rowBytes = fromIntegral shmBuffer.stride
  let height = fromIntegral shmBuffer.height
  let width = fromIntegral shmBuffer.width
  let format = shmBuffer.format
  let grDirectContext = skia.grDirectContext

  pool <- readRcIO shmBuffer.pool
  size <- atomicallyC $ readObservable pool.size
  (disposer, ptr) <- mmapDisposer MmapReadOnly pool.fd (fromIntegral size)

  rasterReleaseProc <- $(C.mkFunPtr [t|Ptr () -> Ptr () -> IO ()|]) \_ _ -> do
    traceIO $ "Callback from C: releasing shm memory"
    dispose disposer
    -- TODO propagate memory release up
    -- That would invalidate the ShmBuffer, so it's only correct if we don't
    -- want to forward the buffer.

  skImage <- [CPPU.throwBlock|SkImage* {
    GrDirectContext* grDirectContext = $(GrDirectContext* grDirectContext);

    auto colorType = $(uint32_t format) == 0 ? kRGBA_8888_SkColorType : kRGB_888x_SkColorType;
    SkImageInfo info = SkImageInfo::Make($(int width), $(int height), colorType, kPremul_SkAlphaType);
    SkPixmap pixmap(info, (void*)($(uint8_t* ptr) + $(size_t offset)), $(size_t rowBytes));

    auto skImage = SkImages::RasterFromPixmap(pixmap, $(SkImages::RasterReleaseProc rasterReleaseProc), nullptr);
    return skImage.release();
  }|]

  skiaImage <- liftIO $ newSkiaImage skia skImage

  liftIO $ newRcIO (SkiaRenderedFrameImportedShm skiaImage (Borrowed (getDisposer rc) x))



data SkiaDmabufProperties = SkiaDmabufProperties {
  version1Formats :: [DrmFormat],
  version3FormatTable :: DmabufFormatTable,
  feedback :: CompiledDmabufFeedback
}

data ExternalDmabuf s = ExternalDmabuf (Skia s) Unique Dmabuf

instance Disposable (ExternalDmabuf s) where
  getDisposer (ExternalDmabuf _ _ borrow) = getDisposer borrow


instance IsSkiaBackend s => IsBufferBackend Dmabuf (Skia s) where
  type ExternalBuffer Dmabuf (Skia s) = (ExternalDmabuf s)

  newExternalBuffer :: Skia s -> Dmabuf -> STMc NoRetry '[] (ExternalDmabuf s)
  newExternalBuffer skia dmabuf = do
    key <- newUniqueSTM
    pure (ExternalDmabuf skia key dmabuf)

  wrapExternalFrame :: ExternalFrame Dmabuf (Skia s) -> STMc NoRetry '[DisposedException] (SkiaFrame s)
  wrapExternalFrame externalFrame = liftSTMc do
    newSkiaFrame (SkiaFrameExternalDmabuf externalFrame)

importDmabuf :: IsSkiaBackend s => ExternalFrame Dmabuf (Skia s) -> SkiaIO (Rc (SkiaRenderedFrame s))
importDmabuf (ExternalFrame _ rc) = do
  x@(ExternalDmabuf skia _ dmabuf) <- readRcIO rc

  skImage <- skiaImportDmabuf skia dmabuf
  skiaImage <- liftIO $ newSkiaImage skia skImage

  liftIO $ newRcIO (SkiaRenderedFrameImportedDmabuf skiaImage (Borrowed (getDisposer rc) x))


newSkiaImage :: Skia s -> Ptr SkImage -> IO (SkiaImage s)
newSkiaImage skia skImage =
  SkiaImage <$> newFnDisposableVarIO skia.exceptionSink destroyImage skImage
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
skiaGlobals skia = [skiaDmabufGlobal skia]


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
