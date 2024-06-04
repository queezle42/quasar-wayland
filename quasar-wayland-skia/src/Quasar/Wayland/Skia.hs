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
import Quasar.Exceptions (AsyncException(..), mkDisposedException, ExceptionSink, DisposedException)
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future
import Quasar.Prelude
import Quasar.Resources
import Quasar.Resources.DisposableVar
import Quasar.Resources.Rc
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
import Quasar.Wayland.Utils.Resources


C.context (CPP.cppCtx <> C.fptrCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
    (C.TypeName "GrDirectContext", [t|GrDirectContext|]),
    (C.TypeName "SkSurface", [t|SkSurface|]),
    (C.TypeName "SkImage", [t|SkImage|])
  ]
})

C.include "<iostream>"

C.include "include/core/SkGraphics.h"
C.include "include/core/SkSurface.h"
C.include "include/core/SkCanvas.h"
C.include "include/core/SkColorSpace.h"

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
  disposer <- atomicallyC $ newUnmanagedIODisposer (destroySkia thread grDirectContext context) exceptionSink
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
  = SkiaFrameExternalDmabuf (Skia s) TDisposer (Rc ExternalDmabuf)
  --- | SkiaFrameShaderOp
  --- | SkiaFrameExported (Rc (SkiaExportBuffer s))
  | SkiaFrameSinglePixel SinglePixelBuffer

instance Disposable (SkiaFrameOp s) where
  getDisposer (SkiaFrameExternalDmabuf _ disposer rc) = getDisposer disposer <> getDisposer rc
  getDisposer (SkiaFrameSinglePixel _) = mempty


data SkiaRenderedFrame s
  -- | Owned surface, i.e. the surface belongs to the frame and will be
  -- destroyed with the frame.
  = SkiaRenderedFrameOwnedSurface (SkiaSurface s)
  -- Borrowed surface, i.e. a surface that is used by the frame and will be
  -- "returned" to the owner once the frame is destroyed.
  | SkiaRenderedFrameBorrowedSurface (Borrowed (SkiaSurface s))
  | SkiaRenderedFrameImportedDmabuf TDisposer (SkiaImage s) (Rc ExternalDmabuf)
  | SkiaRenderedFrameSinglePixel SinglePixelBuffer

instance Disposable (SkiaRenderedFrame s) where
  getDisposer (SkiaRenderedFrameOwnedSurface surface) = getDisposer surface
  getDisposer (SkiaRenderedFrameBorrowedSurface rc) = getDisposer rc
  getDisposer (SkiaRenderedFrameImportedDmabuf disposer image rc) = getDisposer disposer <> getDisposer image <> getDisposer rc
  getDisposer (SkiaRenderedFrameSinglePixel _) = mempty

newtype SkiaClientBufferManager s = SkiaClientBufferManager {
  dmabufSingleton :: ClientDmabufSingleton
}


data SkiaExportBuffer s
  = SkiaExportBufferSurface (SkiaSurface s)
  | SkiaExportBufferImportedDmabuf ExternalDmabuf
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
    HasCallStack =>
    SkiaRenderedFrame s -> STMc NoRetry '[DisposedException] SkiaExportBufferId
  getExportBufferId (SkiaRenderedFrameOwnedSurface surface) =
    pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameBorrowedSurface (Borrowed _ surface)) =
    pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameImportedDmabuf _ _ rc) = do
    (ExternalDmabuf key _) <- readRc rc
    pure (SkiaExportBufferIdUnique key)
  getExportBufferId (SkiaRenderedFrameSinglePixel pixel) = pure (SkiaExportBufferIdSinglePixel pixel)

  exportWlBuffer :: SkiaClientBufferManager s -> SkiaRenderedFrame s -> IO (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer manager renderedFrame =
    atomicallyC (getExportBuffer renderedFrame) >>= \case
      SkiaExportBufferSurface surface -> exportSkiaSurface manager surface
      SkiaExportBufferImportedDmabuf (ExternalDmabuf _ (Borrowed _ dmabuf)) ->
        atomicallyC $ sharedDmabufExportWlBuffer manager.dmabufSingleton dmabuf
      SkiaExportBufferSinglePixel pixel -> undefined

  syncExportBuffer :: SkiaRenderedFrame s -> IO ()
  syncExportBuffer renderedFrame =
    atomicallyC (getExportBuffer renderedFrame) >>= \case
      SkiaExportBufferSurface surface ->
        -- TODO use fence instead of full cpu sync if available
        flushAndSync surface
      SkiaExportBufferImportedDmabuf _ ->
        pure () -- upstream is responsible for sync
      SkiaExportBufferSinglePixel _ ->
        pure () -- trivial buffer, no sync required

  getExportBufferDestroyedFuture :: SkiaRenderedFrame s -> STMc NoRetry '[] (Future '[] ())
  getExportBufferDestroyedFuture renderedFrame = do
    getExportBuffer renderedFrame >>= \case
      SkiaExportBufferSurface surface -> pure (isDisposed surface)
      SkiaExportBufferImportedDmabuf importedDmabuf -> pure (toFuture (getDisposer importedDmabuf))
      SkiaExportBufferSinglePixel pixel -> undefined

getExportBuffer :: SkiaRenderedFrame s -> STMc NoRetry '[] (SkiaExportBuffer s)
getExportBuffer (SkiaRenderedFrameOwnedSurface surface) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameBorrowedSurface (Borrowed _ surface)) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameImportedDmabuf _ _ rc) = do
  tryReadRc rc >>= \case
    Nothing -> undefined
    Just externalDmabuf -> pure (SkiaExportBufferImportedDmabuf externalDmabuf)
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
          tryWriteDisposableVar var (SkiaFrameSparked disposer frc)
          pure frc
    renderedFrameRc <- await frc
    atomically do
      tryDuplicateRc renderedFrameRc >>= \case
        Nothing ->
          -- We are holding an Rc (via `consumeRc`), which holds the SkiaFrame,
          -- which holds the Rc for the SkiaRenderedFrame. If this exception
          -- is encountered, someone somewhere disposed the SkiaFrame directly,
          -- which is a bug.
          throwC mkDisposedException
        Just duplicatedRc -> pure duplicatedRc

sparkFrameOp :: IsSkiaBackend s => SkiaFrameOp s -> STM (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))
sparkFrameOp (SkiaFrameExternalDmabuf skia frameRelease rc) =
  queueSkiaIO skia.thread (importDmabuf skia frameRelease rc)
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



instance IsSkiaBackend s => IsSinglePixelBufferBackend (Skia s) where
  createSinglePixelBufferFrame :: Skia s -> SinglePixelBuffer -> STMc NoRetry '[] (SkiaFrame s)
  createSinglePixelBufferFrame _skia pixel =
    newSkiaFrame (SkiaFrameSinglePixel pixel)
    --newFrameFromRenderedFrame =<< newRc (SkiaRenderedFrameSinglePixel pixel)



data ExternalShmBuffer

instance Disposable ExternalShmBuffer where
  getDisposer = \case {}

instance IsSkiaBackend s => IsBufferBackend ShmBuffer (Skia s) where
  type ExternalBuffer ShmBuffer (Skia s) = ExternalShmBuffer

  newExternalBuffer :: Skia s -> Borrowed ShmBuffer -> STMc NoRetry '[] ExternalShmBuffer
  newExternalBuffer _skia dmabuf = undefined
    --key <- newUniqueSTM
    --pure (ExternalDmabuf key dmabuf)

  createExternalBufferFrame :: Skia s -> TDisposer -> Rc ExternalShmBuffer -> STMc NoRetry '[] (SkiaFrame s)
  createExternalBufferFrame skia tdisposer rc = undefined
    --newSkiaFrame (SkiaFrameExternalDmabuf skia tdisposer rc)



data SkiaDmabufProperties = SkiaDmabufProperties {
  version1Formats :: [DrmFormat],
  version3FormatTable :: DmabufFormatTable,
  feedback :: CompiledDmabufFeedback
}

data ExternalDmabuf = ExternalDmabuf Unique (Borrowed Dmabuf)

instance Disposable ExternalDmabuf where
  getDisposer (ExternalDmabuf key borrow) = getDisposer borrow


instance IsSkiaBackend s => IsBufferBackend Dmabuf (Skia s) where
  type ExternalBuffer Dmabuf (Skia s) = ExternalDmabuf

  newExternalBuffer :: Skia s -> Borrowed Dmabuf -> STMc NoRetry '[] ExternalDmabuf
  newExternalBuffer _skia dmabuf = do
    key <- newUniqueSTM
    pure (ExternalDmabuf key dmabuf)

  createExternalBufferFrame :: Skia s -> TDisposer -> Rc ExternalDmabuf -> STMc NoRetry '[] (SkiaFrame s)
  createExternalBufferFrame skia tdisposer rc = do
    newSkiaFrame (SkiaFrameExternalDmabuf skia tdisposer rc)

importDmabuf :: IsSkiaBackend s => Skia s -> TDisposer -> Rc ExternalDmabuf -> SkiaIO (Rc (SkiaRenderedFrame s))
importDmabuf skia frameRelease rc = do
  atomicallyC (tryReadRc rc) >>= \case
    Nothing -> undefined
    Just (ExternalDmabuf _ (Borrowed _ dmabuf)) -> do
      skImage <- skiaImportDmabuf skia dmabuf
      skiaImage <- liftIO $ newSkiaImage skia skImage

      liftIO $ newRcIO (SkiaRenderedFrameImportedDmabuf frameRelease skiaImage rc)


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
