{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia (
  Skia(..),
  initializeSkia,
  SkiaSurface(..),
  SkiaSurfaceState(..),
  readSkiaSurfaceState,
  newSkiaSurface,
  newFrameConsumeSurface,

  clearSkiaSurface,
  flushAndSubmit,

  -- * Internal
  IsSkiaBackend(..),
  readSkiaSurfaceStateIO,
  --ManagedSkiaSurface,
) where

import Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Foreign.ForeignPtr
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Cpp qualified as CPP
import Language.C.Inline.Cpp.Unsafe qualified as CPPU
import Language.C.Types qualified as C
import Quasar.Exceptions (AsyncException(..), mkDisposedException, ExceptionSink)
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future
import Quasar.Prelude
import Quasar.Resources
import Quasar.Resources.DisposableVar
import Quasar.Resources.Rc
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Skia.CTypes
import Quasar.Wayland.Skia.Thread


C.context (CPP.cppCtx <> C.fptrCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
    (C.TypeName "GrDirectContext", [t|GrDirectContext|]),
    (C.TypeName "SkSurface", [t|SkSurface|])
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
class
  (Eq (SkiaTextureStorage s), Hashable (SkiaTextureStorage s), Typeable s) =>
  IsSkiaBackend s
  where
    type SkiaBackendContext s
    type SkiaTextureStorage s
    initializeSkiaBackend :: SkiaIO (ForeignPtr GrDirectContext, SkiaBackendContext s)
    newSkiaBackendTexture :: Skia s -> Int32 -> Int32 -> SkiaIO (ForeignPtr SkSurface, SkiaTextureStorage s)
    destroySkiaTextureStorage :: SkiaSurfaceState s -> SkiaIO ()
    exportSkiaSurfaceDmabuf :: SkiaSurfaceState s -> SkiaIO Dmabuf

data Skia s = Skia {
  exceptionSink :: ExceptionSink,
  thread :: SkiaThread,
  grDirectContext :: ForeignPtr GrDirectContext,
  context :: SkiaBackendContext s
}

initializeSkia :: forall s. IsSkiaBackend s => IO (Skia s)
initializeSkia = do
  let exceptionSink = loggingExceptionSink
  thread <- newSkiaThread
  future <- atomically (queueSkiaIO thread (initializeSkiaBackend @s))
  (grDirectContext, context) <- await future
  pure Skia {
    exceptionSink,
    thread,
    grDirectContext,
    context
  }

newtype SkiaSurface s = SkiaSurface (DisposableVar (SkiaSurfaceState s))
  deriving (Eq, Hashable, Disposable)

data SkiaSurfaceState s = SkiaSurfaceState {
  skia :: Skia s,
  skSurface :: ForeignPtr SkSurface,
  storage :: SkiaTextureStorage s,
  width :: Int32,
  height :: Int32
}

newSkiaSurface ::
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
    destroySurface :: forall s. IsSkiaBackend s => SkiaSurfaceState s -> IO ()
    destroySurface state = runSkiaIO skia.thread do
      destroySkiaTextureStorage @s state
      liftIO $ finalizeForeignPtr state.skSurface

skiaSurfaceKey :: SkiaSurface s -> Unique
skiaSurfaceKey (SkiaSurface var) = disposerElementKey var

readSkiaSurfaceState :: SkiaSurface s -> STMc NoRetry '[] (SkiaSurfaceState s)
readSkiaSurfaceState (SkiaSurface var) =
  tryReadDisposableVar var >>= \case
    Nothing -> undefined
    Just surfaceState -> pure surfaceState

readSkiaSurfaceStateIO :: SkiaSurface s -> IO (SkiaSurfaceState s)
readSkiaSurfaceStateIO (SkiaSurface var) =
  tryReadDisposableVarIO var >>= \case
    Nothing -> undefined
    Just surfaceState -> pure surfaceState

data ReadonlySkiaSurface = ReadonlySkiaSurface {
  skSurface :: ForeignPtr SkSurface,
  disposer :: Disposer
}

--newtype ManagedSkiaSurface = ManagedSkiaSurface {
--  skSurface :: ForeignPtr SkSurface
--}


data Borrowed a = Borrowed Disposer a

instance Disposable (Borrowed a) where
  getDisposer (Borrowed disposer _) = disposer


data ImportedDmabuf = ImportedDmabuf Unique Dmabuf


newtype SkiaFrame s = SkiaFrame (DisposableVar (SkiaFrameState s))

instance Disposable (SkiaFrame s) where
  getDisposer (SkiaFrame var) = getDisposer var

data SkiaFrameState s
  = SkiaFrameLazy (SkiaFrameOp s)
  | SkiaFrameSparked Disposer (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))

instance Disposable (SkiaFrameState s) where
  getDisposer (SkiaFrameLazy op) = getDisposer op
  getDisposer (SkiaFrameSparked disposer _) = disposer

data SkiaFrameOp s
  = SkiaFrameImportOwnedDmabuf (Skia s) (Rc Dmabuf)
  --- | SkiaFrameShaderOp
  --- | SkiaFrameExported (Rc (SkiaExportBuffer s))
  | SkiaFrameSinglePixel SkiaSinglePixelBuffer

instance Disposable (SkiaFrameOp s) where
  getDisposer (SkiaFrameImportOwnedDmabuf _ rc) = getDisposer rc
  getDisposer (SkiaFrameSinglePixel _) = mempty

data SkiaSinglePixelBuffer = SkiaSinglePixelBuffer Word32 Word32 Word32 Word32
  deriving (Eq, Generic)

instance Hashable SkiaSinglePixelBuffer


data SkiaRenderedFrame s
  -- | Owned surface, i.e. the surface belongs to the frame and will be
  -- destroyed with the frame.
  = SkiaRenderedFrameOwnedSurface (SkiaSurface s)
  -- Borrowed surface, i.e. a surface that is used by the frame and will be
  -- "returned" to the owner once the frame is destroyed.
  | SkiaRenderedFrameBorrowedSurface (Rc (Borrowed (SkiaSurface s)))
  | SkiaRenderedFrameImportedDmabuf (Rc ReadonlySkiaSurface) (Rc ImportedDmabuf)
  | SkiaRenderedFrameSinglePixel SkiaSinglePixelBuffer

instance Disposable (SkiaRenderedFrame s) where
  getDisposer (SkiaRenderedFrameOwnedSurface surface) = getDisposer surface
  getDisposer (SkiaRenderedFrameBorrowedSurface rc) = getDisposer rc
  getDisposer (SkiaRenderedFrameImportedDmabuf x y) = getDisposer x <> getDisposer y
  getDisposer (SkiaRenderedFrameSinglePixel _) = mempty

newtype SkiaClientBufferManager s = SkiaClientBufferManager {
  dmabufSingleton :: ClientDmabufSingleton
}


data SkiaExportBuffer s
  = SkiaExportBufferSurface (SkiaSurface s)
  | SkiaExportBufferImportedDmabuf ImportedDmabuf
  | SkiaExportBufferSinglePixel SkiaSinglePixelBuffer

data SkiaExportBufferId
  = SkiaExportBufferIdUnique Unique
  | SkiaExportBufferIdSinglePixel SkiaSinglePixelBuffer
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

  getExportBufferId :: SkiaRenderedFrame s -> STMc NoRetry '[] SkiaExportBufferId
  getExportBufferId (SkiaRenderedFrameOwnedSurface surface) =
      pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameBorrowedSurface borrowedSurfaceRc) =
    tryReadRc borrowedSurfaceRc >>= \case
      Nothing -> undefined
      Just (Borrowed _ surface) ->
        pure (SkiaExportBufferIdUnique (skiaSurfaceKey surface))
  getExportBufferId (SkiaRenderedFrameImportedDmabuf _ dmabufRc) =
    tryReadRc dmabufRc >>= \case
      Nothing -> undefined
      Just (ImportedDmabuf key _) -> pure (SkiaExportBufferIdUnique key)
  getExportBufferId (SkiaRenderedFrameSinglePixel pixel) = pure (SkiaExportBufferIdSinglePixel pixel)

  exportWlBuffer :: SkiaClientBufferManager s -> SkiaRenderedFrame s -> IO (NewObject 'Client Interface_wl_buffer)
  exportWlBuffer manager renderedFrame =
    atomicallyC (getExportBuffer renderedFrame) >>= \case
      SkiaExportBufferSurface surface -> exportSkiaSurface manager surface
      SkiaExportBufferImportedDmabuf importedDmabuf -> undefined
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
      SkiaExportBufferImportedDmabuf importedDmabuf -> undefined
      SkiaExportBufferSinglePixel pixel -> undefined

getExportBuffer :: SkiaRenderedFrame s -> STMc NoRetry '[] (SkiaExportBuffer s)
getExportBuffer (SkiaRenderedFrameOwnedSurface surface) =
  pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameBorrowedSurface borrowedSurfaceRc) =
  tryReadRc borrowedSurfaceRc >>= \case
    Nothing -> undefined
    Just (Borrowed _ surface) -> pure (SkiaExportBufferSurface surface)
getExportBuffer (SkiaRenderedFrameImportedDmabuf _ importedDmabuf) = undefined
getExportBuffer (SkiaRenderedFrameSinglePixel pixel) =
  pure (SkiaExportBufferSinglePixel pixel)

exportSkiaSurface :: forall s. IsSkiaBackend s => SkiaClientBufferManager s -> SkiaSurface s -> IO (NewObject 'Client Interface_wl_buffer)
exportSkiaSurface manager surface = do
  surfaceState <- readSkiaSurfaceStateIO surface
  dmabuf <- runSkiaIO surfaceState.skia.thread $ exportSkiaSurfaceDmabuf surfaceState
  atomicallyC $ consumeDmabufExportWlBuffer manager.dmabufSingleton dmabuf

importDmabuf :: Rc Dmabuf -> SkiaIO (Rc (SkiaRenderedFrame s))
importDmabuf dmabuf = do
  undefined

renderFrameInternal :: Rc (SkiaFrame s) -> IO (Rc (SkiaRenderedFrame s))
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

sparkFrameOp :: SkiaFrameOp s -> STM (Future '[AsyncException] (Rc (SkiaRenderedFrame s)))
sparkFrameOp (SkiaFrameImportOwnedDmabuf skia dmabuf) =
  queueSkiaIO skia.thread (importDmabuf dmabuf)
sparkFrameOp (SkiaFrameSinglePixel singlePixelBuffer) =
  pure <$> newRc (SkiaRenderedFrameSinglePixel singlePixelBuffer)

-- | Takes ownership of a skia surface to create a frame. The surface will
-- be destroyed later.
newFrameConsumeSurface :: SkiaSurface s -> IO (SkiaFrame s)
newFrameConsumeSurface surface = do
  renderedFrameRc <- newRcIO (SkiaRenderedFrameOwnedSurface surface)
  newFrameFromRenderedFrame renderedFrameRc

newFrameFromRenderedFrame :: Rc (SkiaRenderedFrame s) -> IO (SkiaFrame s)
newFrameFromRenderedFrame renderedFrameRc =
  SkiaFrame <$> newDisposableVarIO (SkiaFrameSparked (getDisposer renderedFrameRc) (pure renderedFrameRc))

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


clearSkiaSurfaceInternal :: ForeignPtr SkSurface -> SkiaIO ()
clearSkiaSurfaceInternal skSurface = liftIO do
  [CPPU.throwBlock|void {
    SkCanvas* skCanvas = ($fptr-ptr:(SkSurface* skSurface))->getCanvas();
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
    GrDirectContext* grDirectContext = $fptr-ptr:(GrDirectContext* grDirectContext);
    grDirectContext->flushAndSubmit($fptr-ptr:(SkSurface* skSurface));
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
    GrDirectContext* grDirectContext = $fptr-ptr:(GrDirectContext* grDirectContext);
    grDirectContext->flushAndSubmit($fptr-ptr:(SkSurface* skSurface), GrSyncCpu::kYes);
  }|]
