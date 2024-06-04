module Quasar.Wayland.Client.Surface (
  -- * Image backend
  ClientBufferBackend(..),
  getClientBufferManager,

  -- * Surface
  ClientSurfaceManager,
  getClientSurfaceManager,

  ClientSurface,
  newClientSurface,
) where

import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Typeable (Typeable)
import Quasar.Async.Fork (forkSTM_)
import Quasar.Exceptions
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future (Future, Promise, ToFuture (toFuture), newPromise, peekFuture, fulfillPromise, MonadAwait (await), callOnceCompleted_)
import Quasar.Prelude
import Quasar.Resources (Disposer, disposeEventually, disposeEventually_, isDisposed, getDisposer)
import Quasar.Resources.Rc
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appRect)
import Quasar.Wayland.Shared.Surface


class (RenderBackend b, Typeable (ClientBufferManager b), Hashable (ExportBufferId b)) => ClientBufferBackend b where
  type ClientBufferManager b
  type ExportBufferId b
  type RenderedFrame b
  newClientBufferManager :: WaylandClient -> STMc NoRetry '[SomeException] (ClientBufferManager b)

  -- | Render a frame into a buffer. The frame might be rendered into a new-,
  -- or into an existing buffer (chosen by the implementation of `renderFrame`).
  -- The buffer can then be exported by using `exportWlBuffer`. Exported buffers
  -- stay mapped to the server and might be reused, if the buffer is already
  -- mapped it should not be mapped again. To identity which buffer the frame
  -- has been rendered to, use `getExportBufferId`.
  --
  -- Disposing the `RenderedFrame` rc will release resources associated with the
  -- frame (if applicable) and releases the internal buffer so it can be reused
  -- or destroyed by the buffer owner.
  --
  -- Takes responsibility for the frame rc. The caller is responsible for the
  -- returned RenderedFrame rc.
  --
  -- The exported buffers contents should not change until it is unlocked by the
  -- caller.
  renderFrame :: Rc (Frame b) -> IO (Rc (RenderedFrame b))

  -- | Look up the identity for an `ExportBuffer`.
  --
  -- This should return the id of an internal buffer that the frame has been
  -- rendered to.
  getExportBufferId :: HasCallStack => RenderedFrame b -> STMc NoRetry '[DisposedException] (ExportBufferId b)

  -- | Called by the `Surface`-implementation when a buffer should be mapped
  -- from the wayland client to the wayland server. This usually shares memory
  -- from the client to the server.
  --
  -- This targets the internal buffer that is currently in use by the
  -- `RenderedFrame` (this should match `getExportBufferId`). Even after
  -- disposing the rendered frame, the buffer stays mapped to the server.
  --
  -- The caller takes ownership of the resulting @wl_buffer@ and will attach the
  -- event handler.
  --
  -- The @RenderedFrame@ argument is owned by the caller and must not be
  -- disposed by the callee.
  exportWlBuffer :: ClientBufferManager b -> RenderedFrame b -> IO (NewObject 'Client Interface_wl_buffer)

  syncExportBuffer :: RenderedFrame b -> IO ()

  -- | TODO rewrite documentation for future-based reimplementation of this function.
  --
  -- Attach a callback to a buffer. The callback must be called when the
  -- buffer that is backing the `RenderedFrame` argument is released.
  --
  -- Usually an exported buffer can be reused. When the underlying texture or
  -- buffer is destroyed by the rendering engine (or another buffer source), the
  -- buffer has to be unmapped from the wayland server. This event is used as a
  -- signal that the @wl_buffer@ that belongs to the texture/buffer should be
  -- destroyed.
  --
  -- If the buffer can't be reused the event should be called when the
  -- `RenderedFrame` is disposed.
  --
  -- The `RenderedFrame` argument is owned by the caller and must not be disposed by
  -- the callee.
  getExportBufferDestroyedFuture :: RenderedFrame b -> STMc NoRetry '[] (Future '[] ())

getClientBufferManager :: forall b. ClientBufferBackend b => WaylandClient -> STMc NoRetry '[SomeException] (ClientBufferManager b)
getClientBufferManager client =
  getClientComponent (newClientBufferManager @b client) client


-- | Requests a wl_buffer for a locked buffer and changes it's state to attached.
--
-- Takes ownership of the frame rc.
requestWlBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> ExportBufferId b -> Rc (RenderedFrame b) -> IO (Object 'Client Interface_wl_buffer)
requestWlBuffer client bufferId frame = do
  join $ atomically do
    bufferMap <- readTVar client.bufferMap
    case HM.lookup bufferId bufferMap of
      -- Buffer is already exported to client
      Just clientBuffer -> do
        readTVar clientBuffer.state >>= \case
          Released -> do
            -- Unlock buffer on wl_buffer release
            writeTVar clientBuffer.state (Attached (getDisposer frame))
            pure (pure clientBuffer.wlBuffer)

          -- TODO using (z)wp_linux_buffer_release a buffer could be attached to multiple surfaces
          Attached _ -> pure $ newSingleUseWlBuffer client frame

          -- This should not happen - disposed ClientBuffers are removed from the buffer map
          Destroyed -> throwM (userError "ClientBuffer.requestClientBuffer called on a destroyed ClientBuffer")
      -- Buffer is currently not exported to client
      Nothing -> pure do
        clientBuffer <- newClientBuffer client bufferId frame
        pure clientBuffer.wlBuffer



-- | Create a reusable client buffer in attached state.
--
-- Takes ownership of the frame rc.
newClientBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> ExportBufferId b -> Rc (RenderedFrame b) -> IO (ClientBuffer b)
newClientBuffer client bufferId frame = do
  atomically (tryReadRc frame) >>= \case
    Nothing -> throwM (userError "ClientBuffer.newClientBuffer called with a disposed buffer")
    Just rawFrame -> do
      wlBuffer <- exportWlBuffer @b client.bufferManager rawFrame

      state <- newTVarIO (Attached (getDisposer frame))
      let clientBuffer = ClientBuffer {
        wlBuffer,
        state
      }

      atomicallyC do
        attachFinalizer wlBuffer do
          modifyTVar client.bufferMap (HM.delete bufferId)
          readTVar clientBuffer.state >>= \case
            Attached lockDisposer -> do
              writeTVar clientBuffer.state Destroyed
              -- TODO do we need to handle the disposer future?
              disposeEventually_ lockDisposer
            Released ->
              writeTVar clientBuffer.state Destroyed
            Destroyed -> pure ()

        setEventHandler wlBuffer EventHandler_wl_buffer {
          release = releaseClientBuffer clientBuffer
        }
        exportBufferDestroyedFuture <- getExportBufferDestroyedFuture @b rawFrame
        callOnceCompleted_ exportBufferDestroyedFuture (\_ -> destroyClientBuffer clientBuffer)
        modifyTVar client.bufferMap (HM.insert bufferId clientBuffer)
        pure clientBuffer

releaseClientBuffer :: ClientBuffer b -> STMc NoRetry '[SomeException] ()
releaseClientBuffer clientBuffer = do
  readTVar clientBuffer.state >>= \case
    Attached lockDisposer -> do
      traceM "ClientBuffer: releasing"
      writeTVar clientBuffer.state Released
      -- TODO do we need to handle the disposer future?
      disposeEventually_ lockDisposer
      traceM "ClientBuffer: released"
    Released -> traceM "ClientBuffer: Duplicate release"
    Destroyed -> traceM "ClientBuffer: Release called but is already destroyed"

destroyClientBuffer :: ClientBuffer b -> STMc NoRetry '[] ()
destroyClientBuffer clientBuffer = do
  tryCall clientBuffer.wlBuffer.destroy


-- | Since `release` is undefined when a buffer is attached to multiple surfaces,
-- using multiple wl_buffer objects for the same `Image` might be required.
--
-- This function creates single-use buffers, that is, buffers that are destroyed
-- as soon as they receive a `release`-event.
--
-- Takes ownership of the frame rc.
newSingleUseWlBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> Rc (RenderedFrame b) -> IO (Object 'Client Interface_wl_buffer)
newSingleUseWlBuffer surfaceManager frame = do
  atomicallyC (tryReadRc frame) >>= \case
    Nothing -> throwM (userError "ClientBuffer.newSingleUseWlBuffer called with a disposed buffer")
    Just rawFrame -> do
      wlBuffer <- exportWlBuffer @b surfaceManager.bufferManager rawFrame

      atomicallyC do
        -- Attach ownership/cleanup of buffer lock to wl_buffer
        -- TODO do we need to handle the disposer future?
        attachFinalizer wlBuffer (disposeEventually_ frame)
        setEventHandler wlBuffer EventHandler_wl_buffer {
          release = wlBuffer.destroy
        }
        pure wlBuffer


data ClientSurfaceManager b = ClientSurfaceManager {
  bufferManager :: ClientBufferManager b,
  wlCompositor :: Object 'Client Interface_wl_compositor,
  bufferMap :: TVar (HashMap (ExportBufferId b) (ClientBuffer b))
}

data ClientSurface b = ClientSurface {
  surfaceManager :: ClientSurfaceManager b,
  wlSurface :: Object 'Client Interface_wl_surface,
  pendingCommit :: TVar (Maybe (PendingCommit b)),
  pendingDisposerFuture :: TVar (Future '[] ())
}

data PendingCommit b = PendingCommit {
  commit :: SurfaceCommit b,
  commitCompletedPromise :: Promise ()
}

data ClientBufferState = Attached Disposer | Released | Destroyed

data ClientBuffer b = ClientBuffer {
  wlBuffer :: Object 'Client Interface_wl_buffer,
  state :: TVar ClientBufferState
}

getClientSurfaceManager :: ClientBufferBackend b => WaylandClient -> STMc NoRetry '[SomeException] (ClientSurfaceManager b)
getClientSurfaceManager client =
  getClientComponent (newClientSurfaceManager client) client

newClientSurfaceManager :: forall b. ClientBufferBackend b => WaylandClient -> STMc NoRetry '[SomeException] (ClientSurfaceManager b)
newClientSurfaceManager client = do
  bufferManager <- getClientBufferManager @b client
  wlCompositor <- getClientComponent @(Object 'Client Interface_wl_compositor) (liftSTMc (newWlCompositor client)) client
  bufferMap <- newTVar mempty
  pure ClientSurfaceManager {
    bufferManager,
    wlCompositor,
    bufferMap
  }

newWlCompositor :: WaylandClient -> STMc NoRetry '[SomeException] (Object 'Client Interface_wl_compositor)
newWlCompositor client = do
  wlCompositor <- bindSingleton client.registry maxVersion
  -- wl_compositor does not have any events. Setting `()` as the event handler will produce a type error if that changes in the future.
  setEventHandler wlCompositor ()
  pure wlCompositor


newClientSurface ::
  forall b a.
  ClientBufferBackend b =>
  WaylandClient ->
  (Object 'Client Interface_wl_surface -> STMc NoRetry '[SomeException] a) ->
  STMc NoRetry '[SomeException] (ClientSurface b, a)
newClientSurface client initializeSurfaceRoleFn = do
  surfaceManager <- getClientSurfaceManager @b client
  wlSurface <- liftSTMc surfaceManager.wlCompositor.create_surface

  -- TODO: add finalizer, so that the surface is destroyed with the wlSurface
  -- TODO event handling
  setEventHandler wlSurface EventHandler_wl_surface {
    enter = \_ -> pure (),
    leave = \_ -> pure ()
  }
  fnResult <- initializeSurfaceRoleFn wlSurface

  -- Commit role
  liftSTMc wlSurface.commit

  pendingCommit <- newTVar Nothing
  pendingDisposerFuture <- newTVar mempty

  let clientSurface = ClientSurface { surfaceManager, wlSurface, pendingCommit, pendingDisposerFuture }

  -- Spawn worker thread. Worker thread will be stopped when the ClientSurface
  -- is garbage collected.
  forkSTM_ (clientSurfaceCommitWorker clientSurface) loggingExceptionSink

  pure (clientSurface, fnResult)

instance ClientBufferBackend b => IsSurfaceDownstream b (ClientSurface b) where
  commitSurfaceDownstream = commitClientSurface
  unmapSurfaceDownstream = undefined

commitClientSurface :: ClientSurface b -> SurfaceCommit b -> STMc NoRetry '[SomeException] (Future '[] ())
commitClientSurface surface commit = do
  readTVar surface.pendingCommit >>= \case

    -- No pending commit
    Nothing -> do
      -- Set commit as pending commit
      commitCompletedPromise <- newPromise
      let newPending = PendingCommit {
        commit = commit,
        commitCompletedPromise
      }
      writeTVar surface.pendingCommit (Just newPending)
      pure (toFuture commitCompletedPromise)

    -- There is a pending commit. Pending commits are not being processed yet,
    -- so the pending commit can be replaced.
    Just pending -> do
      -- Release resources attached to previous SurfaceCommit
      disposerFuture <- disposeEventually pending.commit
      modifyTVar surface.pendingDisposerFuture (<> disposerFuture)

      let newPending = PendingCommit {
        commit = pending.commit <> commit,
        -- Reuse future, so skipped commits will also be signalled once a newer
        -- commit is completed.
        commitCompletedPromise = pending.commitCompletedPromise
      }
      writeTVar surface.pendingCommit (Just newPending)
      pure (toFuture pending.commitCompletedPromise)

clientSurfaceCommitWorker :: forall b. ClientBufferBackend b => ClientSurface b -> IO ()
clientSurfaceCommitWorker surface = forever do
  pending <- atomically do
    readTVar surface.pendingCommit >>= \case
      Nothing -> retry
      Just pending -> do
        pending <$ writeTVar surface.pendingCommit Nothing
  commitPendingCommit surface pending

commitPendingCommit :: forall b. ClientBufferBackend b => ClientSurface b -> PendingCommit b -> IO ()
commitPendingCommit surface pendingCommit = do
  -- Wait for resources from skipped frames to be disposed. This might prevent
  -- a memory leak under pressure.
  await =<< atomically (swapTVar surface.pendingDisposerFuture mempty)

  let commit = pendingCommit.commit
  -- TODO catch exceptions and redirect to client owner (so the shared surface can continue to work when one backend fails)

  -- TODO wl_surface v5 offset changes

  isFrameDestroyed <- atomicallyC $ isJust <$> peekFuture (isDisposed commit.frame)
  when isFrameDestroyed $ throwM (userError "commitPendingCommit was called with a destroyed frame")

  renderedFrame <- renderFrame @b commit.frame

  rawRenderedFrame <- tryReadRcIO renderedFrame >>= \case
    Nothing -> throwM (userError "ClientBuffer.commitPendingCommit renderFrame produced a disposed result")
    Just rawRenderedFrame -> pure rawRenderedFrame

  bufferId <- atomicallyC $ getExportBufferId @b rawRenderedFrame

  let (x, y) = fromMaybe (0, 0) commit.offset


  wlBuffer <- requestWlBuffer surface.surfaceManager bufferId renderedFrame

  syncExportBuffer @b rawRenderedFrame

  -- Simpler alternative to `requestWlBuffer` that never resuses buffers
  -- (useful when debugging):
  --wlBuffer <- newSingleUseWlBuffer surface.surfaceManager buffer


  atomicallyC do
    surface.wlSurface.attach (Just wlBuffer) x y

    mapM_ (addBufferDamage surface.wlSurface) commit.bufferDamage

    forM_ commit.frameCallback \frameCallback -> do
      wlCallback <- surface.wlSurface.frame
      wlCallback `setEventHandler` EventHandler_wl_callback {
        done = \serial -> liftSTMc $ frameCallback serial
      }

    surface.wlSurface.commit

    fulfillPromise pendingCommit.commitCompletedPromise ()

addBufferDamage :: Object 'Client Interface_wl_surface -> Damage -> STMc NoRetry '[SomeException] ()
addBufferDamage wlSurface DamageAll = wlSurface.damage_buffer (minBound `div` 2) (minBound `div` 2) maxBound maxBound
addBufferDamage wlSurface (DamageList xs) = mapM_ (appRect wlSurface.damage_buffer) xs
