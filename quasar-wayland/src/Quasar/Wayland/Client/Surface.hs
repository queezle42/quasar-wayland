module Quasar.Wayland.Client.Surface (
  -- * Surface
  ClientSurfaceManager,
  getClientSurfaceManager,

  ClientSurface,
  newClientSurface,
  commitClientSurface,

  -- * Backend
  getBackendClientBufferManager,

  -- * Reexport
  ClientBufferBackend(..),
) where

import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Quasar.Async.Fork (forkSTM_)
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Backend
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appRect)
import Quasar.Wayland.Shared.Surface


-- | Requests a wl_buffer for a locked buffer and changes it's state to attached.
--
-- Takes ownership of the frame rc.
requestWlBuffer ::
  forall b. ClientBufferBackend b =>
  ClientSurfaceManager b ->
  Owned (RenderedFrame b) ->
  IO (Object 'Client Interface_wl_buffer)
requestWlBuffer client frame = do
  join $ atomically do
    bufferId <- liftSTMc $ getExportBufferId @b (fromOwned frame)
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
newClientBuffer ::
  forall b. ClientBufferBackend b =>
  ClientSurfaceManager b ->
  ExportBufferId b ->
  Owned (RenderedFrame b) ->
  IO (ClientBuffer b)
newClientBuffer client bufferId (Owned disposer frame) = do
  wlBuffer <- exportWlBuffer @b client.backend frame

  state <- newTVarIO (Attached disposer)
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
    exportBufferDestroyedFuture <- getExportBufferDestroyedFuture @b frame
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
newSingleUseWlBuffer ::
  forall b. ClientBufferBackend b =>
  ClientSurfaceManager b ->
  Owned (RenderedFrame b) ->
  IO (Object 'Client Interface_wl_buffer)
newSingleUseWlBuffer client (Owned disposer frame) = do
  wlBuffer <- exportWlBuffer @b client.backend frame

  atomicallyC do
    -- Attach ownership/cleanup of buffer lock to wl_buffer
    -- TODO do we need to handle the disposer future?
    attachFinalizer wlBuffer (disposeEventually_ disposer)
    setEventHandler wlBuffer EventHandler_wl_buffer {
      release = wlBuffer.destroy
    }
    pure wlBuffer


data ClientSurfaceManager b = ClientSurfaceManager {
  backend :: BackendClientBufferManager b,
  bufferMap :: TVar (HashMap (ExportBufferId b) (ClientBuffer b)),
  wlCompositor :: Object 'Client Interface_wl_compositor
}

data ClientSurface b = ClientSurface {
  surfaceManager :: ClientSurfaceManager b,
  wlSurface :: Object 'Client Interface_wl_surface,
  pendingCommit :: TVar (Maybe (PendingCommit b)),
  pendingDisposerFuture :: TVar (Future '[] ())
}

data PendingCommit b = PendingCommit {
  commit :: Owned (SurfaceCommit b),
  commitCompletedPromise :: Promise ()
}

data ClientBufferState = Attached Disposer | Released | Destroyed

data ClientBuffer b = ClientBuffer {
  wlBuffer :: Object 'Client Interface_wl_buffer,
  state :: TVar ClientBufferState
}

getBackendClientBufferManager :: forall b. ClientBufferBackend b => WaylandClient b -> STMc NoRetry '[SomeException] (BackendClientBufferManager b)
getBackendClientBufferManager client = do
  clientSurfaceManager <- getClientSurfaceManager client
  pure clientSurfaceManager.backend

getClientSurfaceManager ::
  ClientBufferBackend b =>
  WaylandClient b -> STMc NoRetry '[SomeException] (ClientSurfaceManager b)
getClientSurfaceManager client =
  getClientComponent (newClientSurfaceManager client) client

newClientSurfaceManager ::
  forall b. ClientBufferBackend b =>
  WaylandClient b -> STMc NoRetry '[SomeException] (ClientSurfaceManager b)
newClientSurfaceManager client = do
  backend <- newBackendClientBufferManager @b client
  bufferMap <- newTVar mempty
  wlCompositor <- getClientComponent @(Object 'Client Interface_wl_compositor) (liftSTMc (newWlCompositor client)) client
  pure ClientSurfaceManager {
    backend,
    bufferMap,
    wlCompositor
  }

newWlCompositor ::
  WaylandClient b ->
  STMc NoRetry '[SomeException] (Object 'Client Interface_wl_compositor)
newWlCompositor client = do
  wlCompositor <- bindSingleton client.registry maxVersion
  -- wl_compositor does not have any events. Setting `()` as the event handler will produce a type error if that changes in the future.
  setEventHandler wlCompositor ()
  pure wlCompositor


newClientSurface ::
  ClientBufferBackend b =>
  WaylandClient b ->
  (Object 'Client Interface_wl_surface -> STMc NoRetry '[SomeException] a) ->
  STMc NoRetry '[SomeException] (ClientSurface b, a)
newClientSurface client initializeSurfaceRoleFn = do
  surfaceManager <- getClientSurfaceManager client

  wlSurface <- liftSTMc surfaceManager.wlCompositor.create_surface

  -- TODO: add finalizer, so that the surface is destroyed with the wlSurface
  -- TODO event handling
  setEventHandler wlSurface EventHandler_wl_surface {
    enter = \_ -> pure (),
    leave = \_ -> pure (),
    preferred_buffer_scale = \_ -> pure (),
    preferred_buffer_transform = \_ -> pure ()
  }
  fnResult <- initializeSurfaceRoleFn wlSurface

  -- Commit role
  liftSTMc wlSurface.commit

  pendingCommit <- newTVar Nothing
  pendingDisposerFuture <- newTVar mempty

  let clientSurface = ClientSurface {
    surfaceManager,
    wlSurface,
    pendingCommit,
    pendingDisposerFuture
  }

  -- Spawn worker thread. Worker thread will be stopped when the ClientSurface
  -- is garbage collected.
  forkSTM_ (clientSurfaceCommitWorker clientSurface) loggingExceptionSink

  pure (clientSurface, fnResult)

commitClientSurface ::
  ClientSurface b -> Owned (SurfaceCommit b) -> STMc NoRetry '[SomeException] (Future '[] ())
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
      let Owned pendingDisposer pendingCommit = pending.commit

      -- Release resources attached to previous SurfaceCommit
      disposerFuture <- disposeEventually pendingDisposer
      modifyTVar surface.pendingDisposerFuture (<> disposerFuture)

      -- Reuse promise, so skipped commits will also be signalled once a newer
      -- commit is completed.
      let commitCompletedPromise = pending.commitCompletedPromise
      let newPending = PendingCommit {
        commit = mergeCommits pendingCommit commit,
        commitCompletedPromise
      }
      writeTVar surface.pendingCommit (Just newPending)
      pure (toFuture commitCompletedPromise)

clientSurfaceCommitWorker :: ClientBufferBackend b => ClientSurface b -> IO ()
clientSurfaceCommitWorker surface = forever do
  pending <- atomically do
    readTVar surface.pendingCommit >>= \case
      Nothing -> retry
      Just pending -> do
        pending <$ writeTVar surface.pendingCommit Nothing
  commitPendingCommit surface pending

commitPendingCommit :: forall b. ClientBufferBackend b => ClientSurface b -> PendingCommit b -> IO ()
commitPendingCommit surface pendingCommit = do
  -- Wait for resources from skipped frames to be disposed. This is a safeguard
  -- to prevent a memory leak under pressure.
  await =<< atomically (swapTVar surface.pendingDisposerFuture mempty)

  let (Owned disposer commit) = pendingCommit.commit
  -- TODO catch exceptions and redirect to client owner (so the shared surface can continue to work when one backend fails)

  -- TODO wl_surface v5 offset changes

  isFrameDestroyed <- atomicallyC $ isJust <$> peekFuture (toFuture commit.frame)
  when isFrameDestroyed $ throwM (userError "commitPendingCommit was called with a destroyed frame")

  renderedFrame <- renderFrame @b (Owned disposer commit.frame)

  ownedRenderedFrame <- atomically $ extractRc renderedFrame

  let (x, y) = fromMaybe (0, 0) commit.offset


  wlBuffer <- requestWlBuffer @b surface.surfaceManager ownedRenderedFrame

  syncExportBuffer @b (fromOwned ownedRenderedFrame)

  -- Simpler alternative to `requestWlBuffer` that never resuses buffers
  -- (useful when debugging):
  --wlBuffer <- newSingleUseWlBuffer bufferManager buffer


  atomicallyC do
    surface.wlSurface.attach (Just wlBuffer) x y

    addBufferDamage surface.wlSurface DamageAll

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
