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
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Future (Future, Promise, ToFuture (toFuture), newPromise, readFuture, peekFuture, fulfillPromise)
import Quasar.Prelude
import Quasar.Resources (TDisposer, disposeSTM, isDisposed, getTDisposer, disposeTDisposer)
import Quasar.Resources.Lock
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appRect)
import Quasar.Wayland.Shared.Surface


class (RenderBackend b, Typeable (ClientBufferManager b), Hashable (ExportBuffer b)) => ClientBufferBackend b where
  type ClientBufferManager b
  type ExportBuffer b
  newClientBufferManager :: WaylandClient -> STMc NoRetry '[SomeException] (ClientBufferManager b)

  -- | Render a frame into a buffer.
  --
  -- Takes responsibility for the frame lock. The caller is responsible for the
  -- returned buffer lock.
  --
  -- The exported buffers contents should not change until it is unlocked by the
  -- caller.
  renderFrame :: Lock (Frame b) -> IO (Lock (ExportBuffer b))

  -- | Called by the `Surface`-implementation when a buffer should be created on a wayland client.
  -- The caller takes ownership of the resulting wl_buffer and will attach the event handler.
  exportWlBuffer :: ClientBufferManager b -> ExportBuffer b -> IO (NewObject 'Client Interface_wl_buffer)

  addBufferDestroyedCallback :: ExportBuffer b -> STMc NoRetry '[] () -> STMc NoRetry '[] ()

getClientBufferManager :: forall b. ClientBufferBackend b => WaylandClient -> STMc NoRetry '[SomeException] (ClientBufferManager b)
getClientBufferManager client =
  getClientComponent (newClientBufferManager @b client) client


-- | Requests a wl_buffer for a locked buffer and changes it's state to attached.
--
-- Takes ownership of the buffer lock.
requestWlBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> Lock (ExportBuffer b) -> IO (Object 'Client Interface_wl_buffer)
requestWlBuffer client buffer = do
  atomically (tryReadLock buffer) >>= \case
    Nothing -> throwM (userError "ClientBuffer.requestWlBuffer renderedImage resulted in a disposed buffer")
    Just rawBuffer -> do
      join $ atomicallyC do
        buffers <- readTVar client.bufferMap
        case HM.lookup rawBuffer buffers of
          -- Buffer is already exported to client
          Just clientBuffer -> do
            readTVar clientBuffer.state >>= \case
              Released -> pure <$> attachClientBuffer clientBuffer
              -- TODO using (z)wp_linux_buffer_release a buffer could be attached to multiple surfaces
              Attached _ -> pure $ newSingleUseWlBuffer client buffer
              -- This should not happen - disposed ClientBuffers are removed from the buffer map
              Destroyed -> throwM (userError "ClientBuffer.requestClientBuffer called on a destroyed ClientBuffer")
          -- Buffer is currently not exported to client
          Nothing -> pure do
            clientBuffer <- newClientBuffer client rawBuffer
            atomicallyC $ attachClientBuffer clientBuffer
  where
    attachClientBuffer :: ClientBuffer b -> STMc NoRetry '[SomeException] (Object 'Client Interface_wl_buffer)
    attachClientBuffer clientBuffer = do
      -- Unlock buffer on wl_buffer release
      writeTVar clientBuffer.state (Attached (getTDisposer buffer))
      pure clientBuffer.wlBuffer


-- | Create a reusable client buffer in a released state.
newClientBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> ExportBuffer b -> IO (ClientBuffer b)
newClientBuffer client rawBuffer = do
  wlBuffer <- exportWlBuffer @b client.bufferManager rawBuffer

  atomicallyC do
    state <- newTVar Released
    let clientBuffer = ClientBuffer {
      wlBuffer,
      state
    }
    attachFinalizer wlBuffer do
      modifyTVar client.bufferMap (HM.delete rawBuffer)
      readTVar clientBuffer.state >>= \case
        Attached lockDisposer -> do
          writeTVar clientBuffer.state Destroyed
          disposeTDisposer lockDisposer
        Released ->
          writeTVar clientBuffer.state Destroyed
        Destroyed -> pure ()

    setEventHandler wlBuffer EventHandler_wl_buffer {
      release = releaseClientBuffer clientBuffer
    }
    liftSTMc $ addBufferDestroyedCallback @b rawBuffer (destroyClientBuffer clientBuffer)
    modifyTVar client.bufferMap (HM.insert rawBuffer clientBuffer)
    pure clientBuffer

releaseClientBuffer :: ClientBuffer b -> STMc NoRetry '[SomeException] ()
releaseClientBuffer clientBuffer = do
  readTVar clientBuffer.state >>= \case
    Attached lockDisposer -> do
      traceM "ClientBuffer: releasing"
      writeTVar clientBuffer.state Released
      disposeTDisposer lockDisposer
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
newSingleUseWlBuffer :: forall b. ClientBufferBackend b => ClientSurfaceManager b -> Lock (ExportBuffer b) -> IO (Object 'Client Interface_wl_buffer)
newSingleUseWlBuffer surfaceManager buffer = do
  atomically (tryReadLock buffer) >>= \case
    Nothing -> throwM (userError "ClientBuffer.newSingleUseWlBuffer called with a disposed buffer")
    Just rawBuffer -> do
      wlBuffer <- exportWlBuffer @b surfaceManager.bufferManager rawBuffer

      atomicallyC do
        -- Attach ownership/cleanup of buffer lock to wl_buffer
        attachFinalizer wlBuffer (disposeSTM buffer)
        setEventHandler wlBuffer EventHandler_wl_buffer {
          release = wlBuffer.destroy
        }
        pure wlBuffer


data ClientSurfaceManager b = ClientSurfaceManager {
  bufferManager :: ClientBufferManager b,
  wlCompositor :: Object 'Client Interface_wl_compositor,
  bufferMap :: TVar (HashMap (ExportBuffer b) (ClientBuffer b))
}

data ClientSurface b = ClientSurface {
  surfaceManager :: ClientSurfaceManager b,
  wlSurface :: Object 'Client Interface_wl_surface,
  pendingCommit :: TVar (Maybe (PendingCommit b))
}

data PendingCommit b = PendingCommit {
  commit :: SurfaceCommit b,
  commitCompletedPromise :: Promise ()
}

data ClientBufferState = Attached TDisposer | Released | Destroyed

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

  let clientSurface = ClientSurface { surfaceManager, wlSurface, pendingCommit }

  -- Spawn worker thread. Worker thread will be stopped when the ClientSurface
  -- is garbage collected.
  forkSTM_ (clientSurfaceCommitWorker clientSurface) loggingExceptionSink

  pure (clientSurface, fnResult)

instance ClientBufferBackend b => IsSurfaceDownstream b (ClientSurface b) where
  commitSurfaceDownstream = commitClientSurface
  unmapSurfaceDownstream = undefined

commitClientSurface :: ClientBufferBackend b => ClientSurface b -> SurfaceCommit b -> STMc NoRetry '[SomeException] (Future ())
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
      liftSTMc $ destroyCommit pending.commit

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
      Just pending -> pending <$ writeTVar surface.pendingCommit Nothing
  commitPendingCommit surface pending

commitPendingCommit :: forall b. ClientBufferBackend b => ClientSurface b -> PendingCommit b -> IO ()
commitPendingCommit surface pendingCommit = do
  let commit = pendingCommit.commit
  -- TODO catch exceptions and redirect to client owner (so the shared surface can continue to work when one backend fails)

  -- TODO wl_surface v5 offset changes

  isFrameDestroyed <- atomicallyC $ isJust <$> peekFuture (isDisposed commit.frame)
  when isFrameDestroyed $ throwM (userError "commitPendingCommit was called with a destroyed frame")

  buffer <- renderFrame @b commit.frame

  let (x, y) = fromMaybe (0, 0) commit.offset


  wlBuffer <- requestWlBuffer surface.surfaceManager buffer

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
