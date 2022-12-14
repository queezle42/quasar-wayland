module Quasar.Wayland.Client.Surface (
  -- * Buffer backend
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
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appRect)
import Quasar.Wayland.Surface


class (BufferBackend b, Typeable (ClientBufferManager b)) => ClientBufferBackend b where
  type ClientBufferManager b
  newClientBufferManager :: WaylandClient -> STM (ClientBufferManager b)
  -- | Called by the `Surface`-implementation when a buffer should be created on a wayland client.
  -- The caller takes ownership of the resulting wl_buffer and will attach the event handler.
  exportWlBuffer :: ClientBufferManager b -> Buffer b -> STM (NewObject 'Client Interface_wl_buffer)

getClientBufferManager :: forall b. ClientBufferBackend b => WaylandClient -> STM (ClientBufferManager b)
getClientBufferManager client =
  getClientComponent (newClientBufferManager @b client) client



-- | Requests a wl_buffer for a `Buffer` and changes it's state to attached.
requestClientBuffer :: ClientBufferBackend b => ClientSurfaceManager b -> Buffer b -> STM (Object 'Client Interface_wl_buffer)
requestClientBuffer client buffer = do
  buffers <- readTVar client.bufferMap
  case HM.lookup buffer buffers of
    Just clientBuffer -> do
      readTVar clientBuffer.state >>= \case
        Released -> useClientBuffer clientBuffer
        -- TODO using (z)wp_linux_buffer_release a buffer could be attached to multiple surfaces
        Attached _ -> newSingleUseClientBuffer client buffer
    Nothing -> do
      clientBuffer <- newClientBuffer client buffer
      writeTVar client.bufferMap (HM.insert buffer clientBuffer buffers)
      useClientBuffer clientBuffer
  where
    useClientBuffer :: ClientBuffer b -> STM (Object 'Client Interface_wl_buffer)
    useClientBuffer clientBuffer = do
      unlockFn <- lockBuffer buffer
      writeTVar clientBuffer.state (Attached unlockFn)
      pure clientBuffer.wlBuffer


-- | Create a reusable client buffer.
newClientBuffer :: ClientBufferBackend b => ClientSurfaceManager b -> Buffer b -> STM (ClientBuffer b)
newClientBuffer client buffer = do
  whenM (isBufferDestroyed buffer) $ throwM (userError "newClientBuffer was called with a destroyed buffer")

  state <- newTVar Released
  wlBuffer <- exportWlBuffer client.bufferManager buffer
  -- TODO register finalizer on wl_buffer to unlock `Buffer` if the client is disconnected before release
  -- TODO in both cases, the `ClientBuffer` should be removed from the `bufferMap`
  destroyed <- newTVar False
  let clientBuffer = ClientBuffer {
    wlBuffer,
    state,
    destroyed
  }
  setEventHandler wlBuffer EventHandler_wl_buffer {
    release = releaseClientBuffer clientBuffer
  }
  addBufferDestroyedCallback buffer (destroyClientBuffer clientBuffer)
  pure clientBuffer

releaseClientBuffer :: ClientBuffer b -> STM ()
releaseClientBuffer clientBuffer = do
  swapTVar clientBuffer.state Released >>= \case
    Attached releaseFn -> releaseFn
    Released -> traceM "ClientBuffer: Duplicate release"

destroyClientBuffer :: ClientBuffer b -> STM ()
destroyClientBuffer clientBuffer = do
  writeTVar clientBuffer.destroyed True
  clientBuffer.wlBuffer.destroy


-- | Since `release` is undefined when a buffer is attached to multiple surfaces,
-- using multiple wl_buffer objects for the same `Buffer` might be required.
--
-- This function creates single-use buffers, that is, buffers that are destroyed
-- as soon as they receive a `release`-event.
newSingleUseClientBuffer :: ClientBufferBackend b => ClientSurfaceManager b -> Buffer b -> STM (Object 'Client Interface_wl_buffer)
newSingleUseClientBuffer surfaceManager buffer = do
  unlockFn <- lockBuffer buffer
  wlBuffer <- exportWlBuffer surfaceManager.bufferManager buffer
  setEventHandler wlBuffer EventHandler_wl_buffer {
    release = wlBuffer.destroy >> unlockFn
  }
  -- TODO register finalizer to release lock when client is disconnected
  pure wlBuffer


data ClientSurfaceManager b = ClientSurfaceManager {
  bufferManager :: ClientBufferManager b,
  wlCompositor :: Object 'Client Interface_wl_compositor,
  bufferMap :: TVar (HashMap (Buffer b) (ClientBuffer b))
}

data ClientSurface b = ClientSurface {
  surfaceManager :: ClientSurfaceManager b,
  wlSurface :: Object 'Client Interface_wl_surface
}

data ClientBufferState = Attached (STM ()) | Released

data ClientBuffer b = ClientBuffer {
  wlBuffer :: Object 'Client Interface_wl_buffer,
  state :: TVar ClientBufferState,
  destroyed :: TVar Bool
}

getClientSurfaceManager :: ClientBufferBackend b => WaylandClient -> STM (ClientSurfaceManager b)
getClientSurfaceManager client =
  getClientComponent (newClientSurfaceManager client) client

newClientSurfaceManager :: forall b. ClientBufferBackend b => WaylandClient -> STM (ClientSurfaceManager b)
newClientSurfaceManager client = do
  bufferManager <- getClientBufferManager @b client
  wlCompositor <- getClientComponent @(Object 'Client Interface_wl_compositor) (newWlCompositor client) client
  bufferMap <- newTVar mempty
  pure ClientSurfaceManager {
    bufferManager,
    wlCompositor,
    bufferMap
  }

newWlCompositor :: WaylandClient -> STM (Object 'Client Interface_wl_compositor)
newWlCompositor client = do
  wlCompositor <- bindSingleton client.registry maxVersion
  -- wl_compositor does not have any events. Setting `()` as the event handler will produce a type error if that changes in the future.
  setEventHandler wlCompositor ()
  pure wlCompositor


newClientSurface :: forall b a. ClientBufferBackend b => WaylandClient -> (Object 'Client Interface_wl_surface -> STM a) -> STM (ClientSurface b, a)
newClientSurface client initializeSurfaceRoleFn = do
  surfaceManager <- getClientSurfaceManager @b client
  wlSurface <- surfaceManager.wlCompositor.create_surface

  -- TODO: add finalizer, so that the surface is destroyed with the wlSurface
  -- TODO event handling
  setEventHandler wlSurface EventHandler_wl_surface {
    enter = \_ -> pure (),
    leave = \_ -> pure ()
  }
  fnResult <- initializeSurfaceRoleFn wlSurface

  -- Commit role
  wlSurface.commit

  let clientSurface = ClientSurface { surfaceManager, wlSurface }
  pure (clientSurface, fnResult)

instance ClientBufferBackend b => IsSurfaceDownstream b (ClientSurface b) where
  commitSurfaceDownstream = commitClientSurface

commitClientSurface :: ClientBufferBackend b => ClientSurface b -> SurfaceCommit b -> STM ()
commitClientSurface surface commit = do
  -- TODO catch exceptions and redirect to client owner (so the shared surface can continue to work when one backend fails)

  -- TODO wl_surface v5 offset changes

  forM_ commit.buffer \buffer -> do
    let offset = fromMaybe (0, 0) commit.offset

    wlBuffer <- requestClientBuffer surface.surfaceManager buffer
    -- NOTE Alternative which does not leak buffer objects (until TODOs are fixed) by never reusing buffers
    --wlBuffer <- newSingleUseClientBuffer surface.surfaceManager buffer
    surface.wlSurface.attach (Just wlBuffer) (fst offset) (snd offset)

  mapM_ (addBufferDamage surface.wlSurface) commit.bufferDamage

  forM_ commit.frameCallback \frameCallback -> do
    wlCallback <- surface.wlSurface.frame
    wlCallback `setEventHandler` EventHandler_wl_callback {
      done = frameCallback
    }

  surface.wlSurface.commit

addBufferDamage :: Object 'Client Interface_wl_surface -> Damage -> STM ()
addBufferDamage wlSurface DamageAll = wlSurface.damage_buffer (minBound `div` 2) (minBound `div` 2) maxBound maxBound
addBufferDamage wlSurface (DamageList xs) = mapM_ (appRect wlSurface.damage_buffer) xs
