module Quasar.Wayland.Client.Surface (
  -- * Buffer backend
  ClientBufferBackend(..),

  -- * Surface
  ClientSurfaceManager,
  newClientSurfaceManager,

  newClientSurface,
  newMirroredClientSurface,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Surface
import Quasar.Wayland.Region (appRect)


class BufferBackend b => ClientBufferBackend b where
  type ClientBufferManager b
  exportWlBuffer :: ClientBufferManager b -> Buffer b -> STM (Object 'Client Interface_wl_buffer)

getClientWlBuffer :: ClientBufferBackend b => ClientSurfaceManager b -> Buffer b -> STM (Object 'Client Interface_wl_buffer)
getClientWlBuffer surfaceManager buffer = do
  buffers <- readTVar bufferMap
  case HM.lookup buffer buffers of
    Just wlBuffer -> pure wlBuffer
    Nothing -> do
      wlBuffer <- exportWlBuffer surfaceManager.bufferManager buffer
      -- TODO register finalizer (on `buffer` and `wlBuffer`) to remove from map
      writeTVar bufferMap (HM.insert buffer wlBuffer buffers)
      pure wlBuffer
  where
    bufferMap = surfaceManager.bufferMap


data ClientSurfaceManager b = ClientSurfaceManager {
  bufferManager :: ClientBufferManager b,
  wlCompositor :: Object 'Client Interface_wl_compositor,
  bufferMap :: TVar (HashMap (Buffer b) (Object 'Client Interface_wl_buffer))
}

data ClientSurface b = ClientSurface {
  surfaceManager :: ClientSurfaceManager b,
  wlSurface :: Object 'Client Interface_wl_surface
}

newClientSurfaceManager :: ClientBufferManager b -> Object 'Client Interface_wl_compositor -> STM (ClientSurfaceManager b)
newClientSurfaceManager bufferManager wlCompositor = do
  bufferMap <- newTVar mempty
  pure ClientSurfaceManager { bufferManager, wlCompositor, bufferMap }

newClientSurface :: ClientBufferBackend b => ClientSurfaceManager b -> STM (Surface b)
newClientSurface surfaceManager = do
  surface <- newSurface
  -- TODO this is quite useless if the wl_surface is dropped, since it can'tbe
  _ <- newMirroredClientSurface surfaceManager surface
  pure surface

-- | Creates a wl_surface object that mirrors the content of a `Surface`.
newMirroredClientSurface :: ClientBufferBackend b => ClientSurfaceManager b -> Surface b -> STM (Object 'Client Interface_wl_surface)
newMirroredClientSurface surfaceManager surface = do
  wlSurface <- surfaceManager.wlCompositor.create_surface
  let clientSurface = ClientSurface { surfaceManager, wlSurface }
  connectSurfaceDownstream surface (surfaceDownstream clientSurface)
  -- TODO: add finalizer, so that the surface is destroyed with the wlSurface
  pure wlSurface

surfaceDownstream :: ClientBufferBackend b => ClientSurface b -> SurfaceDownstream b
surfaceDownstream surface = onSurfaceCommit surface

onSurfaceCommit :: ClientBufferBackend b => ClientSurface b -> SurfaceCommit b -> STM ()
onSurfaceCommit surface commit = do
  -- TODO catch exceptions and redirect to client owner (so the shared surface can continue to work when one backend fails)
  wlBuffer <- mapM (getClientWlBuffer surface.surfaceManager) commit.buffer
  wlSurface.attach wlBuffer (fst commit.offset) (snd commit.offset)
  addBufferDamage surface.wlSurface commit.bufferDamage
  wlSurface.commit
  where
    wlSurface = surface.wlSurface

addBufferDamage :: Object 'Client Interface_wl_surface -> Damage -> STM ()
addBufferDamage wlSurface DamageAll = wlSurface.damage_buffer minBound minBound maxBound maxBound
addBufferDamage wlSurface (DamageList xs) = mapM_ (appRect wlSurface.damage_buffer) xs
