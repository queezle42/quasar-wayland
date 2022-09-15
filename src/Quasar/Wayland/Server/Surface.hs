module Quasar.Wayland.Server.Surface (
  ServerSurface,
  initializeServerSurface,
  getServerSurface,
  assignSurfaceRole,
  removeSurfaceRole,
  initializeWlBuffer,
  getBuffer,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appAsRect)
import Quasar.Wayland.Surface


data ServerSurface b = ServerSurface {
  surface :: Surface b,
  lastRole :: TVar (Maybe String),
  hasActiveRole :: TVar Bool,
  pendingBuffer :: TVar (Maybe (ServerBuffer b)),
  pendingOffset :: TVar (Int32, Int32),
  pendingBufferDamage :: TVar Damage,
  -- Damage specified in surface coordinates (i.e. produced by wl_surface.damage instead of wl_surface.damage_buffer).
  -- Damage can be converted to buffer coordinates on commit (NOTE: conversion requires wl_surface version 4)
  pendingSurfaceDamage :: TVar [Rectangle]
}

data ServerBuffer b = ServerBuffer {
  buffer :: Buffer b,
  wlBuffer :: Object 'Server Interface_wl_buffer
}

newServerSurface :: forall b. STM (ServerSurface b)
newServerSurface = do
  surface <- newSurface @b
  lastRole <- newTVar Nothing
  hasActiveRole <- newTVar False
  pendingBuffer <- newTVar Nothing
  pendingOffset <- newTVar (0, 0)
  pendingBufferDamage <- newTVar mempty
  pendingSurfaceDamage <- newTVar mempty

  pure ServerSurface {
    surface,
    lastRole,
    hasActiveRole,
    pendingBuffer,
    pendingOffset,
    pendingBufferDamage,
    pendingSurfaceDamage
  }

getServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STM (Maybe (ServerSurface b))
getServerSurface wlSurface = getInterfaceData @(ServerSurface b) wlSurface

instance IsSurfaceUpstream b (ServerSurface b) where
  connectSurfaceDownstream serverSurface downstream =
    connectSurfaceDownstream serverSurface.surface downstream

commitServerSurface :: ServerSurface b -> STM ()
commitServerSurface surface = do
  serverBuffer <- swapTVar surface.pendingBuffer Nothing
  offset <- swapTVar surface.pendingOffset (0, 0)
  bufferDamage <- swapTVar surface.pendingBufferDamage mempty
  surfaceDamage <- swapTVar surface.pendingSurfaceDamage mempty
  let
    convertedSurfaceDamage =
      case surfaceDamage of
        [] -> mempty
        -- TODO should do a coordinate conversion
        _ -> DamageAll
    combinedDamage = bufferDamage <> convertedSurfaceDamage

  -- Attach callback for wl_buffer.release
  forM_ serverBuffer \sb ->
    addBufferReleaseCallback sb.buffer sb.wlBuffer.release

  commitSurface surface.surface SurfaceCommit {
    buffer = (.buffer) <$> serverBuffer,
    offset,
    bufferDamage = combinedDamage
  }


attachToSurface :: forall b. BufferBackend b => ServerSurface b -> Maybe (Object 'Server Interface_wl_buffer) -> Int32 -> Int32 -> STM ()
attachToSurface surface wlBuffer x y = do
  buffer <- mapM (getServerBuffer @b) wlBuffer
  writeTVar surface.pendingBuffer buffer
  writeTVar surface.pendingOffset (x, y)

damageSurface :: forall b. ServerSurface b -> Rectangle -> STM ()
damageSurface surface rect = modifyTVar surface.pendingSurfaceDamage (rect:)

damageBuffer :: forall b. ServerSurface b -> Rectangle -> STM ()
damageBuffer surface rect =
  modifyTVar surface.pendingBufferDamage \case
    DamageAll -> DamageAll
    DamageList xs -> DamageList (rect : xs)


initializeServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STM ()
initializeServerSurface wlSurface = do
  serverSurface <- newServerSurface @b
  -- TODO missing requests
  setMessageHandler wlSurface RequestHandler_wl_surface {
    -- TODO ensure role is destroyed before surface
    -- TODO destroy associated surface
    destroy = pure (),
    attach = attachToSurface serverSurface,
    damage = appAsRect (damageSurface serverSurface),
    frame = \callback -> pure (),
    set_opaque_region = \region -> pure (),
    set_input_region = \region -> pure (),
    commit = commitServerSurface serverSurface,
    set_buffer_transform = \transform -> pure (),
    set_buffer_scale = \scale -> pure (),
    damage_buffer = appAsRect (damageBuffer serverSurface)
  }
  setInterfaceData wlSurface serverSurface

initializeWlBuffer :: forall b. BufferBackend b => NewObject 'Server Interface_wl_buffer -> Buffer b -> STM ()
initializeWlBuffer wlBuffer buffer = do
  let serverBuffer = ServerBuffer {
    buffer,
    wlBuffer
  }
  setInterfaceData wlBuffer serverBuffer
  setRequestHandler wlBuffer RequestHandler_wl_buffer {
    -- TODO propagate buffer destruction
    destroy = destroyBuffer buffer
  }


getServerBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STM (ServerBuffer b)
getServerBuffer wlBuffer = do
  ifd <- getInterfaceData @(ServerBuffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)

getBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STM (Buffer b)
getBuffer wlBuffer = (.buffer) <$> getServerBuffer wlBuffer


assignSurfaceRole :: forall i b. IsInterfaceSide 'Server i => ServerSurface b -> STM ()
assignSurfaceRole surface = do
  let role = interfaceName @i

  hasActiveRole <- readTVar surface.hasActiveRole
  if hasActiveRole
    then throwM (ProtocolUsageError "Cannot assign wl_surface a new role, since it already has an active role.")
    else writeTVar surface.hasActiveRole True

  readTVar surface.lastRole >>= \x -> (flip ($)) x \case
    Just ((== role) -> True) -> pure ()
    Just currentRole ->
      let msg = mconcat ["Cannot change wl_surface role. The last role was ", currentRole, "; new role is ", role]
      in throwM (ProtocolUsageError msg)
    Nothing -> writeTVar surface.lastRole (Just role)

removeSurfaceRole :: ServerSurface b -> STM ()
removeSurfaceRole surface = writeTVar surface.hasActiveRole False
