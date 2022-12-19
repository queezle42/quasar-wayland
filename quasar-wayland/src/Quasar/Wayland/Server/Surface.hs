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
  state :: TVar (ServerSurfaceState b),
  lastRole :: TVar (Maybe String),
  pendingFrameCallback :: TVar (Maybe (Word32 -> STM ()))
}

data ServerSurfaceState b =
  Unmapped |
  -- | Surface role was assigned
  Pending (PendingServerSurface b) |
  -- | Surface role has been commited
  Mapped (MappedServerSurface b)

newtype PendingServerSurface b = PendingServerSurface {
  surfaceDownstream :: SurfaceDownstream b
}

data MappedServerSurface b = MappedServerSurface {
  surfaceDownstream :: SurfaceDownstream b,
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

newServerSurface :: STM (ServerSurface b)
newServerSurface = do
  state <- newTVar Unmapped
  lastRole <- newTVar Nothing
  pendingFrameCallback <- newTVar Nothing

  pure ServerSurface {
    state,
    lastRole,
    pendingFrameCallback
  }

getServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STM (Maybe (ServerSurface b))
getServerSurface wlSurface = getInterfaceData @(ServerSurface b) wlSurface

commitServerSurface :: ServerSurface b -> STM ()
commitServerSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    Unmapped -> throwM $ userError "Cannot commit a surface that does not have a role"
    Pending pending -> do
      mappedSurface <- mapServerSurface pending
      writeTVar serverSurface.state (Mapped mappedSurface)
    Mapped mappedSurface -> commitMappedServerSurface serverSurface mappedSurface

mapServerSurface :: PendingServerSurface b -> STM (MappedServerSurface b)
mapServerSurface pending = do
  pendingBuffer <- newTVar Nothing
  pendingOffset <- newTVar (0, 0)
  pendingBufferDamage <- newTVar mempty
  pendingSurfaceDamage <- newTVar mempty
  pure MappedServerSurface {
    surfaceDownstream = pending.surfaceDownstream,
    pendingBuffer,
    pendingOffset,
    pendingBufferDamage,
    pendingSurfaceDamage
  }

commitMappedServerSurface :: ServerSurface b -> MappedServerSurface b -> STM ()
commitMappedServerSurface surface mapped = do
  serverBuffer <- swapTVar mapped.pendingBuffer Nothing
  offset <- swapTVar mapped.pendingOffset (0, 0)
  bufferDamage <- swapTVar mapped.pendingBufferDamage mempty
  surfaceDamage <- swapTVar mapped.pendingSurfaceDamage mempty
  frameCallback <- swapTVar surface.pendingFrameCallback Nothing
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

  commitSurfaceDownstream mapped.surfaceDownstream SurfaceCommit {
    buffer = (.buffer) <$> serverBuffer,
    offset,
    bufferDamage = combinedDamage,
    frameCallback
  }


requireMappedSurface :: ServerSurface b -> STM (MappedServerSurface b)
requireMappedSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    Mapped mapped -> pure mapped
    -- TODO improve exception / propagate error to the client
    _ -> throwM $ userError "Requested operation requires a mapped surface"

attachToSurface :: forall b. BufferBackend b => ServerSurface b -> Maybe (Object 'Server Interface_wl_buffer) -> Int32 -> Int32 -> STM ()
attachToSurface serverSurface wlBuffer x y = do
  mappedSurface <- requireMappedSurface serverSurface
  buffer <- mapM (getServerBuffer @b) wlBuffer
  writeTVar mappedSurface.pendingBuffer buffer
  writeTVar mappedSurface.pendingOffset (x, y)

damageSurface :: ServerSurface b -> Rectangle -> STM ()
damageSurface serverSurface rect = do
  mappedSurface <- requireMappedSurface serverSurface
  modifyTVar mappedSurface.pendingSurfaceDamage (rect:)

damageBuffer :: ServerSurface b -> Rectangle -> STM ()
damageBuffer serverSurface rect = do
  mappedSurface <- requireMappedSurface serverSurface
  modifyTVar mappedSurface.pendingBufferDamage \case
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
    frame = addFrameCallback serverSurface,
    set_opaque_region = \region -> pure (),
    set_input_region = \region -> pure (),
    commit = commitServerSurface serverSurface,
    set_buffer_transform = \transform -> pure (),
    set_buffer_scale = \scale -> pure (),
    damage_buffer = appAsRect (damageBuffer serverSurface)
  }
  setInterfaceData wlSurface serverSurface

addFrameCallback :: ServerSurface b -> Object 'Server Interface_wl_callback -> STM ()
addFrameCallback serverSurface wlCallback = do
  modifyTVar serverSurface.pendingFrameCallback \case
    Nothing -> Just cb
    Just oldCb -> Just (\time -> oldCb time >> cb time)

  where
    cb :: Word32 -> STM ()
    -- NOTE The spec asks for a timestamp in milliseconds, but is specified as
    -- a uint, which would lead to a rollover after ~50 days.
    cb time = unlessM wlCallback.isDestroyed (wlCallback.done time)

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


assignSurfaceRole :: forall i b. IsInterfaceSide 'Server i => ServerSurface b -> SurfaceDownstream b -> STM ()
assignSurfaceRole surface surfaceDownstream = do
  let role = interfaceName @i

  readTVar surface.state >>= \case
    Mapped _ -> throwM (ProtocolUsageError "Cannot assign wl_surface a new role, since it already has an active role.")
    Pending _ -> throwM (ProtocolUsageError "Cannot assign wl_surface a new role, since it already has a pending role.")
    Unmapped -> pure ()

  writeTVar surface.state $ Pending PendingServerSurface { surfaceDownstream }

  readTVar surface.lastRole >>= \case
    Just ((== role) -> True) -> pure ()
    Just currentRole ->
      let msg = mconcat ["Cannot change wl_surface role. The last role was ", currentRole, "; new role is ", role]
      in throwM (ProtocolUsageError msg)
    Nothing -> writeTVar surface.lastRole (Just role)

removeSurfaceRole :: ServerSurface b -> STM ()
removeSurfaceRole surface = undefined
