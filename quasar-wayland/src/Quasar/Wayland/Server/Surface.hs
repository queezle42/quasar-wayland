module Quasar.Wayland.Server.Surface (
  ServerSurface,
  initializeServerSurface,
  initializeServerSubsurface,
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
import Quasar.Wayland.Shared.Surface


data ServerSurface b = ServerSurface {
  state :: TVar (ServerSurfaceState b),
  lastRole :: TVar (Maybe String),
  pendingFrameCallback :: TVar (Maybe (Word32 -> STMc NoRetry '[] ()))
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
  pendingOffset :: TVar (Maybe (Int32, Int32)),
  pendingBufferDamage :: TVar (Maybe Damage),
  -- Damage specified in surface coordinates (i.e. produced by wl_surface.damage instead of wl_surface.damage_buffer).
  -- Damage can be converted to buffer coordinates on commit (NOTE: conversion requires wl_surface version 4)
  pendingSurfaceDamage :: TVar [Rectangle]
}

data ServerBuffer b = ServerBuffer {
  buffer :: Buffer b,
  wlBuffer :: Object 'Server Interface_wl_buffer
}

newServerSurface :: STMc NoRetry '[] (ServerSurface b)
newServerSurface = do
  state <- newTVar Unmapped
  lastRole <- newTVar Nothing
  pendingFrameCallback <- newTVar Nothing

  pure ServerSurface {
    state,
    lastRole,
    pendingFrameCallback
  }

getServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STMc NoRetry '[SomeException] (ServerSurface b)
getServerSurface wlSurface =
  getInterfaceData @(ServerSurface b) wlSurface >>= \case
    Nothing -> throwM (userError "Invalid server surface")
    Just serverSurface -> pure serverSurface

commitServerSurface :: ServerSurface b -> STMc NoRetry '[SomeException] ()
commitServerSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    Unmapped -> throwM $ userError "Cannot commit a surface that does not have a role"
    Pending pending -> do
      mappedSurface <- liftSTMc $ mapServerSurface pending
      writeTVar serverSurface.state (Mapped mappedSurface)
    Mapped mappedSurface -> liftSTMc $ commitMappedServerSurface serverSurface mappedSurface

mapServerSurface :: PendingServerSurface b -> STMc NoRetry '[] (MappedServerSurface b)
mapServerSurface pending = do
  pendingBuffer <- newTVar Nothing
  pendingOffset <- newTVar Nothing
  pendingBufferDamage <- newTVar Nothing
  pendingSurfaceDamage <- newTVar mempty
  pure MappedServerSurface {
    surfaceDownstream = pending.surfaceDownstream,
    pendingBuffer,
    pendingOffset,
    pendingBufferDamage,
    pendingSurfaceDamage
  }

commitMappedServerSurface :: ServerSurface b -> MappedServerSurface b -> STMc NoRetry '[SomeException] ()
commitMappedServerSurface surface mapped = do
  serverBuffer <- swapTVar mapped.pendingBuffer Nothing
  offset <- swapTVar mapped.pendingOffset Nothing
  bufferDamage <- swapTVar mapped.pendingBufferDamage Nothing
  surfaceDamage <- swapTVar mapped.pendingSurfaceDamage mempty
  frameCallback <- swapTVar surface.pendingFrameCallback Nothing
  let
    convertedSurfaceDamage =
      case surfaceDamage of
        [] -> mempty
        -- TODO should do a coordinate conversion
        _ -> Just DamageAll
    combinedDamage = bufferDamage <> convertedSurfaceDamage

  case serverBuffer of
    Nothing -> do
      when (isJust frameCallback) $ throwM $ userError "Must not attach frame callback when unmapping surface"
      unmapSurfaceDownstream mapped.surfaceDownstream
    Just sb -> do
      -- Attach callback for wl_buffer.release
      liftSTMc $ addBufferReleaseCallback sb.buffer (tryCall sb.wlBuffer.release)

      liftSTMc $ commitSurfaceDownstream mapped.surfaceDownstream SurfaceCommit {
        buffer = sb.buffer,
        offset,
        bufferDamage = combinedDamage,
        frameCallback
      }


requireMappedSurface :: ServerSurface b -> STMc NoRetry '[SomeException] (MappedServerSurface b)
requireMappedSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    Mapped mapped -> pure mapped
    -- TODO improve exception / propagate error to the client
    _ -> throwM $ userError "Requested operation requires a mapped surface"

attachToSurface ::
  forall b. BufferBackend b =>
  ServerSurface b ->
  Maybe (Object 'Server Interface_wl_buffer) ->
  Int32 ->
  Int32 ->
  STMc NoRetry '[SomeException] ()
attachToSurface serverSurface wlBuffer x y = do
  mappedSurface <- requireMappedSurface serverSurface
  buffer <- mapM (getServerBuffer @b) wlBuffer
  writeTVar mappedSurface.pendingBuffer buffer
  -- TODO ensure (x == 0 && y == 0) for wl_surface v5
  writeTVar mappedSurface.pendingOffset (Just (x, y))

damageSurface :: ServerSurface b -> Rectangle -> STMc NoRetry '[SomeException] ()
damageSurface serverSurface rect = do
  mappedSurface <- requireMappedSurface serverSurface
  modifyTVar mappedSurface.pendingSurfaceDamage (rect:)

damageBuffer :: ServerSurface b -> Rectangle -> STMc NoRetry '[SomeException] ()
damageBuffer serverSurface rect = do
  mappedSurface <- requireMappedSurface serverSurface
  modifyTVar mappedSurface.pendingBufferDamage (addDamage rect)


initializeServerSurface :: forall b. BufferBackend b => NewObject 'Server Interface_wl_surface -> STMc NoRetry '[] ()
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
    commit = liftSTMc $ commitServerSurface serverSurface,
    set_buffer_transform = \transform -> pure (),
    set_buffer_scale = \scale -> pure (),
    damage_buffer = appAsRect (damageBuffer serverSurface)
  }
  setInterfaceData wlSurface serverSurface

addFrameCallback :: ServerSurface b -> Object 'Server Interface_wl_callback -> STMc NoRetry '[SomeException] ()
addFrameCallback serverSurface wlCallback = do
  modifyTVar serverSurface.pendingFrameCallback \case
    Nothing -> Just cb
    Just oldCb -> Just (\time -> oldCb time >> cb time)

  where
    cb :: Word32 -> STMc NoRetry '[] ()
    -- NOTE The spec asks for a timestamp in milliseconds, but is specified as
    -- a uint, which would lead to a rollover after ~50 days.
    -- TODO improve error handling
    cb time = unlessM wlCallback.isDestroyed (tryCall (wlCallback.done time))

initializeWlBuffer :: forall b. BufferBackend b => NewObject 'Server Interface_wl_buffer -> Buffer b -> STMc NoRetry '[] ()
initializeWlBuffer wlBuffer buffer = do
  let serverBuffer = ServerBuffer {
    buffer,
    wlBuffer
  }
  setInterfaceData wlBuffer serverBuffer
  setRequestHandler wlBuffer RequestHandler_wl_buffer {
    -- TODO propagate buffer destruction
    destroy = liftSTMc $ destroyBuffer buffer
  }


getServerBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STMc NoRetry '[SomeException] (ServerBuffer b)
getServerBuffer wlBuffer = do
  ifd <- getInterfaceData @(ServerBuffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)

getBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STMc NoRetry '[SomeException] (Buffer b)
getBuffer wlBuffer = (.buffer) <$> getServerBuffer wlBuffer


assignSurfaceRole :: forall i b. IsInterfaceSide 'Server i => ServerSurface b -> SurfaceDownstream b -> STMc NoRetry '[SomeException] ()
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

removeSurfaceRole :: ServerSurface b -> STMc NoRetry '[] ()
removeSurfaceRole surface = undefined



data ServerSubsurface b = ServerSubsurface (ServerSurface b)

instance IsSurfaceDownstream b (ServerSubsurface b) where
  commitSurfaceDownstream _self _commit = traceM "Subsurface committed"
  unmapSurfaceDownstream _self = traceM "Subsurface unmapped"

initializeServerSubsurface ::
  forall b. BufferBackend b =>
  NewObject 'Server Interface_wl_subsurface ->
  Object 'Server Interface_wl_surface ->
  Object 'Server Interface_wl_surface ->
  STMc NoRetry '[SomeException] ()
initializeServerSubsurface wlSubsurface wlSurface wlParent = do
  serverSurface <- liftSTMc $ getServerSurface @b wlSurface
  serverParent <- liftSTMc $ getServerSurface @b wlParent
  assignSurfaceRole @Interface_wl_subsurface serverSurface (toSurfaceDownstream (ServerSubsurface serverParent))
  setRequestHandler wlSubsurface RequestHandler_wl_subsurface {
    destroy = pure (),
    set_position = \x y -> traceM (mconcat ["TODO: Subsurface position: ", show x, ", ", show y]),
    place_above = \sibling -> traceM "TODO: Subsurface above",
    place_below = \sibling -> traceM "TODO: Subsurface below",
    set_sync = traceM "TODO: Subsurface sync",
    set_desync = traceM "TODO: Subsurface desync"
  }
