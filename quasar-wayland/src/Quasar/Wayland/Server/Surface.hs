module Quasar.Wayland.Server.Surface (
  -- * wl_buffer
  initializeWlBuffer,

  -- * wl_surface
  ServerSurface,
  initializeServerSurface,
  initializeServerSubsurface,
  getServerSurface,
  assignSurfaceRole,
  removeSurfaceRole,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Resources
import Quasar.Resources.Rc
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Core (attachOrRunFinalizer)
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (appAsRect)
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Utils.Resources


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
  -- Damage specified in surface coordinates (i.e. produced by wl_surface.damage
  -- instead of wl_surface.damage_buffer). Damage can be converted to buffer
  -- coordinates on commit (NOTE: conversion requires wl_surface version 4)
  pendingSurfaceDamage :: TVar [Rectangle]
}

data ServerBuffer b = ServerBuffer {
  wlBuffer :: Object 'Server Interface_wl_buffer,
  createFrame :: TDisposer -> STMc NoRetry '[] (Frame b)
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

getServerSurface :: forall b. RenderBackend b => Object 'Server Interface_wl_surface -> STMc NoRetry '[SomeException] (ServerSurface b)
getServerSurface wlSurface =
  getInterfaceData @(ServerSurface b) wlSurface >>= \case
    Nothing -> throwM (userError "Invalid server surface")
    Just serverSurface -> pure serverSurface

commitServerSurface :: RenderBackend b => ServerSurface b -> STMc NoRetry '[SomeException] ()
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

commitMappedServerSurface :: forall b. RenderBackend b => ServerSurface b -> MappedServerSurface b -> STMc NoRetry '[SomeException] ()
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
      frameRelease <- newUnmanagedTDisposer (tryCall sb.wlBuffer.release)

      rawFrame <- liftSTMc $ sb.createFrame frameRelease
      frame <- newRc rawFrame

      -- TODO Instead of voiding the future we might want to delay the
      -- frameCallback?
      liftSTMc $ void $ commitSurfaceDownstream mapped.surfaceDownstream SurfaceCommit {
        frame,
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
  forall b. RenderBackend b =>
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


initializeServerSurface :: forall b. RenderBackend b => NewObject 'Server Interface_wl_surface -> STMc NoRetry '[] ()
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


-- | Called by a buffer implementation (e.g. the implementation for
-- @wl_shm@, @zwp_linux_dmabuf_v1@ or @wp_single_pixel_buffer_manager_v1@) to
-- initialize a new @wl_buffer@ object.
--
-- The @createFrame@ function will be called whenever the buffer is committed
-- by a wayland client. The disposer provided to @createFrame@ needs to be
-- embedded in the frame and has to be disposed when the frame is disposed - it
-- is used to signal @wl_buffer::release@.
--
-- The @unmapBufferDisposer@ is disposed when the buffer is unmapped by the
-- wayland client (or on connection loss). The disposer can be used as a signal
-- that no further frames will be created from the buffer.
--
-- Resources related to the buffer object should only be released once the
-- buffer has been unmapped and all frames created from the buffer have been
-- destroyed. As documented on @wl_surface::attach@, unmapping a buffer does not
-- invalidate its content (therefore all frames created from the buffer remain
-- valid until they are destroyed).
initializeWlBuffer ::
  forall buffer backend. (IsBufferBackend buffer backend, Disposable buffer) =>
  backend ->
  NewObject 'Server Interface_wl_buffer ->
  buffer ->
  STMc NoRetry '[] ()
initializeWlBuffer backend wlBuffer buffer = do
  mappedBuffer <- newExternalBuffer backend (Borrowed (getDisposer buffer) buffer)
  rc <- newRc mappedBuffer
  let serverBuffer = ServerBuffer {
    wlBuffer,
    createFrame = createFrameImpl rc
  }
  setInterfaceData wlBuffer (serverBuffer :: ServerBuffer backend)
  setRequestHandler wlBuffer RequestHandler_wl_buffer {
    destroy = pure ()
  }
  -- TODO This removes back pressure for released buffers. We should await the
  -- @unmapBufferDisposer@ somewhere in the chain of new buffer allocations.
  -- The best place would probably be to delay the frame callback, but I'm not
  -- sure how to do that properly and cleanly.
  attachOrRunFinalizer wlBuffer (disposeEventually_ rc)

  where
    createFrameImpl rc frameRelease = do
      tryDuplicateRc rc >>= \case
        Nothing ->
          -- Frame was created from an unmapped buffer, which would probably be
          -- a bug somewhere in this module.
          undefined
        Just dupedRc -> createExternalBufferFrame @buffer @backend backend frameRelease dupedRc


getServerBuffer :: forall b. RenderBackend b => Object 'Server Interface_wl_buffer -> STMc NoRetry '[SomeException] (ServerBuffer b)
getServerBuffer wlBuffer = do
  ifd <- getInterfaceData @(ServerBuffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)


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
  commitSurfaceDownstream _self _commit = pure () <$ traceM "Subsurface committed"
  unmapSurfaceDownstream _self = traceM "Subsurface unmapped"

initializeServerSubsurface ::
  forall b. RenderBackend b =>
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
