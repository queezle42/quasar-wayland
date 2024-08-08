module Quasar.Wayland.Server.Surface (
  -- * Globals
  compositorGlobal,
  subcompositorGlobal,

  -- * wl_buffer
  initializeWlBuffer,

  -- * wl_surface
  ServerSurface,
  getServerSurface,
  assignSurfaceRole,
  removeSurfaceRole,

  IsSurfaceRole(..),
  SurfaceRole,
) where

import Control.Monad.Catch
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Exceptions
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Backend
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Shared.Surface

compositorGlobal :: forall b. RenderBackend b => Global b
compositorGlobal = createGlobal @Interface_wl_compositor maxVersion bindCompositor
  where
    bindCompositor :: Object 'Server Interface_wl_compositor -> STMc NoRetry '[SomeException] ()
    bindCompositor wlCompositor = setMessageHandler wlCompositor handler

    handler :: RequestHandler_wl_compositor
    handler = RequestHandler_wl_compositor {
      create_surface = \wlSurface -> liftSTMc $ initializeServerSurface @b wlSurface,
      create_region = \wlRegion -> liftSTMc $ initializeServerRegion wlRegion
    }

subcompositorGlobal :: forall b. RenderBackend b => Global b
subcompositorGlobal = createGlobal @Interface_wl_subcompositor maxVersion bindCompositor
  where
    bindCompositor :: Object 'Server Interface_wl_subcompositor -> STMc NoRetry '[SomeException] ()
    bindCompositor wlCompositor = setMessageHandler wlCompositor handler

    handler :: RequestHandler_wl_subcompositor
    handler = RequestHandler_wl_subcompositor {
      destroy = pure (), -- Destroy has no effect, as specified.
      get_subsurface = initializeServerSubsurface @b
    }


data SurfaceRole b
  = forall a. IsSurfaceRole b a => SurfaceRole a
  | SurfaceRoleSubsurface (Subsurface b)

class IsSurfaceRole b a | a -> b where
  toSurfaceRole :: a -> SurfaceRole b
  toSurfaceRole = SurfaceRole

  -- TODO Don't allow exceptions or limit allowed exception types. Currently implementations of this leak exceptions across a responsibility bondary.

  -- | Called on surface commit.
  --
  -- Ownership of the frame lock is transferred to the callee. The callee must
  -- ensure the frame lock is disposed at an appropriate time, or resources will
  -- be leaked.
  commitSurfaceRole :: a -> Owned (SurfaceCommit b) -> STMc NoRetry '[SomeException] (Future '[] ())

  -- | Called on a NULL surface commit.
  unmapSurfaceRole :: a -> STMc NoRetry '[SomeException] ()


data ServerSurface b = ServerSurface {
  state :: TVar (ServerSurfaceState b),
  lastRole :: TVar (Maybe String),
  pendingFrameCallback :: TVar (Maybe (Word32 -> STMc NoRetry '[] ())),
  pendingSubsurfaces :: TVar (Seq (Subsurface b))
}

data ServerSurfaceState b =
  -- TODO this isn't actually unmapped, it's "no role assigned"
  NoRole |
  -- | Surface role was assigned
  RolePending (PendingServerSurface b) |
  -- Surface role has been commited
  -- TODO this isn't necessarily mapped
  RoleActive (ActiveServerSurface b)

newtype PendingServerSurface b = PendingServerSurface {
  surfaceRole :: SurfaceRole b
}

data ActiveServerSurface b = ActiveServerSurface {
  surfaceRole :: SurfaceRole b,
  pendingBuffer :: TMVar (Maybe (ServerBuffer b)),
  pendingOffset :: TVar (Maybe (Int32, Int32)),
  pendingBufferDamage :: TVar (Maybe Damage),
  -- Damage specified in surface coordinates (i.e. produced by wl_surface.damage
  -- instead of wl_surface.damage_buffer). Damage can be converted to buffer
  -- coordinates on commit (NOTE: conversion requires wl_surface version 4)
  pendingSurfaceDamage :: TVar [Rectangle],

  -- Content generated during the last `commit`-request. Can also be updated
  -- during a desynchronized subsurface commit.
  content :: TVar (Maybe (Content b))
}

-- Committed surface content
data Content b = Content {
  frame :: Owned (Rc (Frame b)),
  subsurfaceContents :: Seq (Unique, Maybe (Content b))
}

instance Disposable (Content b) where
  getDisposer content =
    getDisposer content.frame <> foldMap (foldMap getDisposer . snd) content.subsurfaceContents

data ServerBuffer b = ServerBuffer {
  wlBuffer :: Object 'Server Interface_wl_buffer,
  createFrame :: TDisposer -> STMc NoRetry '[DisposedException] (Owned (Frame b))
}

newServerSurface :: STMc NoRetry '[] (ServerSurface b)
newServerSurface = do
  state <- newTVar NoRole
  lastRole <- newTVar Nothing
  pendingFrameCallback <- newTVar Nothing
  pendingSubsurfaces <- newTVar mempty

  pure ServerSurface {
    state,
    lastRole,
    pendingFrameCallback,
    pendingSubsurfaces
  }

getServerSurface :: forall b. RenderBackend b => Object 'Server Interface_wl_surface -> STMc NoRetry '[SomeException] (ServerSurface b)
getServerSurface wlSurface =
  getInterfaceData @(ServerSurface b) wlSurface >>= \case
    Nothing -> throwM (userError "Invalid server surface")
    Just serverSurface -> pure serverSurface

commitServerSurface :: ServerSurface b -> STMc NoRetry '[SomeException] ()
commitServerSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    NoRole -> throwM $ userError "Cannot commit a surface that does not have a role"
    RolePending pending -> do
      mappedSurface <- liftSTMc $ activateServerSurface pending
      writeTVar serverSurface.state (RoleActive mappedSurface)
    RoleActive activeSurface -> commitActiveServerSurface serverSurface activeSurface

activateServerSurface :: PendingServerSurface b -> STMc NoRetry '[] (ActiveServerSurface b)
activateServerSurface pending = do
  pendingBuffer <- newEmptyTMVar
  pendingOffset <- newTVar Nothing
  pendingBufferDamage <- newTVar Nothing
  pendingSurfaceDamage <- newTVar mempty

  content <- newTVar Nothing

  pure ActiveServerSurface {
    surfaceRole = pending.surfaceRole,
    pendingBuffer,
    pendingOffset,
    pendingBufferDamage,
    pendingSurfaceDamage,
    content
  }

commitActiveServerSurface ::
  forall b.
  ServerSurface b -> ActiveServerSurface b -> STMc NoRetry '[SomeException] ()
commitActiveServerSurface surface mapped = do
  serverBuffer <- tryTakeTMVar mapped.pendingBuffer
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

  oldFrame :: Maybe (Owned (Rc (Frame b))) <- readTVar mapped.content >>= \case
    Nothing -> pure Nothing
    Just oldContent -> do
      mapM_ (mapM_ disposeEventually_ . snd) oldContent.subsurfaceContents
      pure (Just oldContent.frame)


  -- If a buffer was attached, create a frame from the buffer and store it
  frame :: Maybe (Owned (Rc (Frame b))) <- case serverBuffer of
    Nothing -> do
      -- No buffer attached, so the previous frame is kept.
      case oldFrame of
        Nothing -> do
          -- No subsurface update is required if the surface is already unmapped.
          -- TODO is this a bug or a no-op?
          traceM "Surface committed without a buffer, but the surface is already unmapped."
          pure Nothing
        Just oldFrame' -> pure (Just oldFrame')
    Just (Just sb) -> do
      -- New frame.
      mapM_ disposeEventually_ oldFrame
      frameRelease <- newTDisposer (tryCall sb.wlBuffer.release)
      frame <- liftSTMc $ newRc =<< sb.createFrame frameRelease
      pure (Just frame)
    Just Nothing -> do
      -- Unmapping (i.e. attaching NULL buffer)
      when (isJust frameCallback) $ throwM $ userError "Must not attach frame callback when unmapping surface"
      mapM_ disposeEventually_ oldFrame
      pure Nothing

  content :: Maybe (Content b) <- forM frame \frame' -> do
    -- Update subsurface order and content (if surface is mapped)
    subsurfaces <- readTVar surface.pendingSubsurfaces
    subsurfaceContents <- mapM getClonedSubsurfaceContent subsurfaces
    pure Content {
      frame = frame',
      subsurfaceContents
    }

  writeTVar mapped.content content

  case mapped.surfaceRole of
    SurfaceRoleSubsurface subsurface ->
      -- Desynchronized subsurfaces create a parent surface update.
      whenM (liftSTMc (isDesynchronizedSubsurface subsurface)) do
        void $ propagateDesynchronizedSubsurfaceChange subsurface content

    SurfaceRole role -> do
      commitSurfaceRoleInternal role content offset frameCallback


-- Does not take ownership of the content.
commitSurfaceRoleInternal ::
  IsSurfaceRole b a =>
  a ->
  Maybe (Content b) ->
  Maybe (Int32, Int32) ->
  Maybe (Word32 -> STMc NoRetry '[] ()) ->
  STMc NoRetry '[SomeException] ()
commitSurfaceRoleInternal role content offset frameCallback = do
  case content of
    Nothing -> unmapSurfaceRole role
    Just content' -> do
      -- Current surface is a normal surface with a "normal" role, i.e. not
      -- a surface modifier like subsurface.

      (Owned disposer compoundFrame) <- createCompoundFrame content'
      -- TODO Instead of voiding the future we might want to delay the
      -- frameCallback?
      void $ commitSurfaceRole role $
        Owned disposer SurfaceCommit {
          frame = compoundFrame,
          offset,
          frameCallback
        }


getClonedSubsurfaceContent :: Subsurface b -> STMc NoRetry '[SomeException] (Unique, Maybe (Content b))
getClonedSubsurfaceContent subsurface = (subsurface.key,) <$> do
  readTVar subsurface.surface.state >>= \case
    RoleActive active -> do
      content <- readTVar active.content
      mapM cloneContent content
    _ -> pure Nothing

cloneContent :: Content b -> STMc NoRetry '[SomeException] (Content b)
cloneContent content = do
  frame <- cloneRc (fromOwned content.frame)
  subsurfaceContents <- forM content.subsurfaceContents
    \(subsurface, subsurfaceContent) -> do
      ownedContent <- mapM cloneContent subsurfaceContent
      pure (subsurface, ownedContent)
  pure Content {
    frame,
    subsurfaceContents
  }

requireMappedSurface :: ServerSurface b -> STMc NoRetry '[SomeException] (ActiveServerSurface b)
requireMappedSurface serverSurface = do
  readTVar serverSurface.state >>= \case
    RoleActive mapped -> pure mapped
    -- TODO improve exception / propagate error to the client
    _ -> throwM $ userError "Requested operation requires a mapped surface"

-- Does not take ownership of content.
createCompoundFrame :: Content b -> STMc NoRetry '[SomeException] (Owned (Rc (Frame b)))
createCompoundFrame content = do
  case content.subsurfaceContents of
    Seq.Empty -> do
      cloneRc (fromOwned content.frame)
    subsurfaces -> undefined

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
  writeTMVar mappedSurface.pendingBuffer buffer
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
    damage_buffer = appAsRect (damageBuffer serverSurface),
    offset = \_x _y -> undefined -- TODO not implemented
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
  forall buffer backend. IsBufferBackend buffer backend =>
  Rc backend ->
  NewObject 'Server Interface_wl_buffer ->
  Owned buffer ->
  STMc NoRetry '[DisposedException] ()
initializeWlBuffer backendRc wlBuffer buffer = do
  backend <- cloneRc backendRc
  liftSTMc do
    mappedBuffer <- newExternalBuffer backend buffer
    rc <- newRc mappedBuffer
    let serverBuffer = ServerBuffer {
      wlBuffer,
      createFrame = createFrameImpl (fromOwned rc)
    }
    setInterfaceData wlBuffer (serverBuffer :: ServerBuffer backend)
    setRequestHandler wlBuffer RequestHandler_wl_buffer {
      destroy = pure ()
    }
    -- TODO This removes back pressure for released buffers. We should await the
    -- disposer somewhere in the chain of new buffer allocations.
    -- The best place would probably be to delay the frame callback, but I'm not
    -- sure how to do that in a clean way.
    -- We don't want to delay the whole rendering backend (since that could be
    -- rendering content for/from multiple clients).
    attachOrRunFinalizer wlBuffer (disposeEventually_ rc)

  where
    createFrameImpl :: Rc (ExternalBuffer buffer backend) -> TDisposer -> STMc NoRetry '[DisposedException] (Owned (Frame backend))
    createFrameImpl rc frameRelease = do
      -- If duplicating the frame rc fails, the frame was created from an
      -- unmapped buffer, which would probably be a bug somewhere in this module.
      dupedRc <- cloneAndExtractRc rc
      createExternalBufferFrame @buffer @backend frameRelease dupedRc


getServerBuffer :: forall b. RenderBackend b => Object 'Server Interface_wl_buffer -> STMc NoRetry '[SomeException] (ServerBuffer b)
getServerBuffer wlBuffer = do
  ifd <- getInterfaceData @(ServerBuffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)


assignSurfaceRole :: forall i b. IsInterfaceSide 'Server i => ServerSurface b -> SurfaceRole b -> STMc NoRetry '[SomeException] ()
assignSurfaceRole surface surfaceRole = do
  let role = interfaceName @i

  readTVar surface.state >>= \case
    RoleActive _ -> throwM (ProtocolUsageError "Cannot assign wl_surface a new role, since it already has an active role.")
    RolePending _ -> throwM (ProtocolUsageError "Cannot assign wl_surface a new role, since it already has a pending role.")
    NoRole -> pure ()

  writeTVar surface.state $ RolePending PendingServerSurface { surfaceRole }

  readTVar surface.lastRole >>= \case
    Just ((== role) -> True) -> pure ()
    Just currentRole ->
      let msg = mconcat ["Cannot change wl_surface role. The last role was ", currentRole, "; new role is ", role]
      in throwM (ProtocolUsageError msg)
    Nothing -> writeTVar surface.lastRole (Just role)

removeSurfaceRole :: ServerSurface b -> STMc NoRetry '[] ()
removeSurfaceRole surface = do
  readTVar surface.state >>= \case
    NoRole -> traceM "TODO removing surface role from surface without role - bug?"
    RolePending _ ->
      writeTVar surface.state NoRole
    RoleActive state -> do
      writeTVar surface.state NoRole
      -- TODO fire frame callback to destroy it?
      mapM_ disposeEventually_ =<< swapTVar state.content Nothing

data SubsurfaceMode = Synchronized | Desynchronized
  deriving (Eq, Show)

data Subsurface b = Subsurface {
  key :: Unique,
  surface :: ServerSurface b,
  parentSurface :: ServerSurface b,
  subsurfaceMode :: TVar SubsurfaceMode
}

isDesynchronizedSurface :: ServerSurface b -> STMc NoRetry '[] Bool
isDesynchronizedSurface surface = do
  readTVar surface.state >>= \case
    NoRole -> pure False
    RolePending _ -> pure False
    RoleActive active -> do
      case active.surfaceRole of
        SurfaceRole _ -> pure False
        SurfaceRoleSubsurface subsurface ->
          isDesynchronizedSubsurface subsurface

isDesynchronizedSubsurface :: Subsurface b -> STMc NoRetry '[] Bool
isDesynchronizedSubsurface subsurface = do
  readTVar subsurface.subsurfaceMode >>= \case
    Synchronized -> pure False
    Desynchronized -> isDesynchronizedSurface subsurface.parentSurface

-- Propagate surface change from subsurfaces to parent or to role object.
--
-- Must only be called for active desynchronized subsurfaces (checked using
-- `isDesynchronizedSurface`).
--
-- Subsurface content is borrowed and will be cloned if required.
propagateDesynchronizedSubsurfaceChange :: Subsurface b -> Maybe (Content b) -> STMc NoRetry '[SomeException] ()
propagateDesynchronizedSubsurfaceChange subsurface content = do
  readTVar subsurface.parentSurface.state >>= \case
    RoleActive parentActive -> do
      readTVar parentActive.content >>= \case
        Just parentContent -> do

          ownedContent <- mapM cloneContent content

          case replaceSubsurfaceContent subsurface.key ownedContent parentContent of
            Nothing -> unreachableCodePathM
            Just (disposer, newParentContent) -> do
              disposeEventually_ disposer

              writeTVar parentActive.content (Just newParentContent)

              propagateDesynchronizedSurfaceChange
                subsurface.parentSurface
                parentActive
                (Just newParentContent)

        Nothing -> unreachableCodePathM
    _ -> unreachableCodePathM

propagateDesynchronizedSurfaceChange :: ServerSurface b -> ActiveServerSurface b -> Maybe (Content b) -> STMc NoRetry '[SomeException] ()
propagateDesynchronizedSurfaceChange surface active content = do
  case active.surfaceRole of
    SurfaceRoleSubsurface subsurface -> do
      propagateDesynchronizedSubsurfaceChange subsurface content
    SurfaceRole role -> do
      commitSurfaceRoleInternal role content Nothing Nothing

replaceSubsurfaceContent :: Unique -> Maybe (Content b) -> Content b -> Maybe (Disposer, Content b)
replaceSubsurfaceContent key newContent parentContent = do
  let parentSubsurfaceContents = parentContent.subsurfaceContents
  Seq.findIndexL (\(iKey, _) -> iKey == key) parentSubsurfaceContents <&> \i ->
    case Seq.splitAt i parentSubsurfaceContents of
      (_pre, Empty) -> unreachableCodePath
      (pre, x :<| post) ->
        (foldMap getDisposer (snd x), parentContent {
          subsurfaceContents = pre <> ((key, newContent) :<| post)
        })


initializeServerSubsurface ::
  forall b. RenderBackend b =>
  NewObject 'Server Interface_wl_subsurface ->
  Object 'Server Interface_wl_surface ->
  Object 'Server Interface_wl_surface ->
  STMc NoRetry '[SomeException] ()
initializeServerSubsurface wlSubsurface wlSurface wlParent = do
  key <- newUniqueSTM
  surface <- getServerSurface @b wlSurface
  parentSurface <- getServerSurface @b wlParent
  subsurfaceMode <- newTVar Synchronized
  let subsurface = Subsurface {
    key,
    surface,
    parentSurface,
    subsurfaceMode
  }
  assignSurfaceRole @Interface_wl_subsurface surface (SurfaceRoleSubsurface subsurface)
  attachFinalizer wlSubsurface (destroySubsurface subsurface)
  setRequestHandler wlSubsurface RequestHandler_wl_subsurface {
    destroy = pure (),
    set_position = \x y -> traceM (mconcat ["TODO: Subsurface position: ", show x, ", ", show y]),
    place_above = \sibling -> traceM "TODO: Subsurface above",
    place_below = \sibling -> traceM "TODO: Subsurface below",
    set_sync = setSynchronized subsurface,
    set_desync = setDesynchronized subsurface
  }

setSynchronized :: Subsurface b -> STMc NoRetry '[SomeException] ()
setSynchronized subsurface = writeTVar subsurface.subsurfaceMode Synchronized

setDesynchronized :: Subsurface b -> STMc NoRetry '[SomeException] ()
setDesynchronized subsurface = do
  writeTVar subsurface.subsurfaceMode Desynchronized
  whenM (liftSTMc (isDesynchronizedSubsurface subsurface)) do
    readTVar subsurface.surface.state >>= \case
      RoleActive active -> do
        content <- readTVar active.content
        -- NOTE In theory this implementation should only propagate a change
        -- if the subsurface has changed (= was committed) since it's parent
        -- was last committed. Since this implementation does not track that
        -- information and always propagates the change, since `set_desync` is
        -- usually only used during setup and it's easier to implement this way.
        propagateDesynchronizedSubsurfaceChange subsurface content
      _ -> pure ()

destroySubsurface :: Subsurface b -> STMc NoRetry '[] ()
destroySubsurface subsurface = do
  removeSurfaceRole subsurface.surface
  modifyTVar subsurface.parentSurface.pendingSubsurfaces
    (Seq.filter \x -> x.key /= subsurface.key)
  -- NOTE: The spec says "Destroying a sub-surface takes effect immediately."
  -- This can be interpreted in multiple ways. It may be necessary to also
  -- propagate a surface change.
  readTVar subsurface.parentSurface.state >>= \case
    RoleActive active -> do
      oldContent <- readTVar active.content
      forM_ oldContent \content -> do
        let (removed, remaining) = Seq.partition (\(key, _) -> key /= subsurface.key) content.subsurfaceContents
        mapM_ (mapM_ disposeEventually_ . snd) removed
        writeTVar active.content $ Just content {
          subsurfaceContents = remaining
        }
    _ -> pure ()
