module Quasar.Wayland.Server.Surface (
  initializeServerSurface,
  initializeWlBuffer,
  getBuffer,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (Rectangle(..), appAsRect)
import Quasar.Wayland.Surface


data ServerSurface b = ServerSurface {
  surface :: Surface b,
  pendingSurfaceCommit :: TVar (SurfaceCommit b),
  -- Damage specified in surface coordinates (i.e. produced by wl_surface.damage instead of wl_surface.damage_buffer).
  -- Damage can be converted to buffer coordinates on commit (NOTE: conversion requires wl_surface version 4)
  pendingSurfaceDamage :: TVar [Rectangle]
}

newServerSurface :: forall b. STM (ServerSurface b)
newServerSurface = do
  surface <- newSurface @b
  pendingSurfaceCommit <- newTVar (defaultSurfaceCommit (DamageList []))
  pendingSurfaceDamage <- newTVar []
  pure ServerSurface {
    surface,
    pendingSurfaceCommit,
    pendingSurfaceDamage
  }

modifyPending :: forall b. ServerSurface b -> (SurfaceCommit b -> SurfaceCommit b) -> STM ()
modifyPending surface fn = modifyTVar surface.pendingSurfaceCommit fn

commitServerSurface :: forall b. BufferBackend b => ServerSurface b -> STM ()
commitServerSurface surface = do
  pendingCommit <- readTVar surface.pendingSurfaceCommit

  surfaceDamage <- swapTVar surface.pendingSurfaceDamage mempty
  let convertedSurfaceDamage =
        case surfaceDamage of
          [] -> DamageList []
          -- TODO should do a coordinate conversion
          _ -> DamageAll

  let commit =
        pendingCommit {
          bufferDamage = pendingCommit.bufferDamage <> convertedSurfaceDamage
        }

  writeTVar surface.pendingSurfaceCommit $
    commit {
      buffer = Nothing,
      offset = (0, 0),
      bufferDamage = DamageList []
    }

  commitSurface surface.surface commit

attachToSurface :: forall b. BufferBackend b => ServerSurface b -> Maybe (Object 'Server Interface_wl_buffer) -> Int32 -> Int32 -> STM ()
attachToSurface surface wlBuffer x y = do
  buffer <- mapM (getBuffer @b) wlBuffer
  modifyPending surface \s ->
    s {
      buffer,
      offset = (x, y)
    }

damageSurface :: forall b. ServerSurface b -> Rectangle -> STM ()
damageSurface surface rect =
  modifyTVar surface.pendingSurfaceDamage (rect:)

damageBuffer :: forall b. ServerSurface b -> Rectangle -> STM ()
damageBuffer surface rect =
  modifyPending surface \case
    commit@SurfaceCommit{bufferDamage = DamageAll} -> commit
    commit@SurfaceCommit{bufferDamage = DamageList xs} -> commit { bufferDamage = DamageList (rect : xs) }


initializeServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STM ()
initializeServerSurface wlSurface = do
  surface <- newServerSurface @b
  -- TODO missing requests
  setMessageHandler wlSurface RequestHandler_wl_surface {
    -- TODO ensure role is destroyed before surface
    destroy = pure (),
    attach = attachToSurface surface,
    damage = appAsRect (damageSurface surface),
    frame = \callback -> pure (),
    set_opaque_region = \region -> pure (),
    set_input_region = \region -> pure (),
    commit = commitServerSurface surface,
    set_buffer_transform = \transform -> pure (),
    set_buffer_scale = \scale -> pure (),
    damage_buffer = appAsRect (damageBuffer surface)
  }
  setInterfaceData wlSurface surface
  traceM "wl_surface not implemented"

initializeWlBuffer :: forall b. BufferBackend b => NewObject 'Server Interface_wl_buffer -> Buffer b -> STM ()
initializeWlBuffer wlBuffer buffer = do
  setInterfaceData wlBuffer buffer
  setRequestHandler wlBuffer RequestHandler_wl_buffer {
    -- TODO propagate buffer destruction
    destroy = destroyBuffer buffer
  }

getBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STM (Buffer b)
getBuffer wlBuffer = do
  ifd <- getInterfaceData @(Buffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)
