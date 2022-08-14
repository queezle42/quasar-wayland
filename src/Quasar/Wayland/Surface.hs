module Quasar.Wayland.Surface (
  BufferBackend(..),
  ShmBufferBackend(..),
  Surface,
  newSurface,
  assignSurfaceRole,
  initializeServerSurface,
) where

import Control.Monad.Catch
import Data.Typeable
import GHC.Records
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (Rectangle(..), appAsRect)

class (Typeable b, Typeable (Buffer b)) => BufferBackend (b :: Type) where
  type Buffer b

getBuffer :: forall b. BufferBackend b => Object 'Server Interface_wl_buffer -> STM (Buffer b)
getBuffer wlBuffer = do
  ifd <- getInterfaceData @(Buffer b) wlBuffer
  case ifd of
    Just buffer -> pure buffer
    Nothing -> throwM $ InternalError ("Missing interface data on " <> show wlBuffer)


data ShmBufferBackend = ShmBufferBackend
data ShmBuffer = ShmBuffer

instance BufferBackend ShmBufferBackend where
  type Buffer ShmBufferBackend = ShmBuffer


class SurfaceRole a where
  surfaceRoleName :: a -> String

data SomeSurfaceRole = forall a. SurfaceRole a => SomeSurfaceRole a

instance SurfaceRole SomeSurfaceRole where
  surfaceRoleName (SomeSurfaceRole role) = surfaceRoleName role


data Surface b = Surface {
  surfaceRole :: TVar (Maybe SomeSurfaceRole),
  surfaceState :: TVar (SurfaceState b),
  pendingSurfaceState :: TVar (SurfaceState b),
  pendingSurfaceDamage :: TVar [Rectangle],
  pendingBufferDamage :: TVar [Rectangle]
}

data SurfaceState b = SurfaceState {
  buffer :: Maybe (Buffer b),
  offset :: (Int32, Int32)
}

defaultSurfaceState :: SurfaceState b
defaultSurfaceState = SurfaceState {
  buffer = Nothing,
  offset = (0, 0)
}

newtype ServerSurface b = ServerSurface (Surface b)

newSurface :: forall b. STM (Surface b)
newSurface = do
  surfaceRole <- newTVar Nothing
  surfaceState <- newTVar (defaultSurfaceState @b)
  pendingSurfaceState <- newTVar (defaultSurfaceState @b)
  pendingSurfaceDamage <- newTVar mempty
  pendingBufferDamage <- newTVar mempty
  pure Surface {
    surfaceRole,
    surfaceState,
    pendingSurfaceState,
    pendingSurfaceDamage,
    pendingBufferDamage
  }

modifyPending :: forall b. Surface b -> (SurfaceState b -> SurfaceState b) -> STM ()
modifyPending surface fn = modifyTVar surface.pendingSurfaceState fn

assignSurfaceRole :: SurfaceRole a => Surface b -> a -> STM ()
assignSurfaceRole surface role = do
  readTVar surface.surfaceRole >>= \case
    Just currentRole ->
      let msg = mconcat ["Cannot change wl_surface role. Current role is ", surfaceRoleName currentRole, "; new role is ", surfaceRoleName role]
      in throwM (ProtocolUsageError msg)
    Nothing -> pure ()

  writeTVar surface.surfaceRole (Just (SomeSurfaceRole role))


commitSurface :: forall b. Surface b -> STM ()
commitSurface surface = do
  state <- readTVar surface.pendingSurfaceState

  -- TODO propagate damage
  _surfaceDamage <- swapTVar surface.pendingSurfaceDamage mempty
  _bufferDamage <- swapTVar surface.pendingBufferDamage mempty

  writeTVar surface.surfaceState state
  writeTVar surface.pendingSurfaceState $
    state {
      buffer = Nothing
    }

  traceM "committed"

  -- TODO effects

setSurfaceContent :: forall b. Surface b -> Maybe (Buffer b) -> Int32 -> Int32 -> STM ()
setSurfaceContent surface buffer x y =
  modifyPending surface \s ->
    s {
      buffer,
      offset = (x, y)
    }

damageSurface :: forall b. Surface b -> Rectangle -> STM ()
damageSurface surface rect =
  modifyTVar surface.pendingSurfaceDamage (rect:)

damageBuffer :: forall b. Surface b -> Rectangle -> STM ()
damageBuffer surface rect =
  modifyTVar surface.pendingBufferDamage (rect:)

initializeServerSurface :: forall b. BufferBackend b => Object 'Server Interface_wl_surface -> STM ()
initializeServerSurface wlSurface = do
  surface <- newSurface @b
  traceM "setting message handler"
  setMessageHandler wlSurface RequestHandler_wl_surface {
    -- TODO ensure role is destroyed before surface
    destroy = pure (),
    attach = attach surface,
    damage = appAsRect (damageSurface surface),
    frame = \callback -> pure (),
    set_opaque_region = \region -> pure (),
    set_input_region = \region -> pure (),
    commit = commitSurface surface,
    set_buffer_transform = \transform -> pure (),
    set_buffer_scale = \scale -> pure (),
    damage_buffer = appAsRect (damageBuffer surface)
  }
  setInterfaceData wlSurface (ServerSurface @b surface)
  traceM "wl_surface not implemented"
  where
    attach :: Surface b -> Maybe (Object 'Server Interface_wl_buffer) -> Int32 -> Int32 -> STM ()
    attach surface wlBuffer x y = do
      buffer <- mapM (getBuffer @b) wlBuffer
      setSurfaceContent surface buffer x y
