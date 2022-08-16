module Quasar.Wayland.Surface (
  -- * Buffer backend
  BufferBackend(..),
  ShmBufferBackend(..),
  getBuffer,

  -- * Surface
  Damage(..),
  Surface,
  SurfaceCommit(..),
  defaultSurfaceCommit,
  newSurface,
  assignSurfaceRole,
  commitSurface,
) where

import Control.Monad.Catch
import Data.Typeable
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Region (Rectangle(..))

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


data Damage = DamageAll | DamageList [Rectangle]

instance Semigroup Damage where
  DamageAll <> _ = DamageAll
  _ <> DamageAll = DamageAll
  DamageList xs <> DamageList ys = DamageList (xs <> ys)


data Surface b = Surface {
  surfaceRole :: TVar (Maybe SomeSurfaceRole),
  surfaceState :: TVar (SurfaceCommit b),
  downstreams :: TVar [SurfaceDownstream b]
}

data SurfaceCommit b = SurfaceCommit {
  buffer :: Maybe (Buffer b),
  offset :: (Int32, Int32),
  bufferDamage :: Damage
}

type SurfaceDownstream b = SurfaceCommit b -> STM ()

defaultSurfaceCommit :: Damage -> SurfaceCommit b
defaultSurfaceCommit bufferDamage = SurfaceCommit {
  buffer = Nothing,
  offset = (0, 0),
  bufferDamage
}

newSurface :: forall b. STM (Surface b)
newSurface = do
  surfaceRole <- newTVar Nothing
  surfaceState <- newTVar (defaultSurfaceCommit DamageAll)
  downstreams <- newTVar []
  pure Surface {
    surfaceRole,
    surfaceState,
    downstreams
  }

assignSurfaceRole :: SurfaceRole a => Surface b -> a -> STM ()
assignSurfaceRole surface role = do
  readTVar surface.surfaceRole >>= \case
    Just currentRole ->
      let msg = mconcat ["Cannot change wl_surface role. Current role is ", surfaceRoleName currentRole, "; new role is ", surfaceRoleName role]
      in throwM (ProtocolUsageError msg)
    Nothing -> pure ()

  writeTVar surface.surfaceRole (Just (SomeSurfaceRole role))

commitSurface :: forall b. Surface b -> SurfaceCommit b -> STM ()
commitSurface surface commit = do
  downstreams <- readTVar surface.downstreams
  -- TODO handle exceptions, remove failed downstreams
  mapM_ ($ commit) downstreams

connectSurfaceDownstream :: forall b. Surface b -> SurfaceDownstream b -> STM ()
connectSurfaceDownstream = undefined
