module Quasar.Wayland.Surface (
  -- * Buffer backend
  BufferBackend(..),
  Buffer,
  newBuffer,
  lockBuffer,
  destroyBuffer,
  addBufferReleaseCallback,

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
import Quasar.Wayland.Region (Rectangle(..))

type BufferBackend :: Type -> Constraint
class Typeable b => BufferBackend b where
  type BufferContent b
  -- | A destroyed buffer has been released, so the buffer storage can be freed by the owner.
  releaseBufferStorage :: BufferContent b -> STM ()

data Buffer b = Buffer {
  content :: BufferContent b,
  -- | Buffer has been released by all current users and can be reused by the owner.
  releaseBuffer :: TVar (STM ()),
  -- | Refcount that tracks how many times the buffer is locked by consumers.
  lockCount :: TVar Int,
  destroyed :: TVar Bool
}

newBuffer :: forall b. BufferContent b -> STM (Buffer b)
newBuffer content = do
  releaseBuffer <- newTVar (pure ())
  lockCount <- newTVar 0
  destroyed <- newTVar False
  pure Buffer {
    content,
    releaseBuffer,
    lockCount,
    destroyed
  }


addBufferReleaseCallback :: Buffer b -> STM () -> STM ()
addBufferReleaseCallback buffer releaseFn =
  modifyTVar buffer.releaseBuffer (>> releaseFn)


-- | Prevents the buffer from being released. Returns an unlock action.
lockBuffer :: forall b. BufferBackend b => Buffer b -> STM (STM ())
lockBuffer buffer = do
  modifyTVar buffer.lockCount succ
  pure unlockBuffer
  where
    unlockBuffer :: STM ()
    unlockBuffer = do
      lockCount <- stateTVar buffer.lockCount (dup . pred)
      when (lockCount == 0) do
        join $ swapTVar buffer.releaseBuffer (pure ())
        tryFinalizeBuffer @b buffer

-- | Request destruction of the buffer. Since the buffer might still be in use downstream, the backing storage must not be changed until all downstreams release the buffer (signalled by `releaseBufferStorage`).
destroyBuffer :: forall b. BufferBackend b => Buffer b -> STM ()
destroyBuffer buffer = do
  alreadyDestroyed <- readTVar buffer.destroyed
  unless alreadyDestroyed do
    writeTVar buffer.destroyed True
    tryFinalizeBuffer buffer

tryFinalizeBuffer :: forall b. BufferBackend b => Buffer b -> STM ()
tryFinalizeBuffer buffer = do
  destroyed <- readTVar buffer.destroyed
  lockCount <- readTVar buffer.lockCount
  when (destroyed && lockCount == 0) do
    releaseBufferStorage @b buffer.content


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

instance Monoid Damage where
  mempty = DamageList []


data Surface b = Surface {
  surfaceRole :: TVar (Maybe SomeSurfaceRole),
  surfaceState :: TVar (SurfaceCommit b),
  lastBufferUnlockFn :: TVar (STM ()),
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
  lastBufferUnlockFn <- newTVar (pure ())
  downstreams <- newTVar []
  pure Surface {
    surfaceRole,
    surfaceState,
    lastBufferUnlockFn,
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

commitSurface :: forall b. BufferBackend b => Surface b -> SurfaceCommit b -> STM ()
commitSurface surface commit = do
  join $ readTVar surface.lastBufferUnlockFn

  unlockFn <-
    case commit.buffer of
      Just buffer -> lockBuffer @b buffer
      Nothing -> pure (pure ())

  writeTVar surface.lastBufferUnlockFn unlockFn

  downstreams <- readTVar surface.downstreams
  -- TODO handle exceptions, remove failed downstreams
  mapM_ ($ commit) downstreams

connectSurfaceDownstream :: forall b. Surface b -> SurfaceDownstream b -> STM ()
connectSurfaceDownstream = undefined
