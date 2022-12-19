module Quasar.Wayland.Surface (
  -- * Buffer backend
  BufferBackend(..),
  Buffer(storage),
  newBuffer,
  lockBuffer,
  destroyBuffer,
  isBufferDestroyed,
  addBufferReleaseCallback,
  addBufferDestroyedCallback,

  -- * Surface
  Damage(..),
  addDamage,
  SurfaceCommit(..),
  IsSurfaceDownstream(..),
  SurfaceDownstream,
  IsSurfaceUpstream(..),
  SurfaceUpstream,
  defaultSurfaceCommit,

  --Surface,
  --newSurface,
  --commitSurface,

  -- * Reexports
  Rectangle(..),
) where

import Data.Hashable (Hashable(..))
import Data.Typeable
import Quasar.Prelude
import Quasar.Wayland.Region (Rectangle(..))
import Quasar.Wayland.Utils.Once

type BufferBackend :: Type -> Constraint
class Typeable b => BufferBackend b where
  type BufferStorage b :: Type


data Buffer b = Buffer {
  key :: Unique,
  storage :: BufferStorage b,
  -- | Buffer has been released by all current users and can be reused by the owner.
  releaseBufferCallback :: TVar (STM ()),
  -- | Refcount that tracks how many times the buffer is locked by consumers.
  lockCount :: TVar Int,
  destroyRequested :: TVar Bool,
  destroyed :: TVar Bool,
  destroyedCallback :: TVar (STM ())
}

instance Eq (Buffer b) where
  x == y = x.key == y.key

instance Hashable (Buffer b) where
  hash x = hash x.key
  hashWithSalt salt x = hashWithSalt salt x.key

newBuffer :: forall b. BufferStorage b -> STM () -> STM (Buffer b)
newBuffer storage bufferDestroyedFn = do
  key <- newUniqueSTM
  releaseBufferCallback <- newTVar (pure ())
  lockCount <- newTVar 0
  destroyRequested <- newTVar False
  destroyed <- newTVar False
  destroyedCallback <- newTVar bufferDestroyedFn
  pure Buffer {
    key,
    storage,
    releaseBufferCallback,
    lockCount,
    destroyRequested,
    destroyed,
    destroyedCallback
  }

addBufferReleaseCallback :: Buffer b -> STM () -> STM ()
addBufferReleaseCallback buffer releaseFn =
  modifyTVar buffer.releaseBufferCallback (>> releaseFn)

addBufferDestroyedCallback :: Buffer b -> STM () -> STM ()
addBufferDestroyedCallback buffer callback =
  modifyTVar buffer.destroyedCallback (>> callback)

-- | Prevents the buffer from being released. Returns an unlock action.
lockBuffer :: Buffer b -> STM (STM ())
lockBuffer buffer = do
  modifyTVar buffer.lockCount succ
  once unlockBuffer
  where
    unlockBuffer :: STM ()
    unlockBuffer = do
      lockCount <- stateTVar buffer.lockCount (dup . pred)
      when (lockCount == 0) do
        join $ swapTVar buffer.releaseBufferCallback (pure ())
        tryFinalizeBuffer buffer

-- | Request destruction of the buffer. Since the buffer might still be in use downstream, the backing storage must not be changed until all downstreams release the buffer (signalled finalization, e.g. `addBufferDestroyedCallback`).
destroyBuffer :: Buffer b -> STM ()
destroyBuffer buffer = do
  alreadyRequested <- readTVar buffer.destroyRequested
  unless alreadyRequested do
    writeTVar buffer.destroyRequested True
    tryFinalizeBuffer buffer

tryFinalizeBuffer :: Buffer b -> STM ()
tryFinalizeBuffer buffer = do
  destroyRequested <- readTVar buffer.destroyRequested
  lockCount <- readTVar buffer.lockCount
  when (destroyRequested && lockCount == 0) do
    writeTVar buffer.destroyed True
    -- Run callbacks
    join $ swapTVar buffer.destroyedCallback (pure ())

isBufferDestroyed :: Buffer b -> STM Bool
isBufferDestroyed buffer = readTVar buffer.destroyed


data Damage = DamageAll | DamageList [Rectangle]

instance Semigroup Damage where
  DamageAll <> _ = DamageAll
  _ <> DamageAll = DamageAll
  DamageList xs <> DamageList ys = DamageList (xs <> ys)

instance Monoid Damage where
  mempty = DamageList []

addDamage :: Rectangle -> Maybe Damage -> Maybe Damage
addDamage x Nothing = Just (DamageList [x])
addDamage _ r@(Just DamageAll) = r
addDamage x (Just (DamageList xs)) = Just (DamageList (x : xs))


data SurfaceCommit b = SurfaceCommit {
  buffer :: Maybe (Buffer b),
  offset :: Maybe (Int32, Int32),
  -- | May be empty on the first commit.
  bufferDamage :: Maybe Damage,
  frameCallback :: Maybe (Word32 -> STM ())
}


data SurfaceDownstream b = forall a. IsSurfaceDownstream b a => SurfaceDownstream a

class IsSurfaceDownstream b a | a -> b where
  toSurfaceDownstream :: a -> SurfaceDownstream b
  toSurfaceDownstream = SurfaceDownstream
  commitSurfaceDownstream :: a -> SurfaceCommit b -> STM ()

instance IsSurfaceDownstream b (SurfaceDownstream b) where
  toSurfaceDownstream = id
  commitSurfaceDownstream (SurfaceDownstream x) = commitSurfaceDownstream x


data SurfaceUpstream b = forall a. IsSurfaceUpstream b a => SurfaceUpstream a

class IsSurfaceUpstream b a | a -> b where
  toSurfaceUpstream :: a -> SurfaceUpstream b
  toSurfaceUpstream = SurfaceUpstream
  connectSurfaceDownstream :: IsSurfaceDownstream b d => a -> d -> STM ()

instance IsSurfaceUpstream b (SurfaceUpstream b) where
  toSurfaceUpstream = id
  connectSurfaceDownstream (SurfaceUpstream x) = connectSurfaceDownstream @b x


defaultSurfaceCommit :: SurfaceCommit b
defaultSurfaceCommit = SurfaceCommit {
  buffer = Nothing,
  offset = Nothing,
  bufferDamage = Nothing,
  frameCallback = Nothing
}


---- TODO Needs to be renamed. SurfaceCache? SurfaceMultiplexer?
---- This is only required when attaching multiple consumers to the same surface upstream.
--data Surface b = Surface {
--  cachedBuffer :: TVar (Buffer b),
--  frameCallback :: TVar (Maybe (Word32 -> STM ())),
--  -- Stores an STM action that will release the currently committed buffer.
--  bufferUnlockFn :: TVar (STM ()),
--  downstreams :: TVar [SurfaceDownstream b]
--}

--newSurface :: forall b. SurfaceState b -> STM (Surface b)
--newSurface state = do
--  surfaceState <- newTVar state
--  bufferUnlockFn <- newTVar (pure ())
--  downstreams <- newTVar []
--  pure Surface {
--    surfaceState,
--    bufferUnlockFn,
--    downstreams
--  }
--
--commitSurface :: forall b. Surface b -> SurfaceCommit b -> STM ()
--commitSurface surface commit = do
--  unlockFn <-
--    case commit.buffer of
--      Just buffer -> lockBuffer buffer
--      Nothing -> pure (pure ())
--
--  -- Store new unlockFn and then unlock previously used buffer
--  join $ swapTVar surface.bufferUnlockFn unlockFn
--
--  -- Even with multiple downstreams, the frame callback should only be called once
--  frameCallback <- mapM once1 commit.frameCallback
--
--  downstreams <- readTVar surface.downstreams
--  -- TODO handle exceptions, remove failed downstreams
--  forM_ downstreams \downstream ->
--    commitSurfaceDownstream downstream commit { frameCallback }
--
--instance IsSurfaceUpstream b (Surface b) where
--  connectSurfaceDownstream surface downstream = do
--    modifyTVar surface.downstreams (toSurfaceDownstream downstream:)
--    state <- readTVar surface.surfaceState
--    let commit = SurfaceCommit {
--      buffer = Just (state.buffer :: Buffer b),
--      offset = Nothing,
--      -- Initial commit, so no damage tracking is required
--      bufferDamage = Nothing,
--      frameCallback = state.frameCallback
--    }
--    -- TODO handle exceptions
--    commitSurfaceDownstream downstream commit
