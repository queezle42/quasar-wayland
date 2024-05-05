module Quasar.Wayland.Shared.Surface (
  -- * Render backend
  RenderBackend(..),

  -- * Buffer import backend
  IsBufferBackend(..),
  newFrameConsumeBuffer,

  -- * Surface
  Damage(..),
  addDamage,
  SurfaceCommit(..),
  IsSurfaceDownstream(..),
  SurfaceDownstream,
  defaultSurfaceCommit,

  -- * Reexports
  Rectangle(..),
) where

import Data.Typeable
import Quasar.Future (Future)
import Quasar.Prelude
import Quasar.Resources
import Quasar.Resources.Rc
import Quasar.Wayland.Region (Rectangle(..))
import Quasar.Wayland.Utils.Resources

type RenderBackend :: Type -> Constraint
class (Typeable b, Disposable (Frame b)) => RenderBackend b where
  type Frame b :: Type


class (RenderBackend backend, Disposable (ExternalBuffer buffer backend)) => IsBufferBackend buffer backend where
  type ExternalBuffer buffer backend
  -- | Import an external buffer. The buffer may be mutable shared memory.
  --
  -- Takes ownership of the provided `Borrowed`-object.
  --
  -- Ownership of the resulting @ExternalBuffer@-object is transferred to the
  -- caller, who will `dispose` it later.
  newExternalBuffer :: backend -> Borrowed buffer -> STMc NoRetry '[] (ExternalBuffer buffer backend)

  -- | Create a frame from an @ExternalBuffer@.
  createExternalBufferFrame :: backend -> TDisposer -> Rc (ExternalBuffer buffer backend) -> STMc NoRetry '[] (Frame backend)


-- | Create a new frame by taking ownership of a buffer. The buffer will be
-- disposed when the frame is disposed.
--
-- The caller takes ownership of the resulting frame.
newFrameConsumeBuffer :: forall buffer backend. (IsBufferBackend buffer backend, Disposable buffer) => backend -> buffer -> STMc NoRetry '[] (Frame backend)
newFrameConsumeBuffer backend buffer = do
  externalBuffer <- newExternalBuffer backend (Borrowed (getDisposer buffer) buffer)
  rc <- newRc externalBuffer
  createExternalBufferFrame @buffer backend mempty rc


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
  frame :: Rc (Frame b),
  offset :: Maybe (Int32, Int32),
  -- | May be empty on the first commit.
  bufferDamage :: Maybe Damage,
  frameCallback :: Maybe (Word32 -> STMc NoRetry '[] ())
}

-- | Release resources attached to this commit.
instance Disposable (SurfaceCommit b) where
  getDisposer commit = getDisposer commit.frame

instance Semigroup (SurfaceCommit b) where
  x <> y = SurfaceCommit {
    frame = y.frame,
    offset = y.offset,
    bufferDamage = x.bufferDamage <> y.bufferDamage,
    frameCallback = case (x.frameCallback, y.frameCallback) of
      (Just xfc, Just yfc) -> Just (\time -> xfc time >> yfc time)
      (xfc, Nothing) -> xfc
      (Nothing, yfc) -> yfc
  }


data SurfaceDownstream b = forall a. IsSurfaceDownstream b a => SurfaceDownstream a

class IsSurfaceDownstream b a | a -> b where
  toSurfaceDownstream :: a -> SurfaceDownstream b
  toSurfaceDownstream = SurfaceDownstream

  -- TODO Don't allow exceptions or limit allowed exception types. Currently implementations of this leak exceptions across a responsibility bondary.

  -- | Called on surface commit.
  --
  -- Ownership of the frame lock is transferred to the callee. The callee must
  -- ensure the frame lock is disposed at an appropriate time, or resources will
  -- be leaked.
  commitSurfaceDownstream :: a -> SurfaceCommit b -> STMc NoRetry '[SomeException] (Future '[] ())

  -- | Called on a NULL surface commit.
  unmapSurfaceDownstream :: a -> STMc NoRetry '[SomeException] ()

instance IsSurfaceDownstream b (SurfaceDownstream b) where
  toSurfaceDownstream = id
  commitSurfaceDownstream (SurfaceDownstream x) = commitSurfaceDownstream x
  unmapSurfaceDownstream (SurfaceDownstream x) = unmapSurfaceDownstream x


defaultSurfaceCommit :: Rc (Frame b) -> SurfaceCommit b
defaultSurfaceCommit frame = SurfaceCommit {
  frame,
  offset = Nothing,
  bufferDamage = Nothing,
  frameCallback = Nothing
}
