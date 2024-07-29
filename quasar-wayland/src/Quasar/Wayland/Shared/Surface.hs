module Quasar.Wayland.Shared.Surface (
  -- * Render backend
  RenderBackend(..),

  -- * Surface
  Damage(..),
  addDamage,
  SurfaceCommit(..),
  mergeCommits,
  IsSurfaceDownstream(..),
  SurfaceDownstream,
  defaultSurfaceCommit,

  -- * Buffer import backend
  IsBufferBackend(..),
  createExternalBufferFrame,

  -- ** High-level usage
  newFrameConsumeBuffer,

  -- * Reexports
  Rectangle(..),
  Owned(..),
) where

import Data.Typeable
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Exceptions
import Quasar.Future (Future)
import Quasar.Prelude
import Quasar.Wayland.Region (Rectangle(..))

type RenderBackend :: Type -> Constraint
class Typeable b => RenderBackend b where
  type Frame b :: Type


class RenderBackend backend => IsBufferBackend buffer backend where
  type ExternalBuffer buffer backend
  type instance ExternalBuffer buffer _backend = buffer

  -- | Import an external buffer. The buffer may be mutable shared memory.
  --
  -- Takes ownership of the provided buffer object (the buffer has to be
  -- disposed by the ExternalBuffer when that is disposed).
  --
  -- Ownership of the resulting @ExternalBuffer@-object is transferred to the
  -- caller, who will ensure it is `dispose`d later.
  newExternalBuffer ::
    Owned (Rc backend) -> Owned buffer -> STMc NoRetry '[] (Owned (ExternalBuffer buffer backend))

  -- | Create a backend-specific `Frame` from an `ExternalBuffer`.
  importExternalBuffer :: Owned (ExternalBuffer buffer backend) -> STMc NoRetry '[DisposedException] (Owned (Frame backend))


-- | Create a frame from an @ExternalBuffer@.
--
-- The `TDisposer` argument is used to signal the frame release and will be
-- disposed when the frame is disposed.
--
-- Ownership of the `ExternalBuffer` rc is transferred to the callee, it will
-- also be disposed when the frame is disposed.
--
-- Intended for internal use.
createExternalBufferFrame ::
  forall buffer backend.
  IsBufferBackend buffer backend =>
  TDisposer ->
  Owned (ExternalBuffer buffer backend) ->
  STMc NoRetry '[DisposedException] (Owned (Frame backend))
createExternalBufferFrame frameRelease (Owned disposer externalBuffer) =
  importExternalBuffer @buffer @backend
    (Owned (getDisposer frameRelease <> getDisposer disposer) externalBuffer)

-- | Create a new frame by taking ownership of a buffer. The buffer will be
-- disposed when the frame is disposed.
--
-- The caller takes ownership of the resulting frame.
newFrameConsumeBuffer ::
  forall buffer backend. IsBufferBackend buffer backend =>
  Rc backend -> Owned buffer -> STMc NoRetry '[DisposedException] (Owned (Frame backend))
newFrameConsumeBuffer origBackend buffer = do
  backend <- cloneRc origBackend
  externalBuffer <- liftSTMc $ newExternalBuffer @buffer @backend backend buffer
  createExternalBufferFrame @buffer @backend mempty externalBuffer


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

-- | Merges two commits.
mergeCommits :: SurfaceCommit b -> Owned (SurfaceCommit b) -> Owned (SurfaceCommit b)
mergeCommits prev (Owned disposer next) = do
  Owned disposer SurfaceCommit {
    frame = next.frame,
    offset = next.offset,
    bufferDamage = prev.bufferDamage <> next.bufferDamage,
    frameCallback = case (prev.frameCallback, next.frameCallback) of
      (Just pfc, Just nfc) -> Just (\time -> pfc time >> nfc time)
      (pfc, Nothing) -> pfc
      (Nothing, nfc) -> nfc
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
  commitSurfaceDownstream :: a -> Owned (SurfaceCommit b) -> STMc NoRetry '[SomeException] (Future '[] ())

  -- | Called on a NULL surface commit.
  unmapSurfaceDownstream :: a -> STMc NoRetry '[SomeException] ()

instance IsSurfaceDownstream b (SurfaceDownstream b) where
  toSurfaceDownstream = id
  commitSurfaceDownstream (SurfaceDownstream x) = commitSurfaceDownstream x
  unmapSurfaceDownstream (SurfaceDownstream x) = unmapSurfaceDownstream x


defaultSurfaceCommit :: Owned (Rc (Frame b)) -> Owned (SurfaceCommit b)
defaultSurfaceCommit (Owned disposer frame) =
  Owned disposer SurfaceCommit {
    frame,
    offset = Nothing,
    bufferDamage = Nothing,
    frameCallback = Nothing
  }
