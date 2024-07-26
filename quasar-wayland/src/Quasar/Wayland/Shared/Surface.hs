module Quasar.Wayland.Shared.Surface (
  -- * Render backend
  RenderBackend(..),

  -- * Surface
  Damage(..),
  addDamage,
  SurfaceCommit(..),
  IsSurfaceDownstream(..),
  SurfaceDownstream,
  defaultSurfaceCommit,

  -- * Buffer import backend
  IsBufferBackend(..),
  ExternalFrame,
  createExternalBufferFrame,
  readExternalFrame,
  readExternalFrameIO,
  readOwnedExternalFrame,
  readOwnedExternalFrameIO,

  -- ** High-level usage
  newFrameConsumeBuffer,

  -- * Reexports
  Rectangle(..),
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
    backend -> Owned buffer -> STMc NoRetry '[] (Owned (ExternalBuffer buffer backend))

  -- | Create a backend-specific `Frame` from an `ExternalFrame`.
  --
  -- Ownership of the `ExternalFrame` is transferred to the callee, ownership
  -- of the resulting `Frame` is transferred to the caller.
  wrapExternalFrame :: Owned (ExternalFrame buffer backend) -> STMc NoRetry '[DisposedException] (Owned (Frame backend))

data ExternalFrame buffer backend = ExternalFrame TDisposer (Rc (ExternalBuffer buffer backend))


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
  Rc (ExternalBuffer buffer backend) ->
  STMc NoRetry '[DisposedException] (Owned (Frame backend))
createExternalBufferFrame frameRelease externalBufferRc = do
  wrapExternalFrame @buffer @backend (Owned (getDisposer frameRelease <> getDisposer externalBufferRc) (ExternalFrame frameRelease externalBufferRc))

readExternalFrame ::
  MonadSTMc NoRetry '[DisposedException] m =>
  ExternalFrame buffer backend -> m (ExternalBuffer buffer backend)
readExternalFrame (ExternalFrame _ var) = readRc var

readExternalFrameIO ::
  MonadIO m =>
  ExternalFrame buffer backend -> m (ExternalBuffer buffer backend)
readExternalFrameIO (ExternalFrame _ var) = readRcIO var

readOwnedExternalFrame ::
  MonadSTMc NoRetry '[DisposedException] m =>
  Owned (ExternalFrame buffer backend) ->
  m (Owned (ExternalBuffer buffer backend))
readOwnedExternalFrame (Owned disposer (ExternalFrame _ var)) =
  liftSTMc @NoRetry @'[DisposedException] do
    Owned disposer <$> readRc var

readOwnedExternalFrameIO ::
  MonadIO m =>
  Owned (ExternalFrame buffer backend) ->
  m (Owned (ExternalBuffer buffer backend))
readOwnedExternalFrameIO (Owned disposer (ExternalFrame _ var)) = liftIO do
  Owned disposer <$> readRcIO var


-- | Create a new frame by taking ownership of a buffer. The buffer will be
-- disposed when the frame is disposed.
--
-- The caller takes ownership of the resulting frame.
newFrameConsumeBuffer ::
  forall buffer backend. IsBufferBackend buffer backend =>
  backend -> Owned buffer -> STMc NoRetry '[DisposedException] (Owned (Frame backend))
newFrameConsumeBuffer backend buffer = do
  externalBuffer <- liftSTMc $ newExternalBuffer backend buffer
  externalBufferRc <- newRc externalBuffer
  createExternalBufferFrame @buffer @backend mempty externalBufferRc


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
