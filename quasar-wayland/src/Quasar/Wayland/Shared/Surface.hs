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
import Quasar.Wayland.Backend
import Quasar.Wayland.Region (Rectangle(..))


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
