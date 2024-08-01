module Quasar.Wayland.Shared.Surface (
  -- * Render backend
  RenderBackend(..),

  -- * Surface
  Damage(..),
  addDamage,
  SurfaceCommit(..),
  mergeCommits,
  defaultSurfaceCommit,

  -- * Reexports
  Rectangle(..),
  Owned(..),
) where

import Quasar.Disposer
import Quasar.Disposer.Rc
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

defaultSurfaceCommit :: Owned (Rc (Frame b)) -> Owned (SurfaceCommit b)
defaultSurfaceCommit (Owned disposer frame) =
  Owned disposer SurfaceCommit {
    frame,
    offset = Nothing,
    bufferDamage = Nothing,
    frameCallback = Nothing
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
