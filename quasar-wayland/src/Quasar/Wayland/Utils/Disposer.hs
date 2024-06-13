module Quasar.Wayland.Utils.Disposer (
  Borrowed(..),
) where

import Quasar.Disposer

data Borrowed a = Borrowed Disposer a

instance Disposable (Borrowed a) where
  getDisposer (Borrowed disposer _) = disposer
