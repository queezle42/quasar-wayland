module Quasar.Wayland.Utils.Resources (
  Borrowed(..),
) where

import Quasar.Resources

data Borrowed a = Borrowed Disposer a

instance Disposable (Borrowed a) where
  getDisposer (Borrowed disposer _) = disposer
