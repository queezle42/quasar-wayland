module Quasar.Wayland.SinglePixelBuffer (
  SinglePixelBuffer(..),
  IsSinglePixelBufferBackend(..),
) where

import Quasar.Disposer
import Quasar.Prelude
import Quasar.Wayland.Shared.Surface

data SinglePixelBuffer = SinglePixelBuffer Word32 Word32 Word32 Word32
  deriving (Eq, Generic)

instance Hashable SinglePixelBuffer


class Backend backend => IsSinglePixelBufferBackend backend where
  -- | Create a frame from an @SingPixelBuffer@.
  createSinglePixelBufferFrame :: backend -> SinglePixelBuffer -> STMc NoRetry '[] (Owned (Frame backend))
