module Quasar.Wayland.Skia.Thread (
  SkiaIO,
  queueSkiaIO,
  runSkiaIO,

  SkiaThread,
  newSkiaThread,
) where

import Control.Concurrent (forkOSWithUnmask)
import Control.Exception (mask_, try)
import Control.Monad (MonadPlus)
import Data.Bifunctor qualified as Bifunctor
import Quasar.Future
import Quasar.Prelude
import Quasar.Exceptions (AsyncException(..))


data SkiaJob = forall a. SkiaJob (IO a) (PromiseEx '[AsyncException] a)

newtype SkiaThread = SkiaThread (TQueue SkiaJob)

newSkiaThread :: IO SkiaThread
newSkiaThread = do
  queue <- newTQueueIO
  void $ mask_ $ forkOSWithUnmask \unmask -> forever do
    items <- atomically do
      items <- flushTQueue queue
      check (not (null items))
      pure items
    forM_ items \(SkiaJob job promise) -> do
      tryFulfillPromiseIO_ promise . Bifunctor.first (toEx . AsyncException) =<< try (unmask job)
  pure (SkiaThread queue)


-- | `IO` that has to run on the skia thread.
newtype SkiaIO a = SkiaIO (IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadFix, Alternative, MonadPlus, Monoid, Semigroup)

queueSkiaIO :: MonadSTMc NoRetry '[] m => SkiaThread -> SkiaIO a -> m (Future '[AsyncException] a)
queueSkiaIO (SkiaThread queue) (SkiaIO action) = do
  promise <- newPromise
  writeTQueue queue $ SkiaJob action promise
  pure (toFutureEx promise)

runSkiaIO :: MonadIO m => SkiaThread -> SkiaIO a -> m a
runSkiaIO thread action = liftIO do
  await =<< atomicallyC (queueSkiaIO thread action)
