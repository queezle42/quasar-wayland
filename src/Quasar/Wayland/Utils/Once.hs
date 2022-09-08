module Quasar.Wayland.Utils.Once (once) where

import Quasar.Prelude

-- TODO use no-throw STM wrapper
once :: MonadSTM m => STM a -> m (STM a)
once fn = runOnce <$> newTVar (Left fn)

runOnce :: TVar (Either (STM a) a) -> STM a
runOnce var = do
  readTVar var >>= \case
    Left fn -> do
      result <- fn
      writeTVar var (Right result)
      pure result
    Right result -> pure result
