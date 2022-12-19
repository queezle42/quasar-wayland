module Quasar.Wayland.Utils.Once (
  once,
  once1
) where

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

once1 :: MonadSTM m => (b -> STM a) -> m (b -> STM a)
once1 fn = runOnce1 <$> newTVar (Left fn)

runOnce1 :: TVar (Either (b -> STM a) a) -> b -> STM a
runOnce1 var arg = do
  readTVar var >>= \case
    Left fn -> do
      result <- fn arg
      writeTVar var (Right result)
      pure result
    Right result -> pure result
