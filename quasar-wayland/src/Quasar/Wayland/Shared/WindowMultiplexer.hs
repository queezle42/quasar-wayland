module Quasar.Wayland.Shared.WindowMultiplexer (
  WindowMultiplexerFactory(..),
  WindowMultiplexer,

  attachDownstreamWindow,
) where

import Quasar.Disposer
import Quasar.Disposer.DisposableVar
import Quasar.Disposer.Rc
import Quasar.Prelude
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shared.WindowApi

newtype WindowMultiplexerFactory b = WindowMultiplexerFactory [WindowFactory b]

-- | A window multiplexer that behaves like a window (implements `IsWindow`),
-- but can attach to multiple downstream windows, i.e. it can show it's content
-- in multiple locations.
newtype WindowMultiplexer b = WindowMultiplexer (TDisposableVar (WindowMultiplexerState b))

data WindowMultiplexerState b = WindowMultiplexerState {
  factory :: WindowMultiplexerFactory b,
  windowProperties :: WindowProperties,
  downstreams :: TVar [Window b],
  lastConfiguration :: TVar (Maybe WindowConfiguration),
  lastCommit :: TVar (Maybe (SurfaceCommit b)),
  lastCommitLock :: TVar Disposer
}

instance Backend b => IsWindowManager b (WindowMultiplexer b) (WindowMultiplexerFactory b) where
  newWindow = newWindowMultiplexer

instance Backend b => IsWindow b (WindowMultiplexer b) where
  setFullscreen multiplexer fullscreen = do
    withState multiplexer \state -> do
      readTVar state.downstreams >>= \case
        [] -> pure ()
        (primary:_) -> setFullscreen primary fullscreen
  commitWindow (WindowMultiplexer var) serial windowCommit (Owned originalDisposer surfaceCommit) = do
    future <- tryReadTDisposableVar var >>= \case
      Nothing -> pure (pure ()) -- ignore commits when disposed
      Just state -> do
        windows <- readTVar state.downstreams
        -- TODO use correct per-downstream serial
        mconcat <$> forM windows \window -> do
          (Owned disposer frc) <- cloneRc surfaceCommit.frame
          let clonedSurfaceCommit = Owned disposer surfaceCommit {
            frame = frc
          }
          commitWindow window serial windowCommit clonedSurfaceCommit
    disposeEventually_ originalDisposer
    pure future
  ackWindowConfigure multiplexer serial = do
    withState multiplexer \state -> do
      windows <- readTVar state.downstreams
      -- TODO use correct per-downstream serial
      forM_ windows \window -> ackWindowConfigure window serial

instance Disposable (WindowMultiplexer b) where
  getDisposer (WindowMultiplexer var) = getDisposer var

newWindowMultiplexer ::
  WindowMultiplexerFactory b ->
  WindowProperties ->
  WindowCommit ->
  WindowConfigurationCallback ->
  WindowRequestCallback ->
  STMc NoRetry '[SomeException] (WindowMultiplexer b)
newWindowMultiplexer factory@(WindowMultiplexerFactory []) windowProperties commit configurationCallback requestCallback = undefined
newWindowMultiplexer factory@(WindowMultiplexerFactory (primaryDownstreamFactory:downstreamFactories)) windowProperties commit configurationCallback requestCallback = do
  lastConfiguration <- newTVar Nothing
  lastCommit <- newTVar Nothing
  lastCommitLock <- newTVar mempty
  primaryWindow <- primaryDownstreamFactory windowProperties commit configurationCallback requestCallback
  secondaryWindows <- forM downstreamFactories \downstreamFactory -> do
    downstreamFactory windowProperties commit (\_ -> pure ()) requestCallback
  downstreams <- newTVar (primaryWindow : secondaryWindows)
  let state = WindowMultiplexerState {
    factory,
    windowProperties,
    downstreams,
    lastConfiguration,
    lastCommit,
    lastCommitLock
  }
  WindowMultiplexer <$> newTDisposableVar state disposeWindowMultiplexer

disposeWindowMultiplexer :: WindowMultiplexerState b -> STMc NoRetry '[] ()
disposeWindowMultiplexer state = readTVar state.downstreams >>= mapM_ disposeEventually

withState :: MonadSTMc NoRetry '[] m => WindowMultiplexer b -> (WindowMultiplexerState b -> m ()) -> m ()
withState (WindowMultiplexer var) action = tryReadTDisposableVar var >>= mapM_ action

attachDownstreamWindow :: IsWindowManager b w a => WindowMultiplexer b -> a -> STMc NoRetry '[SomeException] TDisposer
attachDownstreamWindow (WindowMultiplexer var) downstreamWM = do
  tryReadTDisposableVar var >>= \case
    Nothing -> mempty
    Just state -> do
      window <- newWindow downstreamWM undefined undefined undefined undefined
      modifyTVar state.downstreams (toWindow window :)
      undefined
