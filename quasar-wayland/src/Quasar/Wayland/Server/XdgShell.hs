module Quasar.Wayland.Server.XdgShell (
  xdgShellGlobal,
) where

import Control.Monad.Catch
import Quasar.Disposer
import Quasar.Observable.Core (toObservable)
import Quasar.Observable.ObservableVar
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Shared.WindowApi

xdgShellGlobal :: forall b w wm. IsWindowManager b w wm => wm -> Global b
xdgShellGlobal wm =
  createGlobal @Interface_xdg_wm_base maxVersion (initializeXdgWmBase @b wm)

data XdgWmBase b w wm = IsWindowManager b w wm => XdgWmBase {
  wm :: wm
}

initializeXdgWmBase ::
  forall b w a.
  IsWindowManager b w a =>
  a -> Object 'Server Interface_xdg_wm_base -> STMc NoRetry '[SomeException] ()
initializeXdgWmBase wm wlXdgWm = do
  let xdgWmBase = XdgWmBase { wm }
  setRequestHandler wlXdgWm RequestHandler_xdg_wm_base {
    -- TODO raise error if any surface derived from this xdg_wm_base is still
    -- alive
    destroy = pure (),
    create_positioner = undefined,
    get_xdg_surface = initializeXdgSurface xdgWmBase,
    pong = const (pure ())
  }


data XdgSurface b w wm = XdgSurface {
  xdgWmBase :: XdgWmBase b w wm,
  wlXdgSurface :: Object 'Server Interface_xdg_surface,
  serverSurface :: ServerSurface b,
  hasRoleObject :: TVar Bool,
  geometryVar :: TVar (Int32, Int32, Int32, Int32)
}

initializeXdgSurface ::
  forall b w wm.
  IsWindowManager b w wm =>
  XdgWmBase b w wm ->
  NewObject 'Server Interface_xdg_surface ->
  Object 'Server Interface_wl_surface ->
  STMc NoRetry '[SomeException] ()
initializeXdgSurface wm wlXdgSurface wlSurface = do
  serverSurface <- getServerSurface wlSurface
  liftSTMc $ initializeXdgSurface' wm wlXdgSurface serverSurface

initializeXdgSurface' ::
  forall b w wm.
  IsWindowManager b w wm =>
  XdgWmBase b w wm ->
  NewObject 'Server Interface_xdg_surface ->
  ServerSurface b ->
  STMc NoRetry '[] ()
initializeXdgSurface' xdgWmBase wlXdgSurface serverSurface = do
  -- The spec states that "It is illegal to create an xdg_surface for a
  -- wl_surface which already has an assigned role and this will result in a
  -- protocol error."
  --
  -- In practice it's not as easy as just checking for an assigned role, since
  -- this might also occur the other way round (an xdg_surface is created and
  -- then the surface is assigned another role), or multiple xdg_surface objects
  -- might be created for the same wl_surface.
  --
  -- Since an xdg_surface has no effect in itself (in version 5 of xdg_surface),
  -- this part of the spec is ignored in this implementation. A role object is
  -- only set when creating a toplevel- or popup surface.

  geometryVar <- newTVar (0, 0, 0, 0)

  hasRoleObject <- newTVar False
  let xdgSurface =
        XdgSurface {
          xdgWmBase,
          wlXdgSurface,
          serverSurface,
          hasRoleObject,
          geometryVar
        }

  setRequestHandler wlXdgSurface RequestHandler_xdg_surface {
    destroy = destroyXdgSurface xdgSurface,
    get_toplevel = \newToplevel -> liftSTMc $ initializeXdgToplevel xdgSurface newToplevel,
    get_popup = undefined,
    set_window_geometry = \x y w h -> writeTVar geometryVar (x, y, w, h),
    -- TODO
    ack_configure = \_serial -> pure ()
  }

resetXdgSurface :: XdgSurface b w wm -> STMc NoRetry '[] ()
resetXdgSurface xdgSurface = do
  writeTVar xdgSurface.geometryVar (0, 0, 0, 0)

destroyXdgSurface :: XdgSurface b w wm -> STMc NoRetry '[SomeException] ()
destroyXdgSurface xdgSurface =
  whenM (readTVar xdgSurface.hasRoleObject) do
    -- TODO convert to server error that is relayed to the client
    throwM (userError "Cannot destroy xdg_surface before its role object has been destroyed.")

data XdgToplevel b w wm = XdgToplevel {
  state :: TVar (XdgToplevelState w),
  xdgSurface :: XdgSurface b w wm,
  wlXdgToplevel :: Object 'Server Interface_xdg_toplevel,
  windowProperties :: WindowProperties,
  maxSizeVar :: TVar (Int32, Int32),
  minSizeVar :: TVar (Int32, Int32)
}

data XdgToplevelState w
  = Unmapped
  | Created w
  | Configured w
  | Mapped w



instance IsWindowManager b w wm => IsSurfaceRole b (XdgToplevel b w wm) where
  commitSurfaceRole xdgToplevel surfaceCommit = do
    minSize <- readTVar xdgToplevel.minSizeVar
    maxSize <- readTVar xdgToplevel.minSizeVar
    geometry <- readTVar xdgToplevel.xdgSurface.geometryVar
    window <- readTVar xdgToplevel.state >>= \case
      Unmapped -> throwC (userError "xdg_surface::error.unconfigured_buffer")
      Created _window -> throwC (userError "xdg_surface::error.unconfigured_buffer")
      Configured window -> do
        writeTVar xdgToplevel.state (Mapped window)
        pure window
      Mapped window -> pure window

    let windowCommit = WindowCommit {
      minSize,
      maxSize,
      geometry
    }
    commitWindow window unsafeConfigureSerial windowCommit surfaceCommit

  unmapSurfaceRole xdgToplevel = do
    readTVar xdgToplevel.state >>= \case
      Unmapped -> do
        window <- newWindow
          xdgToplevel.xdgSurface.xdgWmBase.wm
          xdgToplevel.windowProperties
          (sendConfigureEvent xdgToplevel)
          (sendWindowRequest xdgToplevel)
        writeTVar xdgToplevel.state (Created window)
      Created _window -> pure ()
      Configured _window -> pure ()
      Mapped window -> liftSTMc do
        disposeEventually_ window
        writeTVar xdgToplevel.state Unmapped
        resetXdgToplevel xdgToplevel

resetXdgToplevel :: XdgToplevel b w wm -> STMc NoRetry '[] ()
resetXdgToplevel xdgToplevel = do
  writeTVar xdgToplevel.maxSizeVar (0, 0)
  writeTVar xdgToplevel.minSizeVar (0, 0)
  resetXdgSurface xdgToplevel.xdgSurface

initializeXdgToplevel :: forall b w wm. IsWindowManager b w wm => XdgSurface b w wm -> NewObject 'Server Interface_xdg_toplevel -> STMc NoRetry '[SomeException] ()
initializeXdgToplevel xdgSurface wlXdgToplevel = do
  writeTVar xdgSurface.hasRoleObject True

  void $ mfix \window -> do

    state <- newTVar Unmapped

    titleVar <- newObservableVar ""
    appIdVar <- newObservableVar ""

    maxSizeVar <- newTVar (0, 0)
    minSizeVar <- newTVar (0, 0)

    let windowProperties = WindowProperties {
      title = toObservable titleVar,
      appId = toObservable appIdVar
    }

    let xdgToplevel = XdgToplevel {
      state,
      xdgSurface,
      wlXdgToplevel,
      windowProperties,
      maxSizeVar,
      minSizeVar
    }

    -- NOTE this throws if the surface role is changed
    -- TODO change error type to a correct ServerError if that happens
    assignSurfaceRole
      @Interface_xdg_toplevel
      xdgSurface.serverSurface
      (toSurfaceRole xdgToplevel)

    attachFinalizer wlXdgToplevel do
      void $ disposeEventually window

    setRequestHandler wlXdgToplevel RequestHandler_xdg_toplevel {
      destroy = liftSTMc $ destroyXdgToplevel xdgToplevel,
      set_parent = undefined,
      set_title = writeObservableVar titleVar,
      set_app_id = writeObservableVar appIdVar,
      show_window_menu = undefined,
      move = undefined,
      resize = undefined,
      set_max_size = \x y -> writeTVar maxSizeVar (x, y),
      set_min_size = \x y -> writeTVar minSizeVar (x, y),
      set_maximized = undefined,
      unset_maximized = undefined,
      set_fullscreen = \_ -> liftSTMc $ setFullscreen window True,
      unset_fullscreen = liftSTMc $ setFullscreen window False,
      set_minimized = undefined
    }

    -- `newWindow` might call the configure callback immediately, so the window
    -- should be created after request handlers are attached.
    newWindow xdgSurface.xdgWmBase.wm windowProperties (sendConfigureEvent xdgToplevel) (sendWindowRequest xdgToplevel)

sendConfigureEvent :: XdgToplevel b w wm -> WindowConfiguration -> STMc NoRetry '[SomeException] ()
sendConfigureEvent xdgToplevel windowConfiguration = do
  readTVar xdgToplevel.state >>= \case
    Created window -> writeTVar xdgToplevel.state (Configured window)
    _ -> pure ()
  traceM "Sending window configuration"

  xdgToplevel.wlXdgToplevel.configure windowConfiguration.width windowConfiguration.height windowConfiguration.states
  xdgToplevel.xdgSurface.wlXdgSurface.configure 0

sendWindowRequest :: XdgToplevel b w wm -> WindowRequest -> STMc NoRetry '[SomeException] ()
sendWindowRequest xdgToplevel WindowRequestClose = do
  traceM "Sending window close request"

  xdgToplevel.wlXdgToplevel.close

destroyXdgToplevel :: XdgToplevel b w wm -> STMc NoRetry '[] ()
destroyXdgToplevel xdgToplevel = do
  removeSurfaceRole xdgToplevel.xdgSurface.serverSurface
  writeTVar xdgToplevel.xdgSurface.hasRoleObject False
