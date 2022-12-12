module Quasar.Wayland.Server.XdgShell (
  xdgShellGlobal,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Shared.WindowManagerApi
import Quasar.Wayland.Surface

xdgShellGlobal :: forall b wm. IsWindowManager b wm => wm -> Global
xdgShellGlobal wm =
  createGlobal @Interface_xdg_wm_base maxVersion (initializeXdgWmBase @b wm)

data XdgWmBase b wm = IsWindowManager b wm => XdgWmBase {
  wm :: wm
}

initializeXdgWmBase ::
  forall b a.
  IsWindowManager b a =>
  a -> Object 'Server Interface_xdg_wm_base -> STM ()
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


data XdgSurface b wm = XdgSurface {
  xdgWmBase :: XdgWmBase b wm,
  wlXdgSurface :: Object 'Server Interface_xdg_surface,
  serverSurface :: ServerSurface b,
  hasRoleObject :: TVar Bool
}

initializeXdgSurface ::
  forall b wm.
  IsWindowManager b wm =>
  XdgWmBase b wm ->
  NewObject 'Server Interface_xdg_surface ->
  Object 'Server Interface_wl_surface ->
  STM ()
initializeXdgSurface wm wlXdgSurface wlSurface = do
  getServerSurface wlSurface >>= \case
    Just serverSurface -> initializeXdgSurface' wm wlXdgSurface serverSurface
    Nothing -> throwM (userError "Invalid server surface")

initializeXdgSurface' ::
  forall b wm.
  IsWindowManager b wm =>
  XdgWmBase b wm ->
  NewObject 'Server Interface_xdg_surface ->
  ServerSurface b ->
  STM ()
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

  hasRoleObject <- newTVar False
  let xdgSurface =
        XdgSurface {
          xdgWmBase,
          wlXdgSurface,
          serverSurface,
          hasRoleObject
        }

  setRequestHandler wlXdgSurface RequestHandler_xdg_surface {
    destroy = destroyXdgSurface xdgSurface,
    get_toplevel = initializeXdgToplevel xdgSurface,
    get_popup = undefined,
    set_window_geometry = undefined,
    -- TODO
    ack_configure = \_serial -> pure ()
  }

destroyXdgSurface :: XdgSurface b wm -> STM ()
destroyXdgSurface xdgSurface =
  whenM (readTVar xdgSurface.hasRoleObject) do
    -- TODO convert to server error that is relayed to the client
    throwM (userError "Cannot destroy xdg_surface before its role object has been destroyed.")

data XdgToplevel b wm = XdgToplevel {
  window :: Window b wm,
  xdgSurface :: XdgSurface b wm,
  wlXdgToplevel :: Object 'Server Interface_xdg_toplevel
}

instance IsWindowManager b wm => IsSurfaceDownstream b (XdgToplevel b wm) where
  commitSurfaceDownstream xdgToplevel surfaceCommit =
    commitWindowContent xdgToplevel.window unsafeConfigureSerial surfaceCommit

initializeXdgToplevel :: forall b wm. IsWindowManager b wm => XdgSurface b wm -> NewObject 'Server Interface_xdg_toplevel -> STM ()
initializeXdgToplevel xdgSurface wlXdgToplevel = do
  writeTVar xdgSurface.hasRoleObject True

  void $ mfix \window -> do

    let xdgToplevel = XdgToplevel {
      window,
      xdgSurface,
      wlXdgToplevel
    }

    -- NOTE this throws if the surface role is changed
    -- TODO change error type to a corret ServerError if that happens
    assignSurfaceRole
      @Interface_xdg_toplevel
      xdgSurface.serverSurface
      (toSurfaceDownstream xdgToplevel)

    setRequestHandler wlXdgToplevel RequestHandler_xdg_toplevel {
      destroy = destroyXdgToplevel xdgToplevel,
      set_parent = undefined,
      set_title = setTitle window,
      set_app_id = setAppId window,
      show_window_menu = undefined,
      move = undefined,
      resize = undefined,
      set_max_size = undefined,
      set_min_size = undefined,
      set_maximized = undefined,
      unset_maximized = undefined,
      set_fullscreen = undefined,
      unset_fullscreen = undefined,
      set_minimized = undefined
    }

    -- `newWindow` might call the configure callback immediately, so the window
    -- should be created after request handlers are attached.
    newWindow xdgSurface.xdgWmBase.wm (sendConfigureEvent xdgToplevel)

sendConfigureEvent :: XdgToplevel b wm -> WindowConfiguration -> STM ()
sendConfigureEvent xdgToplevel windowConfiguration = do
  traceM "Sending window configuration"

  xdgToplevel.wlXdgToplevel.configure windowConfiguration.width windowConfiguration.height windowConfiguration.states
  xdgToplevel.xdgSurface.wlXdgSurface.configure 0

onNullSurfaceCommit :: XdgToplevel b wm -> STM ()
onNullSurfaceCommit = undefined -- TODO unmap surface

destroyXdgToplevel :: XdgToplevel b wm -> STM ()
destroyXdgToplevel xdgToplevel = do
  removeSurfaceRole xdgToplevel.xdgSurface.serverSurface
  writeTVar xdgToplevel.xdgSurface.hasRoleObject False
