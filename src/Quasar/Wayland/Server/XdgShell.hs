module Quasar.Wayland.Server.XdgShell (
  ServerWindowManager,
  newServerWindowManager,
  xdgShellGlobal,
) where

import Control.Monad.Catch
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Surface

data ServerWindowManager b = ServerWindowManager

newServerWindowManager :: STM (ServerWindowManager b)
newServerWindowManager = pure ServerWindowManager

xdgShellGlobal :: forall b. BufferBackend b => ServerWindowManager b -> Global
xdgShellGlobal wm =
  createGlobal @Interface_xdg_wm_base maxVersion (initializeXdgWmBase wm)

data XdgWmBase b = XdgWmBase {
  wm :: ServerWindowManager b
}

initializeXdgWmBase ::
  forall b.
  BufferBackend b =>
  ServerWindowManager b -> Object 'Server Interface_xdg_wm_base -> STM ()
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


data XdgSurface b = XdgSurface {
  wlXdgSurface :: Object 'Server Interface_xdg_surface,
  serverSurface :: ServerSurface b,
  hasRoleObject :: TVar Bool,
  surface :: TVar (Maybe (Surface b))
}

initializeXdgSurface ::
  forall b.
  BufferBackend b =>
  XdgWmBase b ->
  NewObject 'Server Interface_xdg_surface ->
  Object 'Server Interface_wl_surface ->
  STM ()
initializeXdgSurface wm wlXdgSurface wlSurface = do
  getServerSurface @b wlSurface >>= \case
    Just serverSurface -> initializeXdgSurface' wm wlXdgSurface serverSurface
    Nothing -> throwM (userError "Invalid server surface")

initializeXdgSurface' ::
  forall b.
  XdgWmBase b ->
  NewObject 'Server Interface_xdg_surface ->
  ServerSurface b ->
  STM ()
initializeXdgSurface' wm wlXdgSurface serverSurface = do
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
  surface <- newTVar Nothing
  let xdgSurface =
        XdgSurface {
          wlXdgSurface,
          serverSurface,
          hasRoleObject,
          surface
        }

  setRequestHandler wlXdgSurface RequestHandler_xdg_surface {
    destroy = destroyXdgSurface xdgSurface,
    get_toplevel = initializeXdgToplevel xdgSurface,
    get_popup = undefined,
    set_window_geometry = undefined,
    -- TODO
    ack_configure = \_serial -> pure ()
  }

destroyXdgSurface :: XdgSurface b -> STM ()
destroyXdgSurface xdgSurface =
  whenM (readTVar xdgSurface.hasRoleObject) do
    -- TODO convert to server error that is relayed to the client
    throwM (userError "Cannot destroy xdg_surface before its role object has been destroyed.")

data XdgToplevel b = XdgToplevel {
  xdgSurface :: XdgSurface b
}

initializeXdgToplevel :: XdgSurface b -> NewObject 'Server Interface_xdg_toplevel -> STM ()
initializeXdgToplevel xdgSurface wlXdgToplevel = do
  writeTVar xdgSurface.hasRoleObject True

  let xdgToplevel = XdgToplevel {
    xdgSurface
  }

  -- NOTE this throws if the surface role is changed
  -- TODO change error type to a corret ServerError if that happens
  assignSurfaceRole
    @Interface_xdg_toplevel
    xdgSurface.serverSurface
    (onInitialSurfaceCommit xdgToplevel)

  setRequestHandler wlXdgToplevel RequestHandler_xdg_toplevel {
    destroy = destroyXdgToplevel xdgToplevel,
    set_parent = undefined,
    set_title = \title -> pure (),
    set_app_id = undefined,
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

  -- TODO proper mechanism for configure events
  xdgSurface.wlXdgSurface.configure 0

onInitialSurfaceCommit :: XdgToplevel b -> Surface b -> STM ()
onInitialSurfaceCommit xdgToplevel surface =
  writeTVar xdgToplevel.xdgSurface.surface (Just surface)

onNullSurfaceCommit :: XdgToplevel b -> STM ()
onNullSurfaceCommit = undefined -- TODO unmap surface

destroyXdgToplevel :: XdgToplevel b -> STM ()
destroyXdgToplevel xdgToplevel = do
  removeSurfaceRole xdgToplevel.xdgSurface.serverSurface
  writeTVar xdgToplevel.xdgSurface.surface Nothing
  writeTVar xdgToplevel.xdgSurface.hasRoleObject False
