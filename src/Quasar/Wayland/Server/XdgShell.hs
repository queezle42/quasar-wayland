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

initializeXdgWmBase ::
  forall b.
  BufferBackend b =>
  ServerWindowManager b -> Object 'Server Interface_xdg_wm_base -> STM ()
initializeXdgWmBase wm wlXdgWm =
  setRequestHandler wlXdgWm RequestHandler_xdg_wm_base {
    -- TODO raise error if any surface derived from this xdg_wm_base is still
    -- alive
    destroy = pure (),
    create_positioner = undefined,
    get_xdg_surface = initializeXdgSurface wm,
    pong = const (pure ())
  }


data XdgSurface b = XdgSurface {
  wlXdgSurface :: Object 'Server Interface_xdg_surface,
  serverSurface :: ServerSurface b,
  surfaceRole :: TVar (Maybe Role)
}

data Role = Toplevel | Popup

initializeXdgSurface ::
  forall b.
  BufferBackend b =>
  ServerWindowManager b ->
  NewObject 'Server Interface_xdg_surface ->
  Object 'Server Interface_wl_surface ->
  STM ()
initializeXdgSurface wm wlXdgSurface wlSurface = do
  getServerSurface @b wlSurface >>= \case
    Just serverSurface -> initializeXdgSurface' wm wlXdgSurface serverSurface
    Nothing -> throwM (userError "Invalid server surface")

initializeXdgSurface' ::
  forall b.
  ServerWindowManager b ->
  NewObject 'Server Interface_xdg_surface ->
  ServerSurface b ->
  STM ()
initializeXdgSurface' wm wlXdgSurface serverSurface = do
  -- The spec says that "It is illegal to create an xdg_surface for a wl_surface
  -- which already has an assigned role and this will result in a protocol
  -- error."
  --
  -- In practice it's not as easy as just checking for an assigned role, since
  -- this might also occur the other way round (an xdg_surface is created and
  -- then the surface is assigned another role), or multiple xdg_surface objects
  -- might be created for the same wl_surface.
  --
  -- Instead, since an xdg_surface has no effect in itself (in version 5 of
  -- xdg_surface), this part of the spec is ignored in this implementation. A
  -- role object is only set when creating a toplevel- or popup surface.

  surfaceRole <- newTVar Nothing
  let xdgSurface = XdgSurface { wlXdgSurface, serverSurface, surfaceRole }

  setRequestHandler wlXdgSurface RequestHandler_xdg_surface {
    destroy = destroyXdgSurface xdgSurface,
    get_toplevel = initializeXdgToplevel xdgSurface,
    get_popup = undefined,
    set_window_geometry = undefined,
    ack_configure = undefined
  }

destroyXdgSurface :: XdgSurface b -> STM ()
destroyXdgSurface surface = do
  readTVar surface.surfaceRole >>= \case
    Just _ -> throwM (userError "Cannot destroy xdg_surface before its role object has been destroyed.")
    Nothing -> pure ()

data XdgToplevel b = XdgToplevel {
  xdgSurface :: XdgSurface b
}

initializeXdgToplevel :: XdgSurface b -> NewObject 'Server Interface_xdg_toplevel -> STM ()
initializeXdgToplevel xdgSurface wlXdgToplevel = do
  -- NOTE this throws if the surface role is changed
  -- TODO change error type to a corret ServerError if that happens
  assignSurfaceRole @Interface_xdg_toplevel xdgSurface.serverSurface
  writeTVar xdgSurface.surfaceRole (Just Toplevel)

  let xdgToplevel = XdgToplevel {
    xdgSurface
  }

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

destroyXdgToplevel :: XdgToplevel b -> STM ()
destroyXdgToplevel xdgToplevel = do
  removeSurfaceRole xdgToplevel.xdgSurface.serverSurface
  writeTVar xdgToplevel.xdgSurface.surfaceRole Nothing
  undefined
