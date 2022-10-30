module Quasar.Wayland.Client.XdgShell (
  getClientWindowManager
) where

import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated


data ClientWindowManager b = ClientWindowManager {
  wlXdgWmBase :: Object 'Client Interface_xdg_wm_base
}


newClientWindowManager :: WaylandClient -> STM (ClientWindowManager b)
newClientWindowManager client = do
  wlXdgWmBase <- bindSingleton client.registry
  pure ClientWindowManager { wlXdgWmBase }

getClientWindowManager :: forall b. ClientBufferBackend b => WaylandClient -> STM (ClientWindowManager b)
getClientWindowManager client = getClientComponent (newClientWindowManager @b client) client


