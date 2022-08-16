module Quasar.Wayland.Server.Shm (
  shmGlobal,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Shm
import Quasar.Wayland.Server.Surface
import System.Posix (Fd)


shmGlobal :: Global
shmGlobal = createGlobal @Interface_wl_shm maxVersion initializeWlShm

shmRequestHandler :: RequestHandler_wl_shm
shmRequestHandler = RequestHandler_wl_shm {
  create_pool = initializeWlShmPool
}

initializeWlShm :: NewObject 'Server Interface_wl_shm -> STM ()
initializeWlShm wlShm = do
  setRequestHandler wlShm shmRequestHandler
  -- argb8888 (0) and xrgb8888 (1) are required by the spec
  -- TODO add more formats later (i.e. 10bit formats are missing right now)
  wlShm.format 0
  wlShm.format 1

initializeWlShmPool :: NewObject 'Server Interface_wl_shm_pool -> Fd -> Int32 -> STM ()
initializeWlShmPool wlShmPool fd size = do
  pool <- newShmPool fd size
  setRequestHandler wlShmPool RequestHandler_wl_shm_pool {
    create_buffer = initializeWlShmBuffer pool,
    destroy = destroyShmPool pool,
    resize = resizeShmPool pool
  }

initializeWlShmBuffer :: ShmPool -> NewObject 'Server Interface_wl_buffer -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> STM ()
initializeWlShmBuffer pool wlBuffer offset width height stride format = do
  shmBuffer <- newShmBuffer pool offset width height stride format releaseFn
  initializeWlBuffer @ShmBufferBackend wlBuffer shmBuffer
  where
    releaseFn :: STM ()
    -- TODO handle other exceptions (e.g. disconnected)
    releaseFn = unlessM (isDestroyed wlBuffer) wlBuffer.release
