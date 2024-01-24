module Quasar.Wayland.Server.Shm (
  shmGlobal,
) where

import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Shm
import Quasar.Wayland.Server.Surface


shmGlobal :: IsShmBufferBackend b => b -> Global
shmGlobal backend = createGlobal @Interface_wl_shm maxVersion initializeWlShm
  where
    initializeWlShm :: NewObject 'Server Interface_wl_shm -> STMc NoRetry '[SomeException] ()
    initializeWlShm wlShm = do
      setRequestHandler wlShm shmRequestHandler
      -- argb8888 (0) and xrgb8888 (1) are required by the spec
      -- TODO add more formats later (i.e. 10bit formats are missing right now)
      wlShm.format 0
      wlShm.format 1

    shmRequestHandler :: RequestHandler_wl_shm
    shmRequestHandler = RequestHandler_wl_shm {
      create_pool = initializeWlShmPool
    }

    initializeWlShmPool ::
      NewObject 'Server Interface_wl_shm_pool ->
      SharedFd ->
      Int32 ->
      STMc NoRetry '[SomeException] ()
    initializeWlShmPool wlShmPool fd size = liftSTMc do
      pool <- newShmPool fd size
      setRequestHandler wlShmPool RequestHandler_wl_shm_pool {
        create_buffer = initializeWlShmBuffer pool,
        destroy = liftSTMc $ destroyShmPool pool,
        resize = resizeShmPool pool
      }

    initializeWlShmBuffer ::
      ShmPool ->
      NewObject 'Server Interface_wl_buffer ->
      Int32 ->
      Int32 ->
      Int32 ->
      Int32 ->
      Word32 ->
      STMc NoRetry '[SomeException] ()
    initializeWlShmBuffer pool wlBuffer offset width height stride format = liftSTMc do
      buffer <- newShmBuffer backend pool offset width height stride format
      initializeWlBuffer wlBuffer buffer
