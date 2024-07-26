module Quasar.Wayland.Server.Shm (
  shmGlobal,
) where

import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Observable.Core
import Quasar.Observable.ObservableVar
import Quasar.Prelude
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Shared.Surface
import Quasar.Wayland.Shm


shmGlobal :: forall b. IsBufferBackend ShmBuffer b => b -> Global
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
      create_pool = initializeWlShmPool,
      release = pure ()
    }

    initializeWlShmPool ::
      NewObject 'Server Interface_wl_shm_pool ->
      SharedFd ->
      Int32 ->
      STMc NoRetry '[SomeException] ()
    initializeWlShmPool wlShmPool fd size = liftSTMc do
      sizeVar <- newObservableVar size
      pool <- newRc =<< newShmPool fd (toObservable sizeVar)
      attachFinalizer wlShmPool (disposeEventually_ pool)
      setRequestHandler wlShmPool RequestHandler_wl_shm_pool {
        create_buffer = initializeWlShmBuffer pool,
        destroy = disposeEventually_ pool,
        resize = \s -> do
          oldSize <- readObservableVar sizeVar
          when (oldSize > s) $ throwC $ ProtocolUsageError (mconcat ["wl_shm: Invalid resize from ", show oldSize, " to ", show size])
          writeObservableVar sizeVar s
      }

    initializeWlShmBuffer ::
      Rc ShmPool ->
      NewObject 'Server Interface_wl_buffer ->
      Int32 ->
      Int32 ->
      Int32 ->
      Int32 ->
      Word32 ->
      STMc NoRetry '[SomeException] ()
    initializeWlShmBuffer primaryPoolRc wlBuffer offset width height stride format = do
      pool <- cloneAndExtractRc primaryPoolRc
      liftSTMc do
        shmBuffer <- newShmBuffer pool offset width height stride format
        initializeWlBuffer backend wlBuffer shmBuffer
