module Quasar.Wayland.Client.Backend (
  ClientBufferBackend(..),
  getBackendClientBufferManager,
) where

import Control.Monad.Catch
import Data.Typeable (Typeable)
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Exceptions
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Shared.Surface

class (RenderBackend b, Typeable (BackendClientBufferManager b), Hashable (ExportBufferId b)) => ClientBufferBackend b where
  type BackendClientBufferManager b
  type ExportBufferId b
  type RenderedFrame b
  newBackendClientBufferManager :: WaylandClient b -> STMc NoRetry '[SomeException] (BackendClientBufferManager b)

  -- | Render a frame into a buffer. The frame might be rendered into a new-,
  -- or into an existing buffer (chosen by the implementation of `renderFrame`).
  -- The buffer can then be exported by using `exportWlBuffer`. Exported buffers
  -- stay mapped to the server and might be reused, if the buffer is already
  -- mapped it should not be mapped again. To identity which buffer the frame
  -- has been rendered to, use `getExportBufferId`.
  --
  -- Disposing the `RenderedFrame` rc will release resources associated with the
  -- frame (if applicable) and releases the internal buffer so it can be reused
  -- or destroyed by the buffer owner.
  --
  -- Takes responsibility for the frame rc. The caller is responsible for the
  -- returned RenderedFrame rc.
  --
  -- The exported buffers contents should not change until it is unlocked by the
  -- caller.
  renderFrame :: Owned (Rc (Frame b)) -> IO (Owned (Rc (RenderedFrame b)))

  -- | Look up the identity for an `ExportBuffer`.
  --
  -- This should return the id of an internal buffer that the frame has been
  -- rendered to.
  getExportBufferId :: HasCallStack => RenderedFrame b -> STMc NoRetry '[DisposedException] (ExportBufferId b)

  -- | Called by the `Surface`-implementation when a buffer should be mapped
  -- from the wayland client to the wayland server. This usually shares memory
  -- from the client to the server.
  --
  -- This targets the internal buffer that is currently in use by the
  -- `RenderedFrame` (this should match `getExportBufferId`). Even after
  -- disposing the rendered frame, the buffer stays mapped to the server.
  --
  -- The caller takes ownership of the resulting @wl_buffer@ and will attach the
  -- event handler.
  --
  -- The @RenderedFrame@ argument is owned by the caller and must not be
  -- disposed by the callee.
  exportWlBuffer :: BackendClientBufferManager b -> RenderedFrame b -> IO (NewObject 'Client Interface_wl_buffer)

  syncExportBuffer :: RenderedFrame b -> IO ()

  -- | TODO rewrite documentation for future-based reimplementation of this function.
  --
  -- Attach a callback to a buffer. The callback must be called when the
  -- buffer that is backing the `RenderedFrame` argument is released.
  --
  -- Usually an exported buffer can be reused. When the underlying texture or
  -- buffer is destroyed by the rendering engine (or another buffer source), the
  -- buffer has to be unmapped from the wayland server. This event is used as a
  -- signal that the @wl_buffer@ that belongs to the texture/buffer should be
  -- destroyed.
  --
  -- If the buffer can't be reused the event should be called when the
  -- `RenderedFrame` is disposed.
  --
  -- The `RenderedFrame` argument is owned by the caller and must not be disposed by
  -- the callee.
  getExportBufferDestroyedFuture :: RenderedFrame b -> STMc NoRetry '[] (Future '[] ())


getBackendClientBufferManager :: forall b. ClientBufferBackend b => WaylandClient b -> STMc NoRetry '[SomeException] (BackendClientBufferManager b)
getBackendClientBufferManager client =
  getClientComponent (newBackendClientBufferManager @b client) client
