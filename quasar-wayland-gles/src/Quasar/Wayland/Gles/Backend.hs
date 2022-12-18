module Quasar.Wayland.Gles.Backend (
  GlesBackend,
  GlesBuffer(..),

  -- * Client
  ClientDmabufSingleton,
  newClientDmabufSingleton,
  getClientDmabufSingleton,
  awaitSupportedFormats,
) where

import Control.Monad.Catch
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Client
import Quasar.Wayland.Client.Surface
import Quasar.Wayland.Gles.Dmabuf
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Surface
import System.Posix.Types (Fd)

data GlesBackend

instance BufferBackend GlesBackend where
  type BufferStorage GlesBackend = GlesBuffer

data GlesBuffer = GlesBuffer {
  dmabuf :: Dmabuf,
  width :: Int32,
  height :: Int32
}


instance ClientBufferBackend GlesBackend where
  type ClientBufferManager GlesBackend = ClientDmabufSingleton
  newClientBufferManager = newClientDmabufSingleton
  exportWlBuffer manager buffer = exportGlesWlBuffer manager buffer.storage



-- * Client

data ClientDmabufSingleton = ClientDmabufSingleton {
  zwpLinuxDmabuf :: Object 'Client Interface_zwp_linux_dmabuf_v1,
  formatsComplete :: Future (),
  dmabufFormats :: TVar (Set DrmFormat),
  dmabufModifiers :: TVar (Set (DrmFormat, DrmModifier))
}

getClientDmabufSingleton :: WaylandClient -> STM ClientDmabufSingleton
getClientDmabufSingleton client =
  getClientComponent (newClientDmabufSingleton client) client

newClientDmabufSingleton :: WaylandClient -> STM ClientDmabufSingleton
newClientDmabufSingleton client = do
  -- NOTE v3 for now
  zwpLinuxDmabuf <- bindSingleton @Interface_zwp_linux_dmabuf_v1 client.registry 3
  dmabufFormats <- newTVar mempty
  dmabufModifiers <- newTVar mempty
  zwpLinuxDmabuf `setEventHandler` EventHandler_zwp_linux_dmabuf_v1 {
    format = addSupportedFormat dmabufFormats,
    modifier = addSupportedModifier dmabufFormats dmabufModifiers
  }
  -- Format events are forbidden from version 4 up, so no sync is required
  formatsComplete <-
    if zwpLinuxDmabuf.version < 4
      then client.sync
      else pure (pure ())

  pure ClientDmabufSingleton {
    zwpLinuxDmabuf,
    formatsComplete,
    dmabufFormats,
    dmabufModifiers
  }

addSupportedFormat :: TVar (Set DrmFormat) -> Word32 -> STM ()
addSupportedFormat var fourcc = modifyTVar var (Set.insert (DrmFormat fourcc))

addSupportedModifier :: TVar (Set DrmFormat) -> TVar (Set (DrmFormat, DrmModifier)) -> Word32 -> Word32 -> Word32 -> STM ()
addSupportedModifier formats modifiers fourcc modifierLo modifierHi = do
  -- I am not sure if formats should only be added under certain circumstances,
  -- since modifiers are not communicated to the client in v1/v2.
  -- e.g. when (modifier == drmModNone)?
  addSupportedFormat formats fourcc

  modifyTVar modifiers (Set.insert (DrmFormat fourcc, modifier))
  where
    modifier = createDrmModifier modifierLo modifierHi

awaitSupportedFormats :: ClientDmabufSingleton -> IO ([DrmFormat], [(DrmFormat, DrmModifier)])
awaitSupportedFormats dmabuf = do
  await dmabuf.formatsComplete
  formats <- readTVarIO dmabuf.dmabufFormats
  modifiers <- readTVarIO dmabuf.dmabufModifiers
  pure (Set.toList formats, Set.toList modifiers)

exportGlesWlBuffer :: ClientDmabufSingleton -> GlesBuffer -> STM (NewObject 'Client Interface_wl_buffer)
exportGlesWlBuffer dmabufSingleton buffer = do
  bufferParams <- dmabufSingleton.zwpLinuxDmabuf.create_params
  setMessageHandler bufferParams EventHandler_zwp_linux_buffer_params_v1 {
    created = \_ -> traceM "Error: Buffer created even though create_immed was used",
    failed = traceM "Buffer creation failed"
  }

  forM_ (zip [0,1..] buffer.dmabuf.planes) \(i, plane) ->
    bufferParams.add plane.fd i plane.offset plane.stride plane.modifier.hi plane.modifier.lo

  bufferParams.create_immed buffer.width buffer.height buffer.dmabuf.format.fourcc 0
