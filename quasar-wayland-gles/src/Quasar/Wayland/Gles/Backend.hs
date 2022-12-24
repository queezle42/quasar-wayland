module Quasar.Wayland.Gles.Backend (
  GlesBackend,
  GlesBuffer(..),
  getDmabuf,

  -- * Client
  ClientDmabufSingleton,
  newClientDmabufSingleton,
  getClientDmabufSingleton,
  awaitSupportedFormats,

  -- * Server
  dmabufGlobal,
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
  dmabuf :: Dmabuf
}

getDmabuf :: GlesBuffer -> Dmabuf
getDmabuf (GlesBuffer dmabuf) = dmabuf


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

  bufferParams.create_immed buffer.dmabuf.width buffer.dmabuf.height buffer.dmabuf.format.fourcc 0


-- * Server

type ServerDmabufParams = TVar ServerDmabufParamsState
type ServerDmabufParamsState = Maybe (Map Word32 DmabufPlane)

dmabufGlobal :: [DrmFormat] -> [(DrmFormat, DrmModifier)] -> Global
dmabufGlobal supportedFormats supportedModifiers =
  createGlobal @Interface_zwp_linux_dmabuf_v1 3 initialize
  where
    initialize :: NewObject 'Server Interface_zwp_linux_dmabuf_v1 -> STM ()
    initialize wlLinuxDmabuf = do
      wlLinuxDmabuf `setRequestHandler` dmabufHandler
      forM_ supportedFormats \format -> do
        wlLinuxDmabuf.format format.fourcc

      when (wlLinuxDmabuf.version == 3) do
        forM_ supportedModifiers \(format, modifier) -> do
          wlLinuxDmabuf.modifier format.fourcc modifier.hi modifier.lo

    dmabufHandler :: RequestHandler_zwp_linux_dmabuf_v1
    dmabufHandler =
      RequestHandler_zwp_linux_dmabuf_v1 {
        destroy = pure (),
        create_params = initializeDmabufParams,
        -- Not required in version 3
        get_default_feedback = undefined,
        get_surface_feedback = undefined
      }

initializeDmabufParams :: NewObject 'Server Interface_zwp_linux_buffer_params_v1 -> STM ()
initializeDmabufParams wlDmabufParams = do
  var <- newTVar (Just mempty)
  wlDmabufParams `setRequestHandler` dmabufParamsHandler var
  where
    dmabufParamsHandler :: ServerDmabufParams -> RequestHandler_zwp_linux_buffer_params_v1
    dmabufParamsHandler var = RequestHandler_zwp_linux_buffer_params_v1 {
      destroy = pure (),
      add = addDmabufPlane var,
      create = \_width _height _format _flags -> undefined,
      create_immed = initializeDmabufBuffer var
    }

addDmabufPlane :: ServerDmabufParams -> Fd -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> STM ()
addDmabufPlane var fd planeIndex offset stride modifierHi modifierLo = do
  readTVar var >>= \case
    Nothing -> throwM $ userError "zwp_linux_buffer_params_v1::error.already_used: the dmabuf_batch object has already been used to create a wl_buffer"
    Just planes ->
      writeTVar var . Just =<< Map.alterF addPlane planeIndex planes
  where
    modifier :: DrmModifier
    modifier = createDrmModifier modifierHi modifierLo

    addPlane :: Maybe DmabufPlane -> STM (Maybe DmabufPlane)
    addPlane Nothing = pure (Just (DmabufPlane {fd, offset, stride, modifier}))
    addPlane (Just _) = throwM $ userError "zwp_linux_buffer_params_v1::error.plane_set: the plane index was already set"

importDmabufServerBuffer :: ServerDmabufParams -> Int32 -> Int32 -> Word32 -> Word32 -> STM Dmabuf
importDmabufServerBuffer var width height (DrmFormat -> format) _flags@0 = do
  readTVar var >>= \case
    Nothing -> throwM $ userError "zwp_linux_buffer_params_v1::error.already_used: the dmabuf_batch object has already been used to create a wl_buffer"
    Just planesMap -> do
      planes <-
        forM (zip [0,1..] (Map.toList planesMap)) \(i, (clientIndex, plane)) -> do
          if i == clientIndex
            then pure plane
            else throwM $ userError "zwp_linux_buffer_params_v1::error.incomplete: missing or too many planes to create a buffer"
      pure $ Dmabuf { width, height, format, planes }
importDmabufServerBuffer _ _ _ _ _ = throwM $ userError "zwp_linux_buffer_params_v1 flags (inverted, interlaced) are not supported"

initializeDmabufBuffer :: ServerDmabufParams -> NewObject 'Server Interface_wl_buffer -> Int32 -> Int32 -> Word32 -> Word32 -> STM ()
initializeDmabufBuffer var wlBuffer width height format flags = do
  dmabuf <- importDmabufServerBuffer var width height format flags
  -- Second arg is the destroy callback
  buffer <- newBuffer @GlesBackend (GlesBuffer dmabuf) (pure ())
  initializeWlBuffer wlBuffer buffer
