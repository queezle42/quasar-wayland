module Quasar.Wayland.Dmabuf (
  -- * Dmabuf
  Dmabuf(..),
  DmabufPlane(..),
  DrmFormat(..),
  DrmModifier(..),
  createDrmModifier,
  drmModNone,
  drmModInvalid,

  -- ** Feedback
  CompiledDmabufFeedback(..),
  CompiledDmabufFeedbackTranche(..),
  DmabufFormatTable,
  compileDmabufFeedback,
  readFormatTableFd,
  writeFormatTableFd,

  -- ** Client
  ClientDmabufSingleton,
  newClientDmabufSingleton,
  getClientDmabufSingleton,
  awaitSupportedFormats,
  consumeDmabufExportWlBuffer,
  sharedDmabufExportWlBuffer,

  -- ** Server
  IsDmabufBackend,
  dmabufGlobal,
) where

import Control.Monad.Catch
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Foreign
import GHC.Records
import Quasar.Disposer
import Quasar.Disposer.Rc
import Quasar.Future
import Quasar.Prelude
import Quasar.Wayland.Backend
import Quasar.Wayland.Client
import Quasar.Wayland.Protocol
import Quasar.Wayland.Protocol.Generated
import Quasar.Wayland.Server.Registry
import Quasar.Wayland.Server.Surface
import Quasar.Wayland.Utils.SharedFd
import Quasar.Wayland.Utils.SharedMemory
import Quasar.Wayland.Utils.Stat (DevT(..))

type IsDmabufBackend b = IsBufferBackend Dmabuf b

data Dmabuf = Dmabuf {
  width :: Int32,
  height :: Int32,
  format :: DrmFormat,
  planes :: [DmabufPlane]
}
  deriving Show

instance ToFuture '[] () Dmabuf where
  toFuture dmabuf = toFuture (dmabufDisposer dmabuf)

dmabufDisposer :: Dmabuf -> Disposer
dmabufDisposer dmabuf = foldMap dmabufPlaneDisposer dmabuf.planes

ownedDmabuf :: Dmabuf -> Owned Dmabuf
ownedDmabuf dmabuf = Owned (dmabufDisposer dmabuf) dmabuf

data DmabufPlane = DmabufPlane {
  fd :: SharedFd,
  stride :: Word32,
  offset :: Word32,
  modifier :: DrmModifier
}
  deriving Show

dmabufPlaneDisposer :: DmabufPlane -> Disposer
dmabufPlaneDisposer plane = getDisposer plane.fd


newtype DrmFormat = DrmFormat { fourcc :: Word32 }
  deriving (Eq, Ord, Storable)

instance Show DrmFormat where
  show (DrmFormat fourcc) = [a, b, c, d]
    where
      -- Could parse formats from drm_fourcc.h for a better description
      a = w2c (fromIntegral fourcc)
      b = w2c (fromIntegral (fourcc `shiftR` 8))
      c = w2c (fromIntegral (fourcc `shiftR` 16))
      d = w2c (fromIntegral (fourcc `shiftR` 24))

newtype DrmModifier = DrmModifier Word64
  deriving (Eq, Ord, Show, Storable)

instance HasField "lo" DrmModifier Word32 where
  getField (DrmModifier value) = fromIntegral value

instance HasField "hi" DrmModifier Word32 where
  getField (DrmModifier value) = fromIntegral (value `shiftR` 32)

createDrmModifier :: Word32 -> Word32 -> DrmModifier
createDrmModifier hi lo = DrmModifier ((fromIntegral hi `shiftL` 32) .|. fromIntegral lo)

-- | DRM_FORMAT_MOD_NONE
drmModNone :: DrmModifier
drmModNone = DrmModifier 0

-- | DRM_FORMAT_MOD_INVALID
drmModInvalid :: DrmModifier
drmModInvalid = DrmModifier 0x00ffffffffffffff


data CompiledDmabufFeedback = CompiledDmabufFeedback {
  mainDevice :: DevT,
  formatTableFd :: SharedFd,
  formatTableSize :: Word32,
  tranches :: [CompiledDmabufFeedbackTranche]
}

data CompiledDmabufFeedbackTranche = CompiledDmabufFeedbackTranche {
  targetDevice :: DevT,
  formats :: ByteString,
  scanout :: Bool
}

type DmabufFormatTable = [(DrmFormat, DrmModifier)]

compileDmabufFeedback :: DevT -> (DevT, DmabufFormatTable, Bool) -> IO CompiledDmabufFeedback
compileDmabufFeedback mainDevice (targetDevice, formatTable, scanout) = do
  (formatTableFd, formatTableSize) <- writeFormatTableFd formatTable

  let
    -- From linux-dmabuf-unstable-v1 (version 4):
    -- "Each index is a 16-bit unsigned integer in native endianness."
    compiledFormatIndices =
      runPut (mconcat (putWord16host <$> take (length formatTable) [0..]))

    tranche = CompiledDmabufFeedbackTranche {
      targetDevice,
      formats = BS.toStrict compiledFormatIndices,
      scanout
    }

  pure CompiledDmabufFeedback {
    mainDevice,
    formatTableFd,
    formatTableSize,
    tranches = [tranche]
  }

-- | Read a dmabuf format table (as documented in the @linux-dmabuf-unstable-v1@
-- wayland protocol) from a file descriptor.
--
-- Does not close the file descriptor.
readFormatTableFd :: SharedFd -> Word32 -> IO DmabufFormatTable
readFormatTableFd fd size = do
  -- From 'linux-dmabuf-unstable-v1' (version 4):
  -- "The table contains a tightly packed array of consecutive format + modifier
  -- pairs. Each pair is 16 bytes wide. It contains a format as a 32-bit
  -- unsigned integer, followed by 4 bytes of unused padding, and a modifier as
  -- a 64-bit unsigned integer. The native endianness is used."

  let count = fromIntegral (size `div` 16)

  withMmap MmapReadOnly fd (fromIntegral size) \ptr ->
    forM (take count [0,16..]) \offset -> do
      format <- peekByteOff ptr offset
      modifier <- peekByteOff ptr (offset + 8)
      pure (format, modifier)


-- | Write a dmabuf format table (as documented in the
-- @linux-dmabuf-unstable-v1@ wayland protocol) to a shared memory file.
--
-- The caller is responsible for managing the new file descriptor.
writeFormatTableFd :: DmabufFormatTable -> IO (SharedFd, Word32)
writeFormatTableFd table = do
  let size = fromIntegral (length table * 16)

  fd <- memfdCreate (fromIntegral size)

  withMmap MmapReadWrite fd (fromIntegral size) \ptr ->
    forM_ (zip [0,16..] table) \(offset, (format, modifier)) -> do
      pokeByteOff ptr offset format
      pokeByteOff ptr (offset + 8) modifier

  pure (fd, size)



-- * Client

data ClientDmabufSingleton = ClientDmabufSingleton {
  zwpLinuxDmabuf :: Object 'Client Interface_zwp_linux_dmabuf_v1,
  formatsComplete :: FutureEx '[SomeException] (),
  dmabufFormats :: TVar (Set DrmFormat),
  dmabufModifiers :: TVar (Set (DrmFormat, DrmModifier))
}

getClientDmabufSingleton :: WaylandClient b -> STMc NoRetry '[SomeException] ClientDmabufSingleton
getClientDmabufSingleton client =
  getClientComponent (newClientDmabufSingleton client) client

newClientDmabufSingleton :: WaylandClient b -> STMc NoRetry '[SomeException] ClientDmabufSingleton
newClientDmabufSingleton client = do
  zwpLinuxDmabuf <- bindSingleton @Interface_zwp_linux_dmabuf_v1 client.registry 4
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

addSupportedFormat :: TVar (Set DrmFormat) -> Word32 -> STMc NoRetry '[SomeException] ()
addSupportedFormat var fourcc = modifyTVar var (Set.insert (DrmFormat fourcc))

addSupportedModifier :: TVar (Set DrmFormat) -> TVar (Set (DrmFormat, DrmModifier)) -> Word32 -> Word32 -> Word32 -> STMc NoRetry '[SomeException] ()
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

-- | Takes ownership of the `Dmabuf`.
consumeDmabufExportWlBuffer :: ClientDmabufSingleton -> Dmabuf -> STMc NoRetry '[SomeException] (NewObject 'Client Interface_wl_buffer)
consumeDmabufExportWlBuffer dmabufSingleton dmabuf = do
  bufferParams <- dmabufSingleton.zwpLinuxDmabuf.create_params
  setMessageHandler bufferParams EventHandler_zwp_linux_buffer_params_v1 {
    created = \_ -> traceM "Error: Buffer created even though create_immed was used",
    failed = traceM "Buffer creation failed"
  }

  forM_ (zip [0,1..] dmabuf.planes) \(i, plane) -> do
    bufferParams.add plane.fd i plane.offset plane.stride plane.modifier.hi plane.modifier.lo

  bufferParams.create_immed dmabuf.width dmabuf.height dmabuf.format.fourcc 0

sharedDmabufExportWlBuffer :: ClientDmabufSingleton -> Dmabuf -> STMc NoRetry '[SomeException] (NewObject 'Client Interface_wl_buffer)
sharedDmabufExportWlBuffer dmabufSingleton dmabuf = do
  bufferParams <- dmabufSingleton.zwpLinuxDmabuf.create_params
  setMessageHandler bufferParams EventHandler_zwp_linux_buffer_params_v1 {
    created = \_ -> traceM "Error: Buffer created even though create_immed was used",
    failed = traceM "Buffer creation failed"
  }

  forM_ (zip [0,1..] dmabuf.planes) \(i, plane) -> do
    fd <- duplicateSharedFd plane.fd
    bufferParams.add fd i plane.offset plane.stride plane.modifier.hi plane.modifier.lo

  bufferParams.create_immed dmabuf.width dmabuf.height dmabuf.format.fourcc 0


-- * Server

type ServerDmabufParams = TVar ServerDmabufParamsState
type ServerDmabufParamsState = Maybe (Map Word32 DmabufPlane)

dmabufGlobal ::
  forall b. IsDmabufBackend b =>
  (b -> ([DrmFormat], DmabufFormatTable, CompiledDmabufFeedback)) ->
  Global b
dmabufGlobal getFormats =
  createBackendGlobal @Interface_zwp_linux_dmabuf_v1 4 initialize
  where
    initialize :: Rc b -> NewObject 'Server Interface_zwp_linux_dmabuf_v1 -> STMc NoRetry '[SomeException] ()
    initialize origBackendRc wlLinuxDmabuf = do
      (Owned backendDisposer backendRc) <- cloneRc origBackendRc
      backend <- readRc backendRc
      attachFinalizer wlLinuxDmabuf (disposeEventually_ backendDisposer)

      let (version1Formats, version3FormatTable, feedback) = getFormats backend

      wlLinuxDmabuf `setRequestHandler` dmabufHandler backendRc feedback

      when (wlLinuxDmabuf.version <= 3) do
        forM_ version1Formats \format -> do
          wlLinuxDmabuf.format format.fourcc

        when (wlLinuxDmabuf.version == 3) do
          forM_ version3FormatTable \(format, modifier) -> do
            wlLinuxDmabuf.modifier format.fourcc modifier.hi modifier.lo

    dmabufHandler :: Rc b -> CompiledDmabufFeedback -> RequestHandler_zwp_linux_dmabuf_v1
    dmabufHandler backend feedback =
      RequestHandler_zwp_linux_dmabuf_v1 {
        destroy = pure (),
        create_params = initializeDmabufParams backend,
        get_default_feedback = initializeDmabufFeedback feedback,
        -- NOTE "If the surface is destroyed before the wp_linux_dmabuf_feedback
        -- object, the feedback object becomes inert."
        -- (not relevant for static feedback)
        get_surface_feedback = \wlFb _surface -> initializeDmabufFeedback feedback wlFb
      }

initializeDmabufFeedback :: CompiledDmabufFeedback -> NewObject 'Server Interface_zwp_linux_dmabuf_feedback_v1 -> STMc NoRetry '[SomeException] ()
initializeDmabufFeedback feedback wlFeedback = do
  wlFeedback `setRequestHandler` RequestHandler_zwp_linux_dmabuf_feedback_v1 {
    destroy = pure ()
  }
  sendDmabufFeedback feedback wlFeedback

sendDmabufFeedback :: CompiledDmabufFeedback -> Object 'Server Interface_zwp_linux_dmabuf_feedback_v1 -> STMc NoRetry '[SomeException] ()
sendDmabufFeedback feedback wlFeedback = do
  let (DevT mainDevice) = feedback.mainDevice
  wlFeedback.main_device mainDevice
  fd <- duplicateSharedFd feedback.formatTableFd
  wlFeedback.format_table fd feedback.formatTableSize

  forM_ feedback.tranches \tranche -> do
    let (DevT targetDevice) = tranche.targetDevice
    wlFeedback.tranche_target_device targetDevice
    wlFeedback.tranche_formats tranche.formats
    when tranche.scanout $ wlFeedback.tranche_flags 1
    wlFeedback.tranche_done

  wlFeedback.done

initializeDmabufParams :: forall b. IsDmabufBackend b => Rc b -> NewObject 'Server Interface_zwp_linux_buffer_params_v1 -> STMc NoRetry '[SomeException] ()
initializeDmabufParams backend wlDmabufParams = do
  var <- newTVar (Just mempty)
  wlDmabufParams `setRequestHandler` dmabufParamsHandler var
  where
    dmabufParamsHandler :: ServerDmabufParams -> RequestHandler_zwp_linux_buffer_params_v1
    dmabufParamsHandler var = RequestHandler_zwp_linux_buffer_params_v1 {
      destroy = pure (),
      add = addDmabufPlane var,
      create = \_width _height _format _flags -> undefined,
      create_immed = initializeDmabufBuffer backend var
    }

addDmabufPlane :: ServerDmabufParams -> SharedFd -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> STMc NoRetry '[SomeException] ()
addDmabufPlane var fd planeIndex offset stride modifierHi modifierLo = do
  readTVar var >>= \case
    Nothing -> throwM $ userError "zwp_linux_buffer_params_v1::error.already_used: the dmabuf_batch object has already been used to create a wl_buffer"
    Just planes ->
      writeTVar var . Just =<< Map.alterF addPlane planeIndex planes
  where
    modifier :: DrmModifier
    modifier = createDrmModifier modifierHi modifierLo

    addPlane :: Maybe DmabufPlane -> STMc NoRetry '[SomeException] (Maybe DmabufPlane)
    addPlane Nothing = pure (Just (DmabufPlane {fd, offset, stride, modifier}))
    addPlane (Just _) = throwM $ userError "zwp_linux_buffer_params_v1::error.plane_set: the plane index was already set"

newServerDmabuf :: ServerDmabufParams -> Int32 -> Int32 -> Word32 -> Word32 -> STMc NoRetry '[SomeException] (Owned Dmabuf)
newServerDmabuf var width height (DrmFormat -> format) _flags@0 = do
  readTVar var >>= \case
    Nothing -> throwM $ userError "zwp_linux_buffer_params_v1::error.already_used: the dmabuf_batch object has already been used to create a wl_buffer"
    Just planesMap -> do
      planes <-
        forM (zip [0,1..] (Map.toList planesMap)) \(i, (clientIndex, plane)) -> do
          if i == clientIndex
            then pure plane
            else throwM $ userError "zwp_linux_buffer_params_v1::error.incomplete: missing or too many planes to create a buffer"
      pure $ ownedDmabuf Dmabuf { width, height, format, planes }
newServerDmabuf _ _ _ _ _ = throwM $ userError "zwp_linux_buffer_params_v1 flags (inverted, interlaced) are not supported"

initializeDmabufBuffer :: forall b. IsDmabufBackend b => Rc b -> ServerDmabufParams -> NewObject 'Server Interface_wl_buffer -> Int32 -> Int32 -> Word32 -> Word32 -> STMc NoRetry '[SomeException] ()
initializeDmabufBuffer backend var wlBuffer width height format flags = do
  dmabuf <- newServerDmabuf var width height format flags
  liftSTMc do
    initializeWlBuffer backend wlBuffer dmabuf
