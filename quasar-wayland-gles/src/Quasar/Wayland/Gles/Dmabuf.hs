module Quasar.Wayland.Gles.Dmabuf (
  Dmabuf(..),
  DmabufPlane(..),
  DrmFormat(..),
  DrmModifier(..),
  createDrmModifier,
  drmModNone,
  drmModInvalid,

  CompiledDmabufFeedback(..),
  CompiledDmabufFeedbackTranche(..),
  DmabufFormatTable,
  compileDmabufFeedback,
  readFormatTableFd,
  writeFormatTableFd,
) where

import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal (w2c)
import Foreign
import GHC.Records
import Quasar.Prelude
import Quasar.Wayland.Gles.Utils.Stat (DevT(..))
import Quasar.Wayland.Utils.SharedFd
import Quasar.Wayland.Utils.SharedMemory
import Quasar.Resources (Disposable (getDisposer))

data Dmabuf = Dmabuf {
  width :: Int32,
  height :: Int32,
  format :: DrmFormat,
  planes :: [DmabufPlane]
}
  deriving Show

instance Disposable Dmabuf where
  getDisposer dmabuf = foldMap getDisposer dmabuf.planes

data DmabufPlane = DmabufPlane {
  fd :: SharedFd,
  stride :: Word32,
  offset :: Word32,
  modifier :: DrmModifier
}
  deriving Show

instance Disposable DmabufPlane where
  getDisposer plane = getDisposer plane.fd


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
