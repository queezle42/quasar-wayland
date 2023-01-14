module Quasar.Wayland.Gles.Dmabuf (
  Dmabuf(..),
  DmabufPlane(..),
  DrmFormat(..),
  DrmModifier(..),
  createDrmModifier,
  drmModNone,
  drmModInvalid,

  DmabufFeedback,
  DmabufFeedbackTranche,
  DmabufFormatTable,
  readFormatTableFd,
  writeFormatTableFd,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (w2c)
import Foreign
import GHC.Records
import Quasar.Prelude
import Quasar.Wayland.Utils.SharedFd
import Quasar.Wayland.Utils.SharedMemory

data Dmabuf = Dmabuf {
  width :: Int32,
  height :: Int32,
  format :: DrmFormat,
  planes :: [DmabufPlane]
}
  deriving Show

data DmabufPlane = DmabufPlane {
  fd :: SharedFd,
  stride :: Word32,
  offset :: Word32,
  modifier :: DrmModifier
}
  deriving Show


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


data DmabufFeedback = DmabufFeedback {
  mainDevice :: ByteString,
  tranches :: [DmabufFeedbackTranche]
}

data DmabufFeedbackTranche = DmabufFeedbackTranche {
  targetDevice :: ByteString,
  formatTable :: DmabufFormatTable,
  formats :: [Word16],
  scanout :: Bool
}

type DmabufFormatTable = [(DrmFormat, DrmModifier)]

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

  let count = size `div` 16

  withMmap MmapReadOnly fd (fromIntegral size) \ptr ->
    forM [0,16..] \offset -> do
      format <- peekByteOff ptr offset
      modifier <- peekByteOff ptr (offset + 8)
      pure (format, modifier)

-- | Write a dmabuf format table (as documented in the
-- @linux-dmabuf-unstable-v1@ wayland protocol) to a shared memory file.
--
-- The caller is responsible for managing the new file descriptor.
writeFormatTableFd :: DmabufFormatTable -> IO (SharedFd, Word32)
writeFormatTableFd table = do
  fd <- memfdCreate (fromIntegral size)

  withMmap MmapReadWrite fd (fromIntegral size) \ptr ->
    forM_ (zip [0,16..] table) \(offset, (format, modifier)) -> do
      pokeByteOff ptr offset format
      pokeByteOff ptr (offset + 8) modifier

  pure (fd, size)

  where
    size :: Word32
    size = fromIntegral (length table * 16)
