module Quasar.Wayland.Gles.Dmabuf (
  Dmabuf(..),
  DmabufPlane(..),
  DrmFormat(..),
  DrmModifier(..),
  createDrmModifier,
  drmModNone,
  drmModInvalid,
) where

import Data.ByteString.Internal (w2c)
import Foreign
import GHC.Records
import Quasar.Prelude
import System.Posix.Types (Fd)

data Dmabuf = Dmabuf {
  width :: Int32,
  height :: Int32,
  format :: DrmFormat,
  planes :: [DmabufPlane]
}
  deriving Show

data DmabufPlane = DmabufPlane {
  fd :: Fd,
  stride :: Word32,
  offset :: Word32,
  modifier :: DrmModifier
}
  deriving Show


newtype DrmFormat = DrmFormat { fourcc :: Word32 }
  deriving (Eq, Ord)

instance Show DrmFormat where
  show (DrmFormat fourcc) = [a, b, c, d]
    where
      -- Could parse formats from drm_fourcc.h for a better description
      a = w2c (fromIntegral fourcc)
      b = w2c (fromIntegral (fourcc `shiftR` 8))
      c = w2c (fromIntegral (fourcc `shiftR` 16))
      d = w2c (fromIntegral (fourcc `shiftR` 24))

newtype DrmModifier = DrmModifier Word64
  deriving (Eq, Ord, Show)

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
