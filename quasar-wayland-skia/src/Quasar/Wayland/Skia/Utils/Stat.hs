{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Skia.Utils.Stat (
  DevT(..),
  statDevT,
) where

import Data.ByteString (ByteString, packCStringLen)
import Foreign
import Foreign.C
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude

C.include "<stdint.h>"
C.include "<sys/stat.h>"

newtype DevT = DevT ByteString
  -- TODO custom show instance to snow major/minor
  deriving Show

-- | Size of the kernel @dev_t@ struct.
devTSize :: Int
devTSize = fromIntegral [CU.pure|int { sizeof(dev_t) }|]

-- | Returns the @dev_t@ device id of a special file.
--
-- See @man 2 stat@ for details.
statDevT :: FilePath -> IO DevT
statDevT path =
  withCString path \pathPtr ->
    allocaBytes devTSize \(devtPtr :: Ptr CChar) -> do
      -- A void pointer is used to pass the dev_t pointer to C, since
      -- sizeof(dev_t) is defined by the kernel implementation and dev_t has no
      -- Haskell representation.
      let devtVoidPtr = castPtr devtPtr
      throwErrnoIfMinus1_ "stat" [CU.block|int {
        struct stat statdata;
        if (stat($(char* pathPtr), &statdata) == -1) {
          return -1;
        }
        *((dev_t*)($(void* devtVoidPtr))) = statdata.st_rdev;
        return 0;
      }|]
      DevT <$> packCStringLen (devtPtr, devTSize)
