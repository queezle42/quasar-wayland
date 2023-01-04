{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Utils.SharedMemory (
  memfdCreate,

  MmapMode(..),
  mmap,
  withMmap,
) where

import Control.Exception (bracket)
import Foreign
import Foreign.C
import Foreign.Concurrent qualified as FC
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Utils.InlineC
import System.Posix.Types (Fd(Fd), COff(..))

C.context ctx

C.verbatim "#define _GNU_SOURCE"
C.include "<unistd.h>"
C.include "<stdint.h>"
C.include "<sys/mman.h>"

data MmapMode = MmapReadOnly | MmapReadWrite

memfdCreate :: COff -> IO Fd
memfdCreate size = Fd <$> throwErrnoIfMinus1 "memfd_create/ftruncate"
  [CU.block|
    int {
      int fd = memfd_create("shm", MFD_CLOEXEC | MFD_ALLOW_SEALING);
      if (fd < 0) {
              return fd;
      }

      if (ftruncate(fd, $(off_t size)) < 0) {
              close(fd);
              return -1;
      }
      return fd;
    }
  |]

mmap :: MmapMode -> Fd -> CSize -> IO (ForeignPtr Word8)
mmap mode fd size = do
  ptr <- mmapPtr mode fd size
  FC.newForeignPtr ptr (munmapPtr ptr size)

mmapPtr :: MmapMode -> Fd -> CSize -> IO (Ptr Word8)
mmapPtr MmapReadOnly (Fd fd) size =
  fmap intPtrToPtr . throwErrnoIfMinus1 "mmap" $
    ptrToIntPtr <$> [CU.exp|void*{
      mmap(NULL, $(size_t size), PROT_READ, MAP_PRIVATE, $(int fd), 0)
    }|]
mmapPtr MmapReadWrite (Fd fd) size =
  fmap intPtrToPtr . throwErrnoIfMinus1 "mmap" $
    ptrToIntPtr <$> [CU.exp|void*{
      mmap(NULL, $(size_t size), PROT_READ | PROT_WRITE, MAP_SHARED, $(int fd), 0)
    }|]

munmapPtr :: Ptr a -> CSize -> IO ()
munmapPtr (castPtr -> ptr) size =
  throwErrnoIfMinus1_ "munmap" $
    [CU.exp| int { munmap($(void* ptr), $(size_t size)) } |]
