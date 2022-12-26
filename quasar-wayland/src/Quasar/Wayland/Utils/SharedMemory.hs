{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Utils.SharedMemory (
  memfdCreate,
  mmapReadWrite,
  mmapReadOnly,
) where

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

mmapReadWrite :: CSize -> Fd -> IO (ForeignPtr Word8)
mmapReadWrite size (Fd fd) = do
  ptr <- fmap intPtrToPtr . throwErrnoIfMinus1 "mmap" $
    ptrToIntPtr <$> [CU.exp|void*{
      mmap(NULL, $(size_t size), PROT_READ | PROT_WRITE, MAP_SHARED, $(int fd), 0)
    }|]
  FC.newForeignPtr ptr (munmap ptr size)

mmapReadOnly :: CSize -> Fd -> IO (ForeignPtr Word8)
mmapReadOnly size (Fd fd) = do
  ptr <- fmap intPtrToPtr . throwErrnoIfMinus1 "mmap" $
    ptrToIntPtr <$> [CU.exp|void*{
      mmap(NULL, $(size_t size), PROT_READ, MAP_PRIVATE, $(int fd), 0)
    }|]
  FC.newForeignPtr ptr (munmap ptr size)

munmap :: Ptr a -> CSize -> IO ()
munmap (castPtr -> ptr) size =
  throwErrnoIfMinus1_ "munmap" $
    [CU.exp| int { munmap($(void* ptr), $(size_t size)) } |]
