{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Utils.SharedMemory (
  allocateShmFd,
) where

import Foreign.C.Error
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Prelude
import Quasar.Wayland.Utils.InlineC
import System.Posix.Types (COff(..), Fd(Fd))

C.context ctx

C.verbatim "#define _GNU_SOURCE"
C.include "<unistd.h>"
C.include "<sys/mman.h>"

allocateShmFd :: COff -> IO Fd
allocateShmFd size = Fd <$> throwErrnoIfMinus1 "allocateShmFd"
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
