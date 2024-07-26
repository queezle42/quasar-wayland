{-# LANGUAGE TemplateHaskell #-}

module Quasar.Wayland.Utils.SharedMemory (
  memfdCreate,

  MmapMode(..),
  mmap,
  withMmap,

  -- ** Using raw file descriptor
  memfdCreateFd,
  mmapFd,
  withMmapFd,

  -- ** Using a Disposer for releasing the memory
  mmapDisposer,
) where

import Control.Exception (bracket, mask_)
import Foreign
import Foreign.C
import Foreign.Concurrent qualified as FC
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Quasar.Disposer
import Quasar.Exceptions.ExceptionSink (loggingExceptionSink)
import Quasar.Prelude
import Quasar.Wayland.Utils.InlineC
import Quasar.Wayland.Utils.SharedFd
import System.Posix.Types (Fd(Fd), COff(..))

C.context ctx

C.verbatim "#define _GNU_SOURCE"
C.include "<unistd.h>"
C.include "<stdint.h>"
C.include "<sys/mman.h>"

data MmapMode = MmapReadOnly | MmapReadWrite

memfdCreate :: COff -> IO SharedFd
memfdCreate size = mask_ do
  newSharedFd =<< memfdCreateFd size

memfdCreateFd :: COff -> IO Fd
memfdCreateFd size = Fd <$> throwErrnoIfMinus1 "memfd_create/ftruncate"
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

mmap :: MmapMode -> SharedFd -> CSize -> IO (ForeignPtr Word8)
mmap mode sfd size = withSharedFd sfd \fd -> mmapFd mode fd size

mmapFd :: MmapMode -> Fd -> CSize -> IO (ForeignPtr Word8)
mmapFd mode fd size = mask_ do
  ptr <- mmapPtr mode fd size
  FC.newForeignPtr ptr (munmapPtr ptr size)

mmapDisposer ::
  HasCallStack =>
  MmapMode -> SharedFd -> CSize -> IO (Disposer, Ptr Word8)
mmapDisposer mode sfd size = withSharedFd sfd \fd -> do
  ptr <- mmapPtr mode fd size
  d <- atomicallyC $ newDisposer (munmapPtr ptr size) loggingExceptionSink
  pure (d, ptr)

withMmap :: MmapMode -> SharedFd -> CSize -> (Ptr Word8 -> IO b) -> IO b
withMmap mode sfd size =
  bracket
    (withSharedFd sfd \fd -> mmapPtr mode fd size)
    (\ptr -> munmapPtr ptr size)

withMmapFd :: MmapMode -> Fd -> CSize -> (Ptr Word8 -> IO b) -> IO b
withMmapFd mode fd size =
  bracket
    (mmapPtr mode fd size)
    (\ptr -> munmapPtr ptr size)

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
