{-# LANGUAGE CPP #-}

-- |This module provides functionality for flushing the operating system buffer associated with
-- a file handle.
--
-- On Windows, the FlushFileBuffers API call is used.
-- <https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-flushfilebuffers>
--
-- On Mac OS X, fcntl is called with F_FULLFSYNC.
-- <https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/fcntl.2.html>
--
-- On Linux (or other platforms), fdatasync (where supported) or fsync is called.
-- <https://man7.org/linux/man-pages/man2/fsync.2.html>
module Concordium.GlobalState.Persistent.BlobStore.Flush (hFlushOS) where

import Control.Concurrent
import Control.Monad
import Foreign.C
import GHC.IO.FD
import GHC.IO.Handle.FD (handleToFd)
import System.IO

#if defined(mingw32_HOST_OS)
---------- Windows ------------
import GHC.IO.Exception
import GHC.Windows

foreign import ccall safe "FlushFileBuffers" flushFileBuffers :: CInt -> IO CInt

#elif defined(darwin_HOST_OS)
----------- MacOS ------------
#include <fcntl.h>

foreign import ccall safe "fcntl" fcntl :: CInt -> CInt -> IO CInt
-------- Linux/other ---------
#else

#include <unistd.h>
#if _POSIX_SYNCHRONIZED_IO > 0
foreign import ccall safe "fdatasync" fsync :: CInt -> IO CInt
#else
foreign import ccall safe "fsync" fsync :: CInt -> IO CInt
#endif

#endif

osFlush :: Handle -> IO ()
osFlush h = do
    fd <- handleToFd h

#if defined(mingw32_HOST_OS)
    -- Windows
    when (fdIsSocket_ fd /= 0) $ ioException $
        IOError (Just h) IllegalOperation "hFlushOS" "is a socket" Nothing Nothing
    res <- flushFileBuffers (fdFD fd)
    when (res /= 0) $ throwGetLastError "hFlushOS"
#elif defined(darwin_HOST_OS)
    -- MacOS
    res <- fcntl (fdFD fd) (#const F_FULLFSYNC)
    when (res == -1) $ throwErrno "hFlushOS"
#else
    -- Linux/other
    res <- fsync (fdFD fd)
    when (res /= 0) $ throwErrno "hFlushOS"
#endif

-- |Flush a file handle, including the operating system buffers.
hFlushOS :: Handle -> IO ()
hFlushOS h = runInBoundThread $ do
    hFlush h
    osFlush h