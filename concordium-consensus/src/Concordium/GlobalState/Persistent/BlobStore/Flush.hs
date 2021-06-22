{-# LANGUAGE CPP #-}

-- |This module provides functionality for flushing the operating system buffer associated with
-- a file handle.
module Concordium.GlobalState.Persistent.BlobStore.Flush (hFlushOS) where

import Control.Concurrent
import Control.Monad
import Foreign.C
import GHC.IO.Exception
import GHC.IO.FD
import GHC.IO.Handle.FD (handleToFd)
import System.IO

#if defined(mingw32_HOST_OS)
import GHC.Windows
#endif

#if defined(mingw32_HOST_OS)

foreign import ccall safe "FlushFileBuffers" flushFileBuffers :: CInt -> IO CInt

#else

foreign import ccall safe "fsync" fsync :: CInt -> IO CInt

#endif

osFlush :: Handle -> IO ()
osFlush h = do
    fd <- handleToFd h

#if defined(mingw32_HOST_OS)
    when (fdIsSocket_ fd /= 0) $ ioException $
        IOError (Just h) IllegalOperation "hFlushOS" "is a socket" Nothing Nothing
    res <- flushFileBuffers (fdFD fd)
    when (res /= 0) $ throwGetLastError "hFlushOS"
#else
    res <- fsync (fdFD fd)
    when (res /= 0) $ throwErrno "hFlushOS"
#endif

-- |Flush a file handle, including the operating system buffers.
hFlushOS :: Handle -> IO ()
hFlushOS h = runInBoundThread $ do
    hFlush h
    osFlush h