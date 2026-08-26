{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper types and foreign imports to use V1 contract state. This is here and
--    not in Concordium.GlobalState.ContractStateV1 to have an acyclic module
--    hierarchy.
module Concordium.GlobalState.ContractStateFFIHelpers (
    BlobStoreCallbacks (..),
    LoadCallback,
    StoreCallback,
    LoadLengthCallback,
    LoadRangeCallback,
    LoadCallbackType,
    LoadLengthCallbackType,
    LoadRangeCallbackType,
    StoreCallbackType,
    Vec,
    copyToRustVec,
    createSafeLoadLengthCallback,
    createSafeLoadRangeCallback,
    createLoadCallback,
    createStoreCallback,
    errorLoadCallback,
    errorBlobStoreCallbacks,
) where

import qualified Control.Exception as Control
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Word as Data
import qualified Foreign.C.Types as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified System.IO.Unsafe as System

-- | Opaque type representing a Rust vector. The vector's lifetime is managed by Rust, in the sense
--  that the vector will be deallocated by the Rust runtime.
data Vec

-- | Callback for reading from the blob store into the provided buffer. The
--  argument is the location to read from. The return value is (pointer to) a
--  vector that should be passed to the Rust runtime.
type LoadCallbackType = Data.Word64 -> IO (Foreign.Ptr Vec)

type LoadCallback = Foreign.FunPtr LoadCallbackType

-- | Callback that reads the length metadata of a stored blob. The callback
-- writes the length only after a successful read. It does not let an exception
-- cross the native boundary.
type LoadLengthCallbackType = Data.Word64 -> Foreign.Ptr Data.Word64 -> IO Data.Word8

type LoadLengthCallback = Foreign.FunPtr LoadLengthCallbackType

-- | Callback that reads a clamped payload range into an allocated Rust vector.
-- The callback writes the vector only after a successful read.
type LoadRangeCallbackType = Data.Word64 -> Data.Word64 -> Foreign.CSize -> Foreign.Ptr (Foreign.Ptr Vec) -> IO Data.Word8

type LoadRangeCallback = Foreign.FunPtr LoadRangeCallbackType

-- | Named operations for V1 contract-state backing storage. The store owns all
-- function pointers in this record. The store frees the pointers one time when
-- it is destroyed.
data BlobStoreCallbacks = BlobStoreCallbacks
    { loadCallback :: !LoadCallback,
      loadLengthCallback :: !LoadLengthCallback,
      loadRangeCallback :: !LoadRangeCallback,
      storeCallback :: !StoreCallback
    }

-- | Callback for writing to the blob store from the provided buffer. The
--  arguments are the buffer where the data is and the amount of data to write.
--  It is assumed that the buffer has sufficient size. The return value is the
--  location where data was written.
type StoreCallbackType = Foreign.Ptr Data.Word8 -> Foreign.CSize -> IO Data.Word64

type StoreCallback = Foreign.FunPtr StoreCallbackType

-- | Wrappers for making callbacks from Haskell functions or closures.
foreign import ccall "wrapper" createLoadCallback :: LoadCallbackType -> IO LoadCallback

foreign import ccall "wrapper" createLoadLengthCallback :: LoadLengthCallbackType -> IO LoadLengthCallback

foreign import ccall "wrapper" createLoadRangeCallback :: LoadRangeCallbackType -> IO LoadRangeCallback

foreign import ccall "wrapper" createStoreCallback :: StoreCallbackType -> IO StoreCallback

-- | Allocate and return a Rust vector that contains the given data.
foreign import ccall "copy_to_vec_ffi" copyToRustVec :: Foreign.Ptr Data.Word8 -> Foreign.CSize -> IO (Foreign.Ptr Vec)

-- | A callback that always panics. This is used in the basic state
--  implementation which never stores any data in the backing store. NOINLINE
--  here ensures that only a single instance of callbacks is allocated.
{-# NOINLINE errorLoadCallback #-}
errorLoadCallback :: LoadCallback
errorLoadCallback = System.unsafePerformIO $ createLoadCallback (\_location -> error "Error load callback invoked, and it should not have been.")

-- | Metadata callback for contexts that cannot access persisted data.
{-# NOINLINE errorLoadLengthCallback #-}
errorLoadLengthCallback :: LoadLengthCallback
errorLoadLengthCallback = System.unsafePerformIO $ createLoadLengthCallback (\_location _out -> return 1)

-- | Range callback for contexts that cannot access persisted data.
{-# NOINLINE errorLoadRangeCallback #-}
errorLoadRangeCallback :: LoadRangeCallback
errorLoadRangeCallback = System.unsafePerformIO $ createLoadRangeCallback (\_location _offset _length _out -> return 1)

{-# NOINLINE errorStoreCallback #-}
errorStoreCallback :: StoreCallback
errorStoreCallback = System.unsafePerformIO $ createStoreCallback (\_ptr _size -> error "Error store callback invoked, and it should not have been.")

errorBlobStoreCallbacks :: BlobStoreCallbacks
errorBlobStoreCallbacks =
    BlobStoreCallbacks
        { loadCallback = errorLoadCallback,
          loadLengthCallback = errorLoadLengthCallback,
          loadRangeCallback = errorLoadRangeCallback,
          storeCallback = errorStoreCallback
        }

-- | Make an exception-safe metadata callback from an IO length query. Return 0
-- after a successful query. Return 1 if the query throws an exception.
createSafeLoadLengthCallback :: (Data.Word64 -> IO Data.Word64) -> IO LoadLengthCallback
createSafeLoadLengthCallback query = createLoadLengthCallback $ \location outLength -> do
    result <- Control.try (query location)
    case result of
        Left (_ :: Control.SomeException) -> return 1
        Right len -> do
            Foreign.poke outLength len
            return 0

-- | Make an exception-safe range callback. Return 0 after a successful query.
-- Return 1 if the query throws or returns more bytes than requested.
createSafeLoadRangeCallback :: (Data.Word64 -> Data.Word64 -> Int -> IO BS.ByteString) -> IO LoadRangeCallback
createSafeLoadRangeCallback query = createLoadRangeCallback $ \location offset requestedLength outVector -> do
    if toInteger requestedLength > toInteger (maxBound :: Int)
        then return 1
        else do
            let requested = fromIntegral requestedLength
            result <- Control.try $ do
                bytes <- query location offset requested
                if BS.length bytes > requested
                    then return False
                    else do
                        vector <- BS.unsafeUseAsCStringLen bytes $ \(sourcePtr, len) ->
                            copyToRustVec (Foreign.castPtr sourcePtr) (fromIntegral len)
                        Foreign.poke outVector vector
                        return True
            case result of
                Left (_ :: Control.SomeException) -> return 1
                Right False -> return 1
                Right True -> return 0
