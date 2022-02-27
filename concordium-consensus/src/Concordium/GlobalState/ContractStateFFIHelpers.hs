{-| Helper types and foreign imports to use V1 contract state. This is here and
    not in Concordium.GlobalState.ContractStateV1 to have an acyclic module
    hierarchy.
 -}
module Concordium.GlobalState.ContractStateFFIHelpers where
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

-- |Opaque type representing a Rust vector. The vector's lifetime is managed by Rust.
data Vec

-- |Callback for reading from the blob store into the provided buffer. The
-- arguments are (in order) the amount of data to read, a buffer to write it to,
-- and the location to read it from.
type LoadCallbackType = Word64 -> IO (Ptr Vec)
type LoadCallback = FunPtr LoadCallbackType
-- |Callback for writing to the blob store from the provided buffer.
-- The arguments are (in order) the amounf data to write, the buffer where the data is. The return value is
-- the location where data was written.
type StoreCallbackType = Ptr Word8 -> CSize -> IO Word64
type StoreCallback = FunPtr StoreCallbackType

-- |Wrappers for making callbacks from Haskell functions or closures.
foreign import ccall "wrapper" createLoadCallback :: LoadCallbackType -> IO LoadCallback
foreign import ccall "wrapper" createStoreCallback :: StoreCallbackType -> IO StoreCallback

-- |Allocate and return a Rust vector that contains the given data.
foreign import ccall "copy_to_vec_ffi" copyToRustVec :: Ptr Word8 -> CSize -> IO (Ptr Vec)

-- |A callback that always panicks. This is used in the basic state
-- implementation which never stores any data in the backing store.
-- NOINLINE here ensures that only a single instance of callbacks are allocated.
{-# NOINLINE errorLoadCallBack #-}
errorLoadCallBack :: LoadCallback
errorLoadCallBack = unsafePerformIO $ createLoadCallback (\_location -> error "Error load callback invoked, and it should not have been.")

-- |Deallocate the callbacks. This should generally be called to not leak memory.
freeErrorCallBack :: LoadCallback -> IO ()
freeErrorCallBack = freeHaskellFunPtr
