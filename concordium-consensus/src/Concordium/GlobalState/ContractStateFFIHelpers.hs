module Concordium.GlobalState.ContractStateFFIHelpers where
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

-- |Callback for reading from the blob store into the provided buffer.
-- The arguments are (in order) the amount of data to read, a buffer to write it to, and the location to read it from.
data Vec
type LoadCallbackType = Word64 -> IO (Ptr Vec)
type LoadCallback = FunPtr LoadCallbackType
-- |Callback for writing to the blob store from the provided buffer.
-- The arguments are (in order) the amounf data to write, the buffer where the data is. The return value is
-- the location where data was written.
type StoreCallbackType = Ptr Word8 -> CSize -> IO Word64
type StoreCallback = FunPtr StoreCallbackType

foreign import ccall "wrapper" createLoadCallback :: LoadCallbackType -> IO LoadCallback
foreign import ccall "wrapper" createStoreCallback :: StoreCallbackType -> IO StoreCallback

foreign import ccall "copy_to_vec_ffi" copyToRustVec :: Ptr Word8 -> CSize -> IO (Ptr Vec)

-- |A callback that always panicks. This is used in the basic state
-- implementation which never stores any data in the backing store.

{-# NOINLINE errorLoadCallBack #-}
errorLoadCallBack :: LoadCallback
errorLoadCallBack = unsafePerformIO $ createLoadCallback (\_location -> error "Error load callback invoked, and it should not have been.")
