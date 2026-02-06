-- todo remove or change module as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
-- The function rs_free_array_len already exists in concordium_base in Concordium.Crypto.FFIHelpers. If we build plt-scheduler and wasm-chain-integration in one library, we can use the one in Concordium.Crypto.FFIHelpers instead of rs_free_array_len_2 below (which links to the Rust PLT Scheduler library).
-- The function copy_to_vec_ffi already exists in Concordium.GlobalState.ContractStateFFIHelpers. If we build plt-scheduler and wasm-chain-integration in one library, we can use the one in Concordium.GlobalState.ContractStateFFIHelpers instead of copy_to_vec_ffi_2 below (which links to the Rust PLT Scheduler library).

module Concordium.PLTScheduler.PLTMemory (
    rs_free_array_len_2,
    copyToRustVec2,
    RustVec,
) where

import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

-- | Utility function shared by all instantations. Free an array that was
--  allocated on the heap, of the given size.
foreign import ccall unsafe "free_array_len_2"
    rs_free_array_len_2 :: FFI.Ptr Word.Word8 -> Word.Word64 -> IO ()

-- | Opaque type representing a Rust vector. The vector's lifetime is managed by Rust, in the sense
--  that the vector will be deallocated by the Rust runtime.
data RustVec

-- | Allocate and return a Rust vector that contains the given data.
foreign import ccall "copy_to_vec_ffi_2" copyToRustVec2 :: FFI.Ptr Word.Word8 -> FFI.CSize -> IO (FFI.Ptr RustVec)
