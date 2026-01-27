-- todo remove or change module as part of https://linear.app/concordium/issue/COR-2113/fix-rust-allocator-issue-related-to-multiple-rust-cdylibs
-- \* The function rs_free_array_len already exists in concordium_base, if we build plt-scheduler and wasm-chain-integration in one library, we can use the one in base

module Concordium.PLTScheduler.PLTMemory (
    rs_free_array_len_2,
) where

import qualified Data.Word as Word
import qualified Foreign as FFI

-- | Utility function shared by all instantations. Free an array that was
--  allocated on the heap, of the given size.
foreign import ccall unsafe "free_array_len_2"
    rs_free_array_len_2 :: FFI.Ptr Word.Word8 -> Word.Word64 -> IO ()
