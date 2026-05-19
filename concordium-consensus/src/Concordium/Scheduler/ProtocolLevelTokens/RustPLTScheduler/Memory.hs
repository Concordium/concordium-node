-- | Module that implements memory management specific bindings.
--
-- todo remove or change module as part of https://linear.app/concordium/issue/PSR-61/address-potentially-unsafe-behaviour-cased-by-using-shrink-to-fit
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Memory (
    rs_free_array_len_2,
) where

import qualified Data.Word as Word
import qualified Foreign as FFI

-- | Utility function shared by all instantations. Free an array that was
--  allocated on the heap, of the given size.
foreign import ccall unsafe "free_array_len_2"
    rs_free_array_len_2 :: FFI.Ptr Word.Word8 -> Word.Word64 -> IO ()
