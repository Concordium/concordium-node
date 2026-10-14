module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Status (FFIStatusCode (..), parseStatusCode) where

import qualified Data.Word as Word

-- | Status code returned from the Rust scheduler library.
--
-- This must match the @FfiStatusCode@ type defined on the rust side.
data FFIStatusCode
    = -- | The call succeeded.
      FSCSuccess
    | -- | The call failed gracefully.
      FSCFailed
    | -- | The call resulted in a panic.
      FSCPanic

-- | Parse the word8 encoding of the @FfiStatusCode@ type found in the Rust library.
parseStatusCode :: Word.Word8 -> Maybe FFIStatusCode
parseStatusCode code = case code of
    0 -> Just FSCSuccess
    1 -> Just FSCFailed
    2 -> Just FSCPanic
    _ -> Nothing
