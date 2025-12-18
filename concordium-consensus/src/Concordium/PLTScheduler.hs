{-# LANGUAGE OverloadedStrings #-}

-- |
-- Bindings into the @plt-scheduler@ Rust library exposing safe wrappers.
--
-- Each foreign imported function must match the signature of functions found in @plt-scheduler/src/ffi.rs@.
module Concordium.PLTScheduler (
    executeTransaction,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

-- | Execute a transaction payload modifying the `block_state` accordingly.
-- The caller must ensure to rollback state changes in case of the transaction being rejected.
--
-- See @execute_transaction@ in @plt-scheduler@ rust crate for details.
executeTransaction ::
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The events produced or the reject reason.
    IO (Either () ())
executeTransaction transactionPayload = do
    statusCode <- BS.unsafeUseAsCStringLen transactionPayload $ \(transactionPayloadPtr, transactionPayloadLen) -> do
        ffiExecuteTransaction (FFI.castPtr transactionPayloadPtr) (fromIntegral transactionPayloadLen)
    case statusCode of
        0 -> return $ Right ()
        1 -> return $ Left ()
        _ -> error "Unexpected status code from calling 'ffiExecuteTransaction'"

foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        -- | Pointer to transaction payload bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of transaction payload.
        FFI.CSize ->
        -- | Status code
        IO Word.Word8
