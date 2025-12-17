{-# LANGUAGE OverloadedStrings #-}

-- |
-- Bindings into the @plt-scheduler@ Rust library exposing safe wrappers.
--
-- Each foreign imported function must match the signature of functions found in @plt-scheduler/src/ffi.rs@.
module Concordium.PLTScheduler (
    PLTBlockState,
    initialPLTBlockState,
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
    -- | Block state to mutate.
    PLTBlockState ->
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The events produced or the reject reason.
    IO (Either () ())
executeTransaction blockState transactionPayload = do
    statusCode <- withPLTBlockState blockState $ \blockStatePtr ->
      BS.unsafeUseAsCStringLen transactionPayload $ \(transactionPayloadPtr, transactionPayloadLen) -> do
        ffiExecuteTransaction blockStatePtr (FFI.castPtr transactionPayloadPtr) (fromIntegral transactionPayloadLen)
    case statusCode of
        0 -> return $ Right ()
        1 -> return $ Left ()
        _ -> error "Unexpected status code from calling 'ffiExecuteTransaction'"

foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        FFI.Ptr PLTBlockState ->
        -- | Pointer to transaction payload bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of transaction payload.
        FFI.CSize ->
        -- | Status code
        IO Word.Word8

-- Block state FFI

-- | Opaque pointer to the PLT block state managed by the rust library.
--
-- Memory is deallocated using a finalizer.
newtype PLTBlockState = PLTBlockState (FFI.ForeignPtr PLTBlockState)

-- | Allocate new initial block state
initialPLTBlockState :: IO PLTBlockState
initialPLTBlockState = do
  state <- ffiInitialPLTBlockState
  PLTBlockState <$> FFI.newForeignPtr ffiFreePLTBlockState state

foreign import ccall "ffi_initial_plt_block_state" ffiInitialPLTBlockState :: IO (FFI.Ptr PLTBlockState)
foreign import ccall unsafe "&ffi_free_plt_block_state" ffiFreePLTBlockState :: FFI.FinalizerPtr PLTBlockState

-- | Get temporary access to the block state pointer. The pointer should not be
--  leaked from the computation.
--
-- This ensures the finalizer is not called until the computation is over.
withPLTBlockState :: PLTBlockState -> (FFI.Ptr PLTBlockState -> IO a) -> IO a
withPLTBlockState (PLTBlockState foreignPtr) = FFI.withForeignPtr foreignPtr
