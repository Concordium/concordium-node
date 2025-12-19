-- |
-- Bindings into the @plt-scheduler@ Rust library exposing safe wrappers.
--
-- Each foreign imported function must match the signature of functions found in @plt-scheduler/src/ffi.rs@.
module Concordium.PLTScheduler (
    executeTransaction,
    ExecutionOutcome (..),
    ExecutionAccepts (..),
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI
import Control.Monad.IO.Class (liftIO)

import qualified Concordium.PLTScheduler.PLTBlockState as PLTBlockState
import qualified Concordium.Types as Types
import qualified Data.FixedByteString as FixedByteString
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI

-- | Execute a transaction payload modifying the `block_state` accordingly.
--
-- See @execute_transaction@ in @plt-scheduler@ rust crate for details.
executeTransaction :: (BlobStore.MonadBlobStore m) =>
    -- | Block state to mutate.
    PLTBlockState.PLTBlockState ->
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The account index of the account which signed as the sender of the transaction.
    Types.AccountIndex ->
    -- | The account address of the account which signed as the sender of the transaction.
    Types.AccountAddress ->
    -- | Remaining energy.
    Types.Energy ->
    -- | Outcome of the execution
    m ExecutionOutcome
executeTransaction
    blockState
    transactionPayload
    senderAccountIndex
    (Types.AccountAddress senderAccountAddress)
    remainingEnergy = do
        loadCallback <- fst <$> BlobStore.getCallbacks
        liftIO $ FFI.alloca $ \remainingEnergyOut -> FFI.alloca $ \updatedBlockStatePtrOut -> do
                -- Invoke the ffi call
                statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                    FixedByteString.withPtrReadOnly senderAccountAddress $ \senderAccountAddressPtr ->
                        BS.unsafeUseAsCStringLen transactionPayload $
                            \(transactionPayloadPtr, transactionPayloadLen) ->
                                ffiExecuteTransaction
                                    loadCallback
                                    blockStatePtr
                                    (FFI.castPtr transactionPayloadPtr)
                                    (fromIntegral transactionPayloadLen)
                                    (fromIntegral senderAccountIndex)
                                    senderAccountAddressPtr
                                    (fromIntegral remainingEnergy)
                                    updatedBlockStatePtrOut
                                    remainingEnergyOut
                -- Process the and construct the outcome
                newRemainingEnergy <- fromIntegral <$> FFI.peek remainingEnergyOut
                status <- case statusCode of
                    0 -> do
                        updatedBlockState <- FFI.peek updatedBlockStatePtrOut >>= PLTBlockState.wrapFFIPtr
                        return $
                            Right
                                ExecutionAccepts
                                    { eaUpdatedPLTBlockState = updatedBlockState,
                                      eaEvents = ()
                                    }
                    1 -> return $ Left ()
                    _ -> error "Unexpected status code from calling 'ffiExecuteTransaction'"
                return
                    ExecutionOutcome
                        { erRemainingEnergy = newRemainingEnergy,
                          erStatus = status
                        }

foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Pointer to the starting block state.
        FFI.Ptr PLTBlockState.PLTBlockState ->
        -- | Pointer to transaction payload bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of transaction payload.
        FFI.CSize ->
        -- | The account index of the account which signed as the sender of the transaction.
        Word.Word64 ->
        -- | Pointer to 32 bytes representing the account address of the account which signed as the
        -- sender of the transaction.
        FFI.Ptr Word.Word8 ->
        -- | Remaining energy
        Word.Word64 ->
        -- | Output location for the updated block state.
        FFI.Ptr (FFI.Ptr PLTBlockState.PLTBlockState) ->
        -- | Output location for the remaining energy after execution.
        FFI.Ptr Word.Word64 ->
        -- | Status code
        IO Word.Word8

-- | The outcome of executing a transaction using the PLT scheduler.
data ExecutionOutcome = ExecutionOutcome
    { -- | The amount of energy remaining after the execution.
      erRemainingEnergy :: Types.Energy,
      -- | The resulting execution status.
      erStatus :: Either () ExecutionAccepts
    }

-- | Additional execution outcome when the transaction gets accepted by the PLT scheduler.
data ExecutionAccepts = ExecutionAccepts
    { -- | The updated PLT block state after the execution
      eaUpdatedPLTBlockState :: PLTBlockState.PLTBlockState,
      -- | Events produced during the execution
      eaEvents :: ()
    }
