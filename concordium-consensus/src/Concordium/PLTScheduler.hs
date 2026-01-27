-- |
-- Bindings into the @plt-scheduler@ Rust library exposing safe wrappers.
--
-- Each foreign imported function must match the signature of functions found in @plt-scheduler/src/ffi.rs@.
module Concordium.PLTScheduler (
    executeTransaction,
    ExecutionSummary (..),
    ExecutionOutcome (..),
    ReadTokenAccountBalance,
    UpdateTokenAccountBalance,
) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.PLTScheduler.PLTBlockState as PLTBlockState
import Concordium.PLTScheduler.PLTBlockStateCallbacks
import qualified Concordium.PLTScheduler.PLTMemory as Memory
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Utils.Serialization as CS
import Data.Either
import qualified Data.FixedByteString as FixedByteString

-- | Execute a transaction payload modifying the `block_state` accordingly.
executeTransaction ::
    (BlobStore.MonadBlobStore m) =>
    Types.SProtocolVersion pv ->
    -- | Block state to mutate.
    PLTBlockState.PLTBlockState ->
    -- | Callback for reading the token balance of an account.
    ReadTokenAccountBalance ->
    -- | Callback for updating the token balance of an account.
    UpdateTokenAccountBalance ->
    -- | Callback for incrementing PLT update sequence number.
    IncrementPltUpdateSequenceNumber ->
    -- | Callback to get account index by account address
    GetAccountIndexByAddress ->
    -- | Callback to get account address by account index
    GetAccountAddressByIndex ->
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The account index of the account which signed as the sender of the transaction.
    Types.AccountIndex ->
    -- | The account address of the account which signed as the sender of the transaction.
    Types.AccountAddress ->
    -- | Remaining energy.
    Types.Energy ->
    -- | Outcome of the execution
    m ExecutionSummary
executeTransaction
    spv
    blockState
    readTokenAccountBalance
    updateTokenAccountBalance
    incrementPltUpdateSequence
    getAccountIndexByAddress
    getAccountAddressByIndex
    transactionPayload
    senderAccountIndex
    (Types.AccountAddress senderAccountAddress)
    remainingEnergy =
        do
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $
                FFI.alloca $ \usedEnergyOutPtr ->
                    FFI.alloca $ \resultingBlockStateOutPtr ->
                        FFI.alloca $ \returnDataPtrOutPtr ->
                            FFI.alloca $ \returnDataLenOutPtr ->
                                do
                                    readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance readTokenAccountBalance
                                    updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance updateTokenAccountBalance
                                    incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber incrementPltUpdateSequence
                                    getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress getAccountIndexByAddress
                                    getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex getAccountAddressByIndex
                                    -- Invoke the ffi call
                                    statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                                        FixedByteString.withPtrReadOnly (senderAccountAddress) $ \senderAccountAddressPtr ->
                                            BS.unsafeUseAsCStringLen transactionPayload $ \(transactionPayloadPtr, transactionPayloadLen) ->
                                                ffiExecuteTransaction
                                                    loadCallbackPtr
                                                    readTokenAccountBalanceCallbackPtr
                                                    updateTokenAccountBalanceCallbackPtr
                                                    incrementPltUpdateSequenceCallbackPtr
                                                    getAccountIndexByAddressCallbackPtr
                                                    getAccountAddressByIndexCallbackPtr
                                                    blockStatePtr
                                                    (FFI.castPtr transactionPayloadPtr)
                                                    (fromIntegral transactionPayloadLen)
                                                    (fromIntegral senderAccountIndex)
                                                    senderAccountAddressPtr
                                                    (fromIntegral remainingEnergy)
                                                    resultingBlockStateOutPtr
                                                    usedEnergyOutPtr
                                                    returnDataPtrOutPtr
                                                    returnDataLenOutPtr
                                    -- Free the function pointers we have just created (loadCallbackPtr is created in another context)
                                    FFI.freeHaskellFunPtr readTokenAccountBalanceCallbackPtr
                                    FFI.freeHaskellFunPtr updateTokenAccountBalanceCallbackPtr
                                    FFI.freeHaskellFunPtr incrementPltUpdateSequenceCallbackPtr
                                    FFI.freeHaskellFunPtr getAccountIndexByAddressCallbackPtr
                                    FFI.freeHaskellFunPtr getAccountAddressByIndexCallbackPtr
                                    -- Process the returned status and values returend via out pointers
                                    usedEnergy <- fromIntegral <$> FFI.peek usedEnergyOutPtr
                                    returnDataLen <- FFI.peek returnDataLenOutPtr
                                    returnDataPtr <- FFI.peek returnDataPtrOutPtr
                                    returnData <-
                                        BS.unsafePackCStringFinalizer
                                            returnDataPtr
                                            (fromIntegral returnDataLen)
                                            (Memory.rs_free_array_len_2 returnDataPtr (fromIntegral returnDataLen))
                                    oucome <- case statusCode of
                                        0 -> do
                                            updatedBlockState <- FFI.peek resultingBlockStateOutPtr >>= PLTBlockState.wrapFFIPtr
                                            let events =
                                                    fromRight (error "Block item events from Rust PLT Scheduler could not be deserialized") $
                                                        S.runGet (CS.getListOf $ Types.getEvent spv) returnData
                                            return $
                                                ExecutionSuccess $
                                                    ExecutionOutcomeSuccess
                                                        { eosUpdatedPLTBlockState = updatedBlockState,
                                                          eosEvents = events
                                                        }
                                        1 -> do
                                            let rejectReason =
                                                    fromRight (error "Reject reason from Rust PLT Scheduler could not be deserialized") $
                                                        S.decode returnData
                                            return $
                                                ExecutionReject $
                                                    ExecutionOutcomeReject
                                                        { eorRejectReason = rejectReason
                                                        }
                                        _ -> error ("Unexpected status code from calling 'ffiExecuteTransaction'" ++ show statusCode)
                                    return
                                        ExecutionSummary
                                            { esUsedEnergy = usedEnergy,
                                              esOutcome = oucome
                                            }

foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Called to get the token account balance in the haskell-managed block state.
        ReadTokenAccountBalanceCallbackPtr ->
        -- | Called to update the token account balance in the haskell-managed block state.
        UpdateTokenAccountBalanceCallbackPtr ->
        -- | Called to increment the PLT update sequence number.
        IncrementPltUpdateSequenceNumberCallbackPtr ->
        -- | Called to get account index by account address.
        GetAccountIndexByAddressCallbackPtr ->
        -- | Called to get account address by account index.
        GetAccountAddressByIndexCallbackPtr ->
        -- | Pointer to the input PLT block state.
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
        -- | Output location for the resulting PLT block state.
        -- Only written set if the executes was successful (return code `0`)
        FFI.Ptr (FFI.Ptr PLTBlockState.PLTBlockState) ->
        -- | Output location for the energy used by the execution.
        -- This is written regardless of whether return code is `0` or `1`
        FFI.Ptr Word.Word64 ->
        -- | Output location for array containing return data, which is either serialized events or reject reason.
        -- If the return value is `0`, the data is a list of transaction events. If the return value is `1`, it is a reject reason.
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr Word.Word64 ->
        -- | Status code:
        -- * `0` if transaction was executed and applied successfully
        -- * `1` if transaction was rejected
        IO Word.Word8

-- | Summary of executing a transaction using the PLT scheduler.
data ExecutionSummary = ExecutionSummary
    { -- | The amount of energy used by the execution.
      esUsedEnergy :: Types.Energy,
      -- | The outcome (success/rejection) of the execution.
      esOutcome :: ExecutionOutcome
    }

-- | Outcome of the transaction: successful or rejected
data ExecutionOutcome = ExecutionSuccess ExecutionOutcomeSuccess | ExecutionReject ExecutionOutcomeReject

-- todo introduce reject reason as part of https://linear.app/concordium/issue/PSR-44/implement-serialization-and-returning-events-and-reject-reasons
-- eorRejectReason :: Types.RejectReason

-- | Representation of rejected outcome
data ExecutionOutcomeReject = ExecutionOutcomeReject
    { -- | Transaction reject reason
      eorRejectReason :: ()
    }

-- | Representation of successful outcome
data ExecutionOutcomeSuccess = ExecutionOutcomeSuccess
    { -- | The updated PLT block state after the execution
      eosUpdatedPLTBlockState :: PLTBlockState.PLTBlockState,
      -- | Events produced during the execution
      eosEvents :: [Types.Event]
    }
