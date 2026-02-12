-- | Bindings into the Rust PLT Scheduler library. The module contains bindings to execute the payload of block items (currently for protocol-level tokens only).
-- Notice that block item headers are handled outside of the Rust PLT Scheduler.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler (
    executeTransaction,
    executeChainUpdate,
    TransactionExecutionSummary (..),
    TransactionExecutionOutcome (..),
    TransactionExecutionReject (..),
    TransactionExecutionSuccess (..),
    ChainUpdateExecutionOutcome (..),
    ChainUpdateExecutionFailed (..),
    ChainUpdateExecutionSuccess (..),
    ReadTokenAccountBalance,
    UpdateTokenAccountBalance,
    IncrementPltUpdateSequenceNumber,
    GetAccountIndexByAddress,
    GetAccountAddressByIndex,
    GetTokenAccountStates,
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
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLTBlockState
import Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.BlockStateCallbacks
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Memory as Memory
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Utils.Serialization as CS
import qualified Data.FixedByteString as FixedByteString

-- | Execute a transaction payload modifying the `block_state` accordingly.
-- Returns the events produced if successful, otherwise a reject reason. Additionally, the
-- amount of energy used by the execution is returned. The returned values are represented
-- via the type 'TransactionExecutionSummary'.
--
-- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case of the transaction being rejected.
executeTransaction ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block protocol version
    Types.SProtocolVersion pv ->
    -- | Block state to mutate.
    PLTBlockState.ForeignPLTBlockStatePtr ->
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
    -- | Callback to get token account states for an account
    GetTokenAccountStates ->
    -- | Transaction payload byte string.
    BS.ByteString ->
    -- | The account index of the account which signed as the sender of the transaction.
    Types.AccountIndex ->
    -- | The account address of the account which signed as the sender of the transaction.
    Types.AccountAddress ->
    -- | Remaining energy.
    Types.Energy ->
    -- | Outcome of the execution
    m TransactionExecutionSummary
executeTransaction
    spv
    blockState
    readTokenAccountBalance
    updateTokenAccountBalance
    incrementPltUpdateSequence
    getAccountIndexByAddress
    getAccountAddressByIndex
    getTokenAccountStates
    transactionPayload
    senderAccountIndex
    (Types.AccountAddress senderAccountAddress)
    remainingEnergy =
        do
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $ FFI.alloca $ \usedEnergyOutPtr -> FFI.alloca $ \resultingBlockStateOutPtr ->
                FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance readTokenAccountBalance
                        updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance updateTokenAccountBalance
                        incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber incrementPltUpdateSequence
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress getAccountIndexByAddress
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex getAccountAddressByIndex
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates getTokenAccountStates
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
                                        getTokenAccountStatesCallbackPtr
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
                        -- Free the function pointers we have just created
                        -- (loadCallbackPtr is created in another context,
                        -- so we should not free it)
                        FFI.freeHaskellFunPtr readTokenAccountBalanceCallbackPtr
                        FFI.freeHaskellFunPtr updateTokenAccountBalanceCallbackPtr
                        FFI.freeHaskellFunPtr incrementPltUpdateSequenceCallbackPtr
                        FFI.freeHaskellFunPtr getAccountIndexByAddressCallbackPtr
                        FFI.freeHaskellFunPtr getAccountAddressByIndexCallbackPtr
                        FFI.freeHaskellFunPtr getTokenAccountStatesCallbackPtr
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
                                let getEvents = S.isolate (BS.length returnData) $ CS.getListOf $ Types.getEvent spv
                                let events =
                                        either
                                            (\message -> error $ "Transaction events from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getEvents returnData
                                return $
                                    TransactionExecutionOutcomeSuccess $
                                        TransactionExecutionSuccess
                                            { tesUpdatedPLTBlockState = updatedBlockState,
                                              tesEvents = events
                                            }
                            1 -> do
                                let getRejectReason = S.isolate (BS.length returnData) S.get
                                let rejectReason =
                                        either
                                            (\message -> error $ "Transaction reject reason from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getRejectReason returnData
                                return $
                                    TransactionExecutionOutcomeReject $
                                        TransactionExecutionReject
                                            { terRejectReason = rejectReason
                                            }
                            _ -> error ("Unexpected status code from calling 'ffiExecuteTransaction': " ++ show statusCode)
                        return
                            TransactionExecutionSummary
                                { tesUsedEnergy = usedEnergy,
                                  tesOutcome = oucome
                                }

-- | C-binding for calling the Rust function `plt_scheduler::scheduler::execute_transaction`.
--
-- Returns a byte representing the result:
--
-- - `0`: Transaction execution succeeded and transaction was applied to block state.
-- - `1`: Transaction was rejected with a reject reason. Block state changes applied
--   via callbacks must be rolled back.
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
        -- | Called to get token account states for account.
        GetTokenAccountStatesCallbackPtr ->
        -- | Pointer to the input PLT block state.
        FFI.Ptr PLTBlockState.RustPLTBlockState ->
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
        -- This is only set if the execution was successful (return code `0`)
        FFI.Ptr (FFI.Ptr PLTBlockState.RustPLTBlockState) ->
        -- | Output location for the energy used by the execution.
        -- This is written regardless of whether return code is `0` or `1`
        FFI.Ptr Word.Word64 ->
        -- | Output location for array containing return data, which is either serialized events or reject reason.
        -- If the return value is `0`, the data is a list of transaction events. If the return value is `1`, it is a reject reason.
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr FFI.CSize ->
        -- | Status code:
        -- * `0` if transaction was executed and applied successfully.
        -- * `1` if transaction was rejected. Block state changes applied
        --   via callbacks must be rolled back.
        IO Word.Word8

-- | Execute a chain update modifying `block_state` accordingly.
-- Returns the events produced if successful, otherwise a failure kind.
--
-- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case a failure kind is returned.
executeChainUpdate ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block protocol version
    Types.SProtocolVersion pv ->
    -- | Block state to mutate.
    PLTBlockState.ForeignPLTBlockStatePtr ->
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
    -- | Callback to get token account states for an account
    GetTokenAccountStates ->
    -- | Chain update payload byte string.
    BS.ByteString ->
    -- | Outcome of the execution
    m ChainUpdateExecutionOutcome
executeChainUpdate
    spv
    blockState
    readTokenAccountBalance
    updateTokenAccountBalance
    incrementPltUpdateSequence
    getAccountIndexByAddress
    getAccountAddressByIndex
    getTokenAccountStates
    chainUpdatePayload =
        do
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $ FFI.alloca $ \resultingBlockStateOutPtr ->
                FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance readTokenAccountBalance
                        updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance updateTokenAccountBalance
                        incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber incrementPltUpdateSequence
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress getAccountIndexByAddress
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex getAccountAddressByIndex
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates getTokenAccountStates
                        -- Invoke the ffi call
                        statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                            BS.unsafeUseAsCStringLen chainUpdatePayload $ \(chainUpdatePayloadPtr, chainUpdatePayloadLen) ->
                                ffiExecuteChainUpdate
                                    loadCallbackPtr
                                    readTokenAccountBalanceCallbackPtr
                                    updateTokenAccountBalanceCallbackPtr
                                    incrementPltUpdateSequenceCallbackPtr
                                    getAccountIndexByAddressCallbackPtr
                                    getAccountAddressByIndexCallbackPtr
                                    getTokenAccountStatesCallbackPtr
                                    blockStatePtr
                                    (FFI.castPtr chainUpdatePayloadPtr)
                                    (fromIntegral chainUpdatePayloadLen)
                                    resultingBlockStateOutPtr
                                    returnDataPtrOutPtr
                                    returnDataLenOutPtr
                        -- Free the function pointers we have just created
                        -- (loadCallbackPtr is created in another context,
                        -- so we should not free it)
                        FFI.freeHaskellFunPtr readTokenAccountBalanceCallbackPtr
                        FFI.freeHaskellFunPtr updateTokenAccountBalanceCallbackPtr
                        FFI.freeHaskellFunPtr incrementPltUpdateSequenceCallbackPtr
                        FFI.freeHaskellFunPtr getAccountIndexByAddressCallbackPtr
                        FFI.freeHaskellFunPtr getAccountAddressByIndexCallbackPtr
                        FFI.freeHaskellFunPtr getTokenAccountStatesCallbackPtr
                        -- Process the returned status and values returned via out pointers
                        returnDataLen <- FFI.peek returnDataLenOutPtr
                        returnDataPtr <- FFI.peek returnDataPtrOutPtr
                        returnData <-
                            BS.unsafePackCStringFinalizer
                                returnDataPtr
                                (fromIntegral returnDataLen)
                                (Memory.rs_free_array_len_2 returnDataPtr (fromIntegral returnDataLen))
                        case statusCode of
                            0 -> do
                                updatedBlockState <- FFI.peek resultingBlockStateOutPtr >>= PLTBlockState.wrapFFIPtr
                                let getEvents = S.isolate (BS.length returnData) $ CS.getListOf $ Types.getEvent spv
                                let events =
                                        either
                                            (\message -> error $ "Chain update events from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getEvents returnData
                                return $
                                    ChainUpdateExecutionOutcomeSuccess $
                                        ChainUpdateExecutionSuccess
                                            { cuesUpdatedPLTBlockState = updatedBlockState,
                                              cuesEvents = events
                                            }
                            1 -> do
                                let getFailureKind = S.isolate (BS.length returnData) S.get
                                let failureKind =
                                        either
                                            (\message -> error $ "Chain update failure kind from Rust PLT Scheduler could not be deserialized: " ++ message)
                                            id
                                            $ S.runGet getFailureKind returnData
                                return $
                                    ChainUpdateExecutionOutcomeFailed $
                                        ChainUpdateExecutionFailed
                                            { cuefFailureKind = failureKind
                                            }
                            _ -> error ("Unexpected status code from calling 'ffiExecuteChainUpdate': " ++ show statusCode)

-- | C-binding for calling the Rust function `plt_scheduler::scheduler::execute_chain_update`.
--
-- Returns a byte representing the result:
--
-- - `0`: Chain update execution succeeded and update was applied to block state.
-- - `1`: Chain update failed. Block state changes applied
--   via callbacks must be rolled back.
foreign import ccall "ffi_execute_chain_update"
    ffiExecuteChainUpdate ::
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
        -- | Called to get token account states for account.
        GetTokenAccountStatesCallbackPtr ->
        -- | Pointer to the input PLT block state.
        FFI.Ptr PLTBlockState.RustPLTBlockState ->
        -- | Pointer to chain update payload bytes.
        FFI.Ptr Word.Word8 ->
        -- | Byte length of chain update payload.
        FFI.CSize ->
        -- | Output location for the resulting PLT block state.
        -- Only written set if the execution was successful (return code `0`)
        FFI.Ptr (FFI.Ptr PLTBlockState.RustPLTBlockState) ->
        -- | Output location for array containing return data, which is either serialized events or failure kind.
        -- If the return value is `0`, the data is a list of events. If the return value is `1`, it is a failure kind.
        FFI.Ptr (FFI.Ptr Word.Word8) ->
        -- | Output location for writing the length of the return data.
        FFI.Ptr FFI.CSize ->
        -- | Status code:
        -- * `0` if chain update was executed and applied successfully.
        -- * `1` if chain update failed. Block state changes applied
        --   via callbacks must be rolled back.
        IO Word.Word8

-- | Summary of executing a transaction using the PLT scheduler.
data TransactionExecutionSummary = TransactionExecutionSummary
    { -- | The amount of energy used by the transaction execution.
      tesUsedEnergy :: Types.Energy,
      -- | The outcome (success/rejection) of the transaction execution. The transaction can either be successful or rejected.
      -- If the transaction is rejected, the changes to the block state must be rolled back.
      tesOutcome :: TransactionExecutionOutcome
    }

-- | Outcome of the transaction: successful or rejected.
-- If the transaction was rejected, the changes to the block state must be rolled back.
data TransactionExecutionOutcome = TransactionExecutionOutcomeSuccess TransactionExecutionSuccess | TransactionExecutionOutcomeReject TransactionExecutionReject

-- | Representation of rejected transaction execution outcome
data TransactionExecutionReject = TransactionExecutionReject
    { -- | Transaction reject reason
      terRejectReason :: Types.RejectReason
    }

-- | Representation of successful transaction execution outcome
data TransactionExecutionSuccess = TransactionExecutionSuccess
    { -- | The updated PLT block state after the execution
      tesUpdatedPLTBlockState :: PLTBlockState.ForeignPLTBlockStatePtr,
      -- | Events produced during the execution
      tesEvents :: [Types.Event]
    }

-- | Outcome of the chain update: successful or failed.
-- If the chain update failed, the changes to the block state must be rolled back.
data ChainUpdateExecutionOutcome = ChainUpdateExecutionOutcomeSuccess ChainUpdateExecutionSuccess | ChainUpdateExecutionOutcomeFailed ChainUpdateExecutionFailed

-- | Representation of failed chain update outcome
data ChainUpdateExecutionFailed = ChainUpdateExecutionFailed
    { -- | Chain update failure kind
      cuefFailureKind :: Types.FailureKind
    }

-- | Representation of successful chain update outcome
data ChainUpdateExecutionSuccess = ChainUpdateExecutionSuccess
    { -- | The updated PLT block state after the execution
      cuesUpdatedPLTBlockState :: PLTBlockState.ForeignPLTBlockStatePtr,
      -- | Events produced during the execution
      cuesEvents :: [Types.Event]
    }
