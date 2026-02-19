{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Bindings into the Rust PLT Scheduler library. The module contains bindings to execute the payload of block items (currently for protocol-level tokens only).
-- Notice that block item headers are handled outside of the Rust PLT Scheduler.
--
-- Each foreign imported function must match the signature of functions found on the Rust side.
module Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler (
    executeTransaction,
    executeChainUpdate,
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI

import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Types.Updates as Types
import qualified Concordium.Utils.Serialization as CS
import qualified Data.FixedByteString as FixedByteString

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.ContractStateFFIHelpers as FFI
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as PLTBlockState
import qualified Concordium.GlobalState.Types as BS
import qualified Concordium.Scheduler.Environment as EI
import Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.BlockStateCallbacks
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Memory as Memory

-- | Execute a transaction payload modifying the `block_state` accordingly.
-- Returns the events produced if successful, otherwise a reject reason. Additionally, the
-- amount of energy used by the execution is returned. The returned values are represented
-- via the type 'TransactionExecutionSummary'. The function is a wrapper around an FFI call
-- to the Rust PLT Scheduler library.
--
-- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case of the transaction being rejected.
executeTransaction ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block protocol version
    Types.SProtocolVersion pv ->
    -- | Block state to mutate.
    PLTBlockState.ForeignPLTBlockStatePtr ->
    -- | Callbacks need for block state queries on the state maintained by Haskell.
    BlockStateQueryCallbacks ->
    -- | Callbacks need for block state operations on the state maintained by Haskell.
    BlockStateOperationCallbacks ->
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
    queryCallbacks
    operationCallbacks
    transactionPayload
    senderAccountIndex
    (Types.AccountAddress senderAccountAddress)
    remainingEnergy =
        do
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $ FFI.alloca $ \usedEnergyOutPtr -> FFI.alloca $ \resultingBlockStateOutPtr ->
                FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                        updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance $ updateTokenAccountBalance operationCallbacks
                        incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber $ incrementPltUpdateSequenceNumber operationCallbacks
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
        FFI.Ptr Word.Word64 ->
        -- | Status code:
        -- * `0` if transaction was executed and applied successfully.
        -- * `1` if transaction was rejected. Block state changes applied
        --   via callbacks must be rolled back.
        IO Word.Word8

-- | Execute a chain update in the 'SchedulerMonad' modifying the block state accordingly. The chain update
-- is executed via the Rust PLT Scheduler library. Only `CratePLT` chain updates are currently supported.
executeChainUpdate ::
    forall m.
    (Types.PVSupportsRustManagedPLT (Types.MPV m), EI.SchedulerMonad m, EI.ForeingLowLevelSchedulerMonad m) =>
    Types.UpdateHeader ->
    Types.CreatePLT ->
    m (Either Types.FailureKind Types.ValidResult)
executeChainUpdate updateHeader createPLT =
    fmap join $ runExceptT $ do
        unless (Types.updateEffectiveTime updateHeader == 0) $ throwError Types.InvalidUpdateTime
        lift $ EI.updateBlockState $ \bs -> do
            queryCallbacks <- unliftBlockStateQueryCallbacks bs
            let operationCallbacks = undefined
            BS.updateRustPLTState bs $ \pltBlockState -> do
                outcome <-
                    executeChainUpdateInBlobStoreMonad
                        (Types.protocolVersion @(Types.MPV m))
                        pltBlockState
                        queryCallbacks
                        operationCallbacks
                        createPLT
                return $ case outcome of
                    ChainUpdateExecutionOutcomeSuccess (ChainUpdateExecutionSuccess updatedPltBlockState events) ->
                        (Just updatedPltBlockState, Right $ Types.TxSuccess events)
                    ChainUpdateExecutionOutcomeFailed (ChainUpdateExecutionFailed failureKind) ->
                        (Nothing, Left failureKind)

-- | Execute a chain update modifying `block_state` accordingly.
-- Returns the events produced if successful, otherwise a failure kind. The function is a wrapper around an FFI call
-- to the Rust PLT Scheduler library.
--
-- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case a failure kind is returned.
executeChainUpdateInBlobStoreMonad ::
    (BlobStore.MonadBlobStore m) =>
    -- | Block protocol version
    Types.SProtocolVersion pv ->
    -- | Block state to mutate.
    PLTBlockState.ForeignPLTBlockStatePtr ->
    -- | Callbacks need for block state queries on the state maintained by Haskell.
    BlockStateQueryCallbacks ->
    -- | Callbacks need for block state operations on the state maintained by Haskell.
    BlockStateOperationCallbacks ->
    -- | Create PLT chain update.
    Types.CreatePLT ->
    -- | Outcome of the execution
    m ChainUpdateExecutionOutcome
executeChainUpdateInBlobStoreMonad
    spv
    blockState
    queryCallbacks
    operationCallbacks
    chainUpdatePayload =
        do
            let chainUpdatePayloadByteString = S.runPut $ Types.putUpdatePayload $ Types.CreatePLTUpdatePayload chainUpdatePayload
            loadCallbackPtr <- fst <$> BlobStore.getCallbacks
            liftIO $ FFI.alloca $ \resultingBlockStateOutPtr ->
                FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                    do
                        readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                        getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                        getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                        getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                        updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance $ updateTokenAccountBalance operationCallbacks
                        incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber $ incrementPltUpdateSequenceNumber operationCallbacks
                        -- Invoke the ffi call
                        statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                            BS.unsafeUseAsCStringLen chainUpdatePayloadByteString $ \(chainUpdatePayloadPtr, chainUpdatePayloadLen) ->
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
        FFI.Ptr Word.Word64 ->
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

-- | "Unlifts" the callback queries from the 'BlockStateOperations' monad into the IO monad, such that they can
-- be converted to FFI function pointers.
unliftBlockStateQueryCallbacks ::
    forall m.
    (Types.PVSupportsPLT (Types.MPV m), BS.BlockStateOperations m, BS.ForeingLowLevelBlockStateOperations m) =>
    BS.UpdatableBlockState m ->
    m BlockStateQueryCallbacks
unliftBlockStateQueryCallbacks bs = BS.withUnliftBSO $ \unlift ->
    do
        let readTokenAccountBalance accountIndex tokenIndex = do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                let account = maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                unlift $ BS.getAccountTokenBalance account tokenIndex
            getAccountIndexByAddress accountAddress = do
                maybeAccount <- unlift $ BS.bsoGetAccount bs accountAddress
                return $ fst <$> maybeAccount
            getAccountAddressByIndex accountIndex = do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                forM maybeAccount $ \account -> unlift $ BS.getAccountCanonicalAddress account
            getTokenAccountStates accountIndex = do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                let account = maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                fmap Map.toList $ unlift $ BS.getAccountTokens account

        return BlockStateQueryCallbacks{..}
