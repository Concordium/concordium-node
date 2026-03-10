{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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
import Data.Functor
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Word as Word
import qualified Foreign as FFI
import qualified Foreign.C.Types as FFI
import Lens.Micro

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

-- | Execute a transaction payload in the 'SchedulerMonad' modifying the block state accordingly. The transaction
-- is executed via the Rust PLT Scheduler library. Only 'TokenUpdate' transaction payloads are currently supported.
executeTransaction ::
    forall m.
    (BS.BlockStateOperations m, Types.PVSupportsRustManagedPLT (Types.MPV m)) =>
    EI.WithDepositContext m ->
    -- | Transaction payload.
    Types.Payload ->
    -- | Transaction summary
    EI.SchedulerT m (Maybe (Types.TransactionSummary (Types.TransactionOutcomesVersionFor (Types.MPV m))))
executeTransaction depositContext tokenUpdate =
    EI.withDeposit depositContext executeTransactionInLocalT EI.defaultSuccess
  where
    executeTransactionInLocalT :: EI.LocalT [Types.Event] (EI.SchedulerT m) [Types.Event]
    executeTransactionInLocalT = do
        (remainingEnergy, _energyLimitReason) <- EI.getEnergy

        summary <- lift $ executeTransactionInLocalTLiftedSchedulerMonad remainingEnergy

        -- Charge energy
        EI.tickEnergy $ tesUsedEnergy summary

        -- Map execution outcome.
        case tesOutcome summary of
            TransactionExecutionOutcomeSuccess (TransactionExecutionSuccess () events) -> do
                return events
            TransactionExecutionOutcomeReject (TransactionExecutionReject rejectReason) ->
                EI.rejectTransaction rejectReason

    executeTransactionInLocalTLiftedSchedulerMonad :: Types.Energy -> EI.SchedulerT m (TransactionExecutionSummary ())
    executeTransactionInLocalTLiftedSchedulerMonad remainingEnergy =
        -- We need to run with block state rollback, since callbacks may modify the block state, and
        -- it is modified in a non-functional way via interior mutability (PersistentBlockState is an IORef).
        EI.withBlockStateRollback $
            do
                -- Get current block state.
                blockState0 <- EI.getBlockState

                -- Execute transaction in the block state monad.
                summary <- lift $ executeTransactionInBSOMonad remainingEnergy blockState0

                -- Set updated block state if operation was successful and set rollback.
                rollback <- case tesOutcome summary of
                    TransactionExecutionOutcomeSuccess (TransactionExecutionSuccess blockState1 _) -> do
                        EI.setBlockState blockState1
                        return False
                    TransactionExecutionOutcomeReject (TransactionExecutionReject _) ->
                        return True

                return $
                    ( summary $> (),
                      rollback
                    )

    -- Execute a transaction with the given block state as input.
    -- Returns the updated block state and events produced if successful, otherwise a failure kind.
    --
    -- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case a failure kind is returned.
    executeTransactionInBSOMonad ::
        Types.Energy ->
        BS.UpdatableBlockState m ->
        m (TransactionExecutionSummary (BS.UpdatableBlockState m))
    executeTransactionInBSOMonad remainingEnergy blockState0 = do
        -- Get current PLT block state
        pltBlockState0 <- BS.bsoGetRustPLTBlockState blockState0

        -- Put block state in an IORef to allow callbacks to update it.
        blockStateIORef <- BS.liftBlobStore $ liftIO $ IORef.newIORef blockState0
        queryCallbacks <- unliftBlockStateQueryCallbacks blockStateIORef
        operationCallbacks <- unliftBlockStateOperationCallbacks blockStateIORef

        -- Execute chain update via FFI.
        outcome <-
            BS.liftBlobStore $
                executeTransactionInBlobStoreMonad
                    (Types.protocolVersion @(Types.MPV m))
                    pltBlockState0
                    queryCallbacks
                    operationCallbacks
                    (fst $ depositContext ^. EI.wtcSenderAccount)
                    (depositContext ^. EI.wtcSenderAddress)
                    remainingEnergy

        -- Get block state from IORef and set the updated PLT block state if operation was successful.
        forM outcome $ \pltBlockState1 -> do
            blockState1 <- BS.liftBlobStore $ liftIO $ IORef.readIORef blockStateIORef
            BS.bsoSetRustPLTBlockState blockState1 pltBlockState1

    -- Execute a transaction payload with the given PLT block state as input.
    -- Returns the updated PLT block state and events produced if successful, otherwise a failure kind.
    --
    -- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case of the transaction being rejected.
    executeTransactionInBlobStoreMonad ::
        (BlobStore.MonadBlobStore m') =>
        -- Block protocol version
        Types.SProtocolVersion pv ->
        -- Block state to mutate.
        PLTBlockState.ForeignPLTBlockStatePtr ->
        -- Callbacks need for block state queries on the state maintained by Haskell.
        BlockStateQueryCallbacks ->
        -- Callbacks need for block state operations on the state maintained by Haskell.
        BlockStateOperationCallbacks ->
        -- The account index of the account which signed as the sender of the transaction.
        Types.AccountIndex ->
        -- The account address of the account which signed as the sender of the transaction.
        Types.AccountAddress ->
        -- Remaining energy.
        Types.Energy ->
        -- Outcome of the execution
        m' (TransactionExecutionSummary PLTBlockState.ForeignPLTBlockStatePtr)
    executeTransactionInBlobStoreMonad
        spv
        blockState
        queryCallbacks
        operationCallbacks
        senderAccountIndex
        (Types.AccountAddress senderAccountAddress)
        remainingEnergy =
            do
                let transactionPayloadByteString = S.runPut $ Types.putPayload tokenUpdate
                loadCallbackPtr <- fst <$> BlobStore.getCallbacks
                liftIO $ FFI.alloca $ \usedEnergyOutPtr -> FFI.alloca $ \resultingBlockStateOutPtr ->
                    FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                        do
                            readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                            getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                            getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                            getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                            updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance $ updateTokenAccountBalance operationCallbacks
                            touchTokenAccountCallbackPtr <- wrapTouchTokenAccount $ touchTokenAccount operationCallbacks
                            incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber $ incrementPltUpdateSequenceNumber operationCallbacks
                            -- Invoke the ffi call
                            statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                                FixedByteString.withPtrReadOnly (senderAccountAddress) $ \senderAccountAddressPtr ->
                                    BS.unsafeUseAsCStringLen transactionPayloadByteString $ \(transactionPayloadPtr, transactionPayloadLen) ->
                                        ffiExecuteTransaction
                                            loadCallbackPtr
                                            readTokenAccountBalanceCallbackPtr
                                            updateTokenAccountBalanceCallbackPtr
                                            touchTokenAccountCallbackPtr
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
                            FFI.freeHaskellFunPtr touchTokenAccountCallbackPtr
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
                                                { tesUpdatedBlockState = updatedBlockState,
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
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_execute_transaction"
    ffiExecuteTransaction ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Called to get the token account balance in the haskell-managed block state.
        ReadTokenAccountBalanceCallbackPtr ->
        -- | Called to update the token account balance in the haskell-managed block state.
        UpdateTokenAccountBalanceCallbackPtr ->
        -- | Called to touch token account state in the haskell-managed block state.
        TouchTokenAccountCallbackPtr ->
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

-- | Execute a chain update in the 'SchedulerMonad' modifying the block state accordingly. The chain update
-- is executed via the Rust PLT Scheduler library. Only 'CratePLT' chain updates are currently supported.
executeChainUpdate ::
    forall m.
    (BS.BlockStateOperations m, Types.PVSupportsRustManagedPLT (Types.MPV m)) =>
    -- | Chain update header.
    Types.UpdateHeader ->
    -- | Chain update payload.
    Types.CreatePLT ->
    -- | Failure or events.
    EI.SchedulerT m (Either Types.FailureKind Types.ValidResult)
executeChainUpdate updateHeader createPLT =
    fmap join $ runExceptT $ do
        unless (Types.updateEffectiveTime updateHeader == 0) $ throwError Types.InvalidUpdateTime
        -- We need to run with block state rollback, since callbacks may modify the block state, and
        -- it is modified in a non-functional way via interior mutability (PersistentBlockState is an IORef).
        lift $ EI.withBlockStateRollback $ do
            -- Get current block state.
            blockState0 <- EI.getBlockState

            -- Execute chain update in the block state monad.
            outcome <- lift $ executeChainUpdateInBSOMonad blockState0

            -- Set updated block state if operation was successful and map outcome to return value.
            case outcome of
                ChainUpdateExecutionOutcomeSuccess (ChainUpdateExecutionSuccess blockState1 events) -> do
                    EI.setBlockState blockState1
                    return (Right $ Types.TxSuccess events, False)
                ChainUpdateExecutionOutcomeFailed (ChainUpdateExecutionFailed failureKind) ->
                    return (Left failureKind, True)
  where
    -- Execute a chain update with the given block state as input.
    -- Returns the updated block state and events produced if successful, otherwise a failure kind.
    --
    -- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case a failure kind is returned.
    executeChainUpdateInBSOMonad ::
        BS.UpdatableBlockState m -> m (ChainUpdateExecutionOutcome (BS.UpdatableBlockState m))
    executeChainUpdateInBSOMonad blockState0 = do
        -- Get current PLT block state
        pltBlockState0 <- BS.bsoGetRustPLTBlockState blockState0

        -- Put block state in an IORef to allow callbacks to update it.
        blockStateIORef <- BS.liftBlobStore $ liftIO $ IORef.newIORef blockState0
        queryCallbacks <- unliftBlockStateQueryCallbacks blockStateIORef
        operationCallbacks <- unliftBlockStateOperationCallbacks blockStateIORef

        -- Execute chain update via FFI.
        outcome <-
            BS.liftBlobStore $
                executeChainUpdateInBlobStoreMonad
                    (Types.protocolVersion @(Types.MPV m))
                    pltBlockState0
                    queryCallbacks
                    operationCallbacks

        -- Get block state from IORef and set the updated PLT block state if operation was successful.
        forM outcome $ \pltBlockState1 -> do
            blockState1 <- BS.liftBlobStore $ liftIO $ IORef.readIORef blockStateIORef
            BS.bsoSetRustPLTBlockState blockState1 pltBlockState1

    -- Execute a chain update with the given PLT block state as input.
    -- Returns the updated PLT block state and events produced if successful, otherwise a failure kind.
    -- The function is a wrapper around an FFI call to the Rust PLT Scheduler library.
    --
    -- NOTICE: The caller must ensure to rollback state changes applied via callbacks in case a failure kind is returned.
    executeChainUpdateInBlobStoreMonad ::
        (BlobStore.MonadBlobStore m') =>
        -- Block protocol version
        Types.SProtocolVersion pv ->
        -- Block state to mutate.
        PLTBlockState.ForeignPLTBlockStatePtr ->
        -- Callbacks need for block state queries on the state maintained by Haskell.
        BlockStateQueryCallbacks ->
        -- Callbacks need for block state operations on the state maintained by Haskell.
        BlockStateOperationCallbacks ->
        -- Outcome of the execution
        m' (ChainUpdateExecutionOutcome PLTBlockState.ForeignPLTBlockStatePtr)
    executeChainUpdateInBlobStoreMonad
        spv
        blockState
        queryCallbacks
        operationCallbacks =
            do
                let chainUpdatePayloadByteString = S.runPut $ Types.putUpdatePayload $ Types.CreatePLTUpdatePayload createPLT
                loadCallbackPtr <- fst <$> BlobStore.getCallbacks
                liftIO $ FFI.alloca $ \resultingBlockStateOutPtr ->
                    FFI.alloca $ \returnDataPtrOutPtr -> FFI.alloca $ \returnDataLenOutPtr ->
                        do
                            readTokenAccountBalanceCallbackPtr <- wrapReadTokenAccountBalance $ readTokenAccountBalance queryCallbacks
                            getAccountIndexByAddressCallbackPtr <- wrapGetAccountIndexByAddress $ getAccountIndexByAddress queryCallbacks
                            getAccountAddressByIndexCallbackPtr <- wrapGetAccountAddressByIndex $ getAccountAddressByIndex queryCallbacks
                            getTokenAccountStatesCallbackPtr <- wrapGetTokenAccountStates $ getTokenAccountStates queryCallbacks
                            updateTokenAccountBalanceCallbackPtr <- wrapUpdateTokenAccountBalance $ updateTokenAccountBalance operationCallbacks
                            touchTokenAccountCallbackPtr <- wrapTouchTokenAccount $ touchTokenAccount operationCallbacks
                            incrementPltUpdateSequenceCallbackPtr <- wrapIncrementPltUpdateSequenceNumber $ incrementPltUpdateSequenceNumber operationCallbacks
                            -- Invoke the ffi call
                            statusCode <- PLTBlockState.withPLTBlockState blockState $ \blockStatePtr ->
                                BS.unsafeUseAsCStringLen chainUpdatePayloadByteString $ \(chainUpdatePayloadPtr, chainUpdatePayloadLen) ->
                                    ffiExecuteChainUpdate
                                        loadCallbackPtr
                                        readTokenAccountBalanceCallbackPtr
                                        updateTokenAccountBalanceCallbackPtr
                                        touchTokenAccountCallbackPtr
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
                            FFI.freeHaskellFunPtr touchTokenAccountCallbackPtr
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
                                                { cuesUpdatedBlockState = updatedBlockState,
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
--
-- See the exported function in the Rust code for documentation of safety.
foreign import ccall "ffi_execute_chain_update"
    ffiExecuteChainUpdate ::
        -- | Called to read data from blob store.
        FFI.LoadCallback ->
        -- | Called to get the token account balance in the haskell-managed block state.
        ReadTokenAccountBalanceCallbackPtr ->
        -- | Called to update the token account balance in the haskell-managed block state.
        UpdateTokenAccountBalanceCallbackPtr ->
        -- | Called to touch token account state in the haskell-managed block state.
        TouchTokenAccountCallbackPtr ->
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
data TransactionExecutionSummary a = TransactionExecutionSummary
    { -- | The amount of energy used by the transaction execution.
      tesUsedEnergy :: Types.Energy,
      -- | The outcome (success/rejection) of the transaction execution. The transaction can either be successful or rejected.
      -- If the transaction is rejected, the changes to the block state must be rolled back.
      tesOutcome :: TransactionExecutionOutcome a
    }
    deriving (Functor, Foldable, Traversable)

-- | Outcome of the transaction: successful or rejected.
-- If the transaction was rejected, the changes to the block state must be rolled back.
data TransactionExecutionOutcome a
    = TransactionExecutionOutcomeSuccess (TransactionExecutionSuccess a)
    | TransactionExecutionOutcomeReject TransactionExecutionReject
    deriving (Functor, Foldable, Traversable)

-- | Representation of rejected transaction execution outcome
data TransactionExecutionReject = TransactionExecutionReject
    { -- | Transaction reject reason
      terRejectReason :: Types.RejectReason
    }

-- | Representation of successful transaction execution outcome
data TransactionExecutionSuccess a = TransactionExecutionSuccess
    { -- | The updated block state after the execution
      tesUpdatedBlockState :: a,
      -- | Events produced during the execution
      tesEvents :: [Types.Event]
    }
    deriving (Functor, Foldable, Traversable)

-- | Outcome of the chain update: successful or failed.
-- If the chain update failed, the changes to the block state must be rolled back.
data ChainUpdateExecutionOutcome a
    = ChainUpdateExecutionOutcomeSuccess (ChainUpdateExecutionSuccess a)
    | ChainUpdateExecutionOutcomeFailed ChainUpdateExecutionFailed
    deriving (Functor, Foldable, Traversable)

-- | Representation of failed chain update outcome
data ChainUpdateExecutionFailed = ChainUpdateExecutionFailed
    { -- | Chain update failure kind
      cuefFailureKind :: Types.FailureKind
    }

-- | Representation of successful chain update outcome
data ChainUpdateExecutionSuccess a = ChainUpdateExecutionSuccess
    { -- | The updated block state after the execution
      cuesUpdatedBlockState :: a,
      -- | Events produced during the execution
      cuesEvents :: [Types.Event]
    }
    deriving (Functor, Foldable, Traversable)

-- | "Unlifts" the callback queries from the 'BlockStateOperations' monad into the IO monad, such that they can
-- be converted to FFI function pointers.
unliftBlockStateQueryCallbacks ::
    forall m.
    (Types.PVSupportsPLT (Types.MPV m), BS.BlockStateOperations m) =>
    IORef.IORef (BS.UpdatableBlockState m) ->
    m BlockStateQueryCallbacks
unliftBlockStateQueryCallbacks bsIORef = BS.withUnliftBSO $ \unlift ->
    do
        let readTokenAccountBalance accountIndex tokenIndex = withIORef bsIORef $ \bs -> do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                let account = maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                unlift $ BS.getAccountTokenBalance account tokenIndex
            getAccountIndexByAddress accountAddress = withIORef bsIORef $ \bs -> do
                maybeAccount <- unlift $ BS.bsoGetAccount bs accountAddress
                return $ fst <$> maybeAccount
            getAccountAddressByIndex accountIndex = withIORef bsIORef $ \bs -> do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                forM maybeAccount $ \account -> unlift $ BS.getAccountCanonicalAddress account
            getTokenAccountStates accountIndex = withIORef bsIORef $ \bs -> do
                maybeAccount <- unlift $ BS.bsoGetAccountByIndex bs accountIndex
                let account = maybe (error $ "Account with index does not exist: " ++ show accountIndex) id maybeAccount
                fmap Map.toList $ unlift $ BS.getAccountTokens account

        return BlockStateQueryCallbacks{..}

-- | "Unlifts" the callback operations from the 'BlockStateOperations' monad into the IO monad, such that they can
-- be converted to FFI function pointers.
unliftBlockStateOperationCallbacks ::
    forall m.
    (Types.PVSupportsPLT (Types.MPV m), BS.BlockStateOperations m) =>
    IORef.IORef (BS.UpdatableBlockState m) ->
    m BlockStateOperationCallbacks
unliftBlockStateOperationCallbacks bsIORef = BS.withUnliftBSO $ \unlift ->
    do
        let updateTokenAccountBalance accountIndex tokenIndex tokenAmountDelta =
                modifyIORef bsIORef $ \bs -> do
                    maybeBs1 <- unlift $ BS.bsoUpdateTokenAccountBalance bs tokenIndex accountIndex tokenAmountDelta
                    return $ case maybeBs1 of
                        Just bs1 -> (bs1, Just ())
                        Nothing -> (bs, Nothing)
            touchTokenAccount accountIndex tokenIndex =
                modifyIORef_ bsIORef $ \bs -> do
                    maybeBs1 <- unlift $ BS.bsoTouchTokenAccount bs tokenIndex accountIndex
                    return $ case maybeBs1 of
                        Just bs1 -> bs1
                        Nothing -> bs
            incrementPltUpdateSequenceNumber =
                modifyIORef_ bsIORef $ \bs -> do
                    unlift $ BS.bsoIncrementPLTUpdateSequenceNumber bs

        return BlockStateOperationCallbacks{..}

withIORef :: IORef.IORef a -> (a -> IO b) -> IO b
withIORef ref f = IORef.readIORef ref >>= f

modifyIORef :: IORef.IORef a -> (a -> IO (a, b)) -> IO b
modifyIORef ref f = do
    (val, ret) <- IORef.readIORef ref >>= f
    IORef.writeIORef ref val
    return ret

modifyIORef_ :: IORef.IORef a -> (a -> IO a) -> IO ()
modifyIORef_ ref f = modifyIORef ref (fmap (,()) . f)
