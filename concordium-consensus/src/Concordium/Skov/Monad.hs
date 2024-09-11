{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- The instance `GlobalStateTypes (SkovQueryMonadT m)` technically has a redundant constraint,
-- which we allow by supressing this warning.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.Skov.Monad (
    module Concordium.Skov.CatchUp.Types,
    EpochFailureResult (..),
    module Concordium.Skov.Monad,
) where

import Concordium.Skov.Query
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState (AccountOperations, BlockStateOperations, BlockStateQuery, BlockStateStorage, ContractStateOperations, ModuleQuery)
import Concordium.GlobalState.Classes as C
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Statistics (ConsensusStatistics)
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.Types
import Concordium.Logger
import Concordium.Skov.CatchUp.Types
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TV
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus)

data UpdateResult
    = -- | Message received, validated and processed
      ResultSuccess
    | -- | Message deserialization failed
      ResultSerializationFail
    | -- | The message was determined to be invalid
      ResultInvalid
    | -- | The message was received, but is awaiting a block to complete processing
      ResultPendingBlock
    | -- | The message was received, but is awaiting a finalization record to complete processing
      ResultPendingFinalization
    | -- | The message was received, but is being processed asynchronously
      ResultAsync
    | -- | The message duplicates a previously received message
      ResultDuplicate
    | -- | The message may have been valid in the past, but is no longer relevant
      ResultStale
    | -- | The sequence number for that account or update type was already used in another transaction
      ResultDuplicateNonce
    | -- | The sequence number for that account or update type is larger than the next sequence number
      ResultNonceTooLarge
    | -- | An account corresponding to the transaction's sender does not exist in the focus block
      ResultNonexistingSenderAccount
    | -- | Verifying the transaction's signature failed
      ResultVerificationFailed
    | -- | The transaction expiry time is too far in the future
      ResultExpiryTooLate
    | -- | The stated transaction energy is too low
      ResultTooLowEnergy
    | -- | The message refers to a different/unknown finalization session
      ResultIncorrectFinalizationSession
    | -- | The message could not be validated with the current state
      ResultUnverifiable
    | -- | The peer should be marked as pending unless catch up is already in progress
      ResultContinueCatchUp
    | -- | The block was sent too early and should be dropped
      ResultEarlyBlock
    | -- | The file provided for importing blocks is missing
      ResultMissingImportFile
    | -- | The message was not processed because consensus has been shut down
      ResultConsensusShutDown
    | -- | The message is for an unknown genesis index
      ResultInvalidGenesisIndex
    | -- | An account already exists to the corresponding registration id of the 'CredentialDeployment'.
      ResultDuplicateAccountRegistrationID
    | -- | The identity provider was not valid
      ResultCredentialDeploymentInvalidIP
    | -- | The anonymity revokers was not valid
      ResultCredentialDeploymentInvalidAR
    | -- | The 'CredentialDeployment' contained invalid identity provider signatures.
      ResultCredentialDeploymentInvalidSignatures
    | -- | The 'CredentialDeployment' contained an expired 'validTo'.
      ResultCredentialDeploymentExpired
    | -- | The 'ChainUpdate' contained an invalid nonce
      ResultChainUpdateSequenceNumberTooOld
    | -- | The 'ChainUpdate' contained an invalid effective time.
      ResultChainUpdateInvalidEffectiveTime
    | -- | The 'ChainUpdate' contained invalid signatures.
      ResultChainUpdateInvalidSignatures
    | -- | The stated energy of the 'Transaction' exceeds the maximum allowed.
      ResultEnergyExceeded
    | -- | The sender did not have enough funds to cover the costs.
      ResultInsufficientFunds
    | -- | The consensus message is a result of double signing, indicating malicious behaviour.
      ResultDoubleSign
    | -- | The consensus has thrown an exception and entered an unrecoverable state.
      ResultConsensusFailure
    deriving (Eq, Show)

-- | Maps a 'TV.VerificationResult' to the corresponding 'UpdateResult' type.
--  See the 'TV.VerificationResult' for more information.
transactionVerificationResultToUpdateResult :: TV.VerificationResult -> UpdateResult
-- 'Ok' mappings
transactionVerificationResultToUpdateResult (TV.Ok _) = ResultSuccess
-- 'MaybeOk' mappings
transactionVerificationResultToUpdateResult (TV.MaybeOk (TV.CredentialDeploymentInvalidIdentityProvider _)) = ResultCredentialDeploymentInvalidIP
transactionVerificationResultToUpdateResult (TV.MaybeOk TV.CredentialDeploymentInvalidAnonymityRevokers) = ResultCredentialDeploymentInvalidAR
transactionVerificationResultToUpdateResult (TV.MaybeOk (TV.ChainUpdateInvalidNonce _)) = ResultNonceTooLarge
transactionVerificationResultToUpdateResult (TV.MaybeOk TV.ChainUpdateInvalidSignatures) = ResultChainUpdateInvalidSignatures
transactionVerificationResultToUpdateResult (TV.MaybeOk TV.NormalTransactionInsufficientFunds) = ResultInsufficientFunds
transactionVerificationResultToUpdateResult (TV.MaybeOk (TV.NormalTransactionInvalidSender _)) = ResultNonexistingSenderAccount
transactionVerificationResultToUpdateResult (TV.MaybeOk TV.NormalTransactionInvalidSignatures) = ResultVerificationFailed
transactionVerificationResultToUpdateResult (TV.MaybeOk (TV.NormalTransactionInvalidNonce _)) = ResultNonceTooLarge
transactionVerificationResultToUpdateResult (TV.MaybeOk TV.NormalTransactionEnergyExceeded) = ResultEnergyExceeded
-- 'NotOk' mappings
transactionVerificationResultToUpdateResult (TV.NotOk (TV.CredentialDeploymentDuplicateAccountRegistrationID _)) = ResultDuplicateAccountRegistrationID
transactionVerificationResultToUpdateResult (TV.NotOk TV.CredentialDeploymentInvalidSignatures) = ResultCredentialDeploymentInvalidSignatures
transactionVerificationResultToUpdateResult (TV.NotOk (TV.ChainUpdateSequenceNumberTooOld _)) = ResultChainUpdateSequenceNumberTooOld
transactionVerificationResultToUpdateResult (TV.NotOk TV.ChainUpdateEffectiveTimeBeforeTimeout) = ResultChainUpdateInvalidEffectiveTime
transactionVerificationResultToUpdateResult (TV.NotOk TV.CredentialDeploymentExpired) = ResultCredentialDeploymentExpired
transactionVerificationResultToUpdateResult (TV.NotOk TV.NormalTransactionDepositInsufficient) = ResultTooLowEnergy
transactionVerificationResultToUpdateResult (TV.NotOk (TV.NormalTransactionDuplicateNonce _)) = ResultDuplicateNonce
transactionVerificationResultToUpdateResult (TV.NotOk TV.Expired) = ResultStale
transactionVerificationResultToUpdateResult (TV.NotOk TV.InvalidPayloadSize) = ResultSerializationFail

class
    ( Monad m,
      Eq (BlockPointerType m),
      HashableTo BlockHash (BlockPointerType m),
      BlockPointerData (BlockPointerType m),
      GlobalStateTypes m,
      BlockPointerMonad m,
      BlockStateQuery m,
      MonadProtocolVersion m,
      IsConsensusV0 (MPV m)
    ) =>
    SkovQueryMonad m
    where
    -- | Look up a block in the table given its hash.
    resolveBlock :: BlockHash -> m (Maybe (BlockPointerType m))

    -- | Check whether the block is known to us and part of the live tree (i.e.,
    --  not dead or pending). If only block existence is needed then this will be
    --  more efficient than using 'resolveBlock' and checking for a 'Just'
    --  response.
    isBlockKnownAndLive :: BlockHash -> m Bool

    -- | Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool

    -- | Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointerType m)

    -- | Determine the last finalized block and return it together with the finalization record
    --  that finalizes it..
    lastFinalizedBlockWithRecord :: m (BlockPointerType m, FinalizationRecord)

    -- | Retrieve the finalized block at a given finalization index, if any.
    blockAtFinIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m))

    -- | Retrieve the finalization record at a given finalization index, if any.
    recordAtFinIndex :: FinalizationIndex -> m (Maybe FinalizationRecord)

    -- | Determine the next index for finalization.
    nextFinalizationIndex :: m FinalizationIndex

    -- | Get the genesis configuration.
    getGenesisData :: m GenesisConfiguration

    -- | Get the genesis block pointer.
    genesisBlock :: m (BlockPointerType m)

    -- | Get the height of the highest blocks in the tree.
    --  Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight

    -- | Get the blocks in the branches of the tree grouped by descending height.
    --  That is the first element of the list is all of the blocks at 'getCurrentHeight',
    --  the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockPointerType m]]

    -- | Get a list of all the blocks at a given height in the tree.
    getBlocksAtHeight :: BlockHeight -> m [BlockPointerType m]

    -- | Get the first finalized block (if any) in a given epoch. The epoch can either be
    --  specified directly, or indirectly as the epoch of a supplied block.
    getFirstFinalizedOfEpoch ::
        Either Epoch (BlockPointerType m) ->
        m (Either EpochFailureResult (BlockPointerType m))

    -- | Get a block's state.
    queryBlockState :: BlockPointerType m -> m (BlockState m)

    -- | Get the outcomes of a transaction.
    queryTransactionStatus :: TransactionHash -> m (Maybe TS.TransactionStatus)

    -- | Get non-finalized transactions for an account, ordered by increasing nonce.
    queryNonFinalizedTransactions :: AccountAddressEq -> m [TransactionHash]

    -- | Get the total number of non-finalized transactions across all accounts.
    queryNumberOfNonFinalizedTransactions :: m Int

    -- | Get best guess for next account nonce.
    --  The second argument is 'True' if and only if all transactions from this account are finalized.
    queryNextAccountNonce :: AccountAddressEq -> m (Nonce, Bool)

    -- | Get the finalization index of a block's last finalized block.
    blockLastFinalizedIndex :: BlockPointerType m -> m FinalizationIndex

    -- | Get a catch-up status message. The flag indicates if the
    --  message should be a catch-up request.
    getCatchUpStatus :: Bool -> m CatchUpStatus

    -- | Get the 'RuntimeParameters'.
    getRuntimeParameters :: m RuntimeParameters
    default getRuntimeParameters :: (TS.TreeStateMonad m) => m RuntimeParameters
    getRuntimeParameters = TS.getRuntimeParameters

    -- | Determine if consensus has been shut down. This is the case if a protocol update has
    --  taken effect as of the last finalized block.
    isShutDown :: m Bool

    -- | Return the current protocol update, or any pending updates if none has
    --  yet taken effect.
    getProtocolUpdateStatus :: m ProtocolUpdateStatus

    getConsensusStatistics :: m ConsensusStatistics
    default getConsensusStatistics :: (TS.TreeStateMonad m) => m ConsensusStatistics
    getConsensusStatistics = TS.getConsensusStatistics

    -- | Verify a transaction that was received separately from a block.
    --  The return value consists of:
    --
    --  * A 'Bool' that is 'True' if the transaction is already in the non-finalized pool.
    --
    --  * The 'TV.VerificationResult' of verifying the transaction.
    preverifyTransaction :: BlockItem -> m (Bool, TV.VerificationResult)

data MessageType
    = MessageBlock
    | MessageFinalization
    | MessageFinalizationRecord
    | MessageCatchUpStatus
    deriving (Eq, Show)

-- | Wrapper for a 'PendingBlock' that has been received and is now ready for execution.
newtype VerifiedPendingBlock = VerifiedPendingBlock PendingBlock

class (SkovQueryMonad m, TimeMonad m, MonadLogger m) => SkovMonad m where
    -- | Receive a block from the network.
    --  This checks the authenticity of the block itself but no transactions are verified yet.
    --
    --  A 'Just VerifiedPendingBlock' will be returned only if the parent of the block is live
    --  and the metadata of the block could be verified.
    --
    --  The caller is then expected to invoke 'executeBlock' with the returned 'VerifiedPendingBlock' if present.
    --
    --  Note. As `receiveBlock` and 'executeBlock' both acquire and release a write lock on the state then
    --  it is possible that the state has changed in between the two calls, but this is OK as
    --  'executeBlock' must always ensure that the parent of the 'PendingBlock' is alive.
    --  'receiveBlock' checks the following:
    --   - Consensus is running.
    --   - The block is not too early.
    --   - Whether the block is a duplicate.
    --   - Whether the block is stale (the parent is either dead or the parent is older than the last finalized block)
    --   - If the parent of the block is alive then:
    --       - Check the claimed signature on the block.
    --       - Check that the slot number exceeds the parent.
    --       - Check that the block baker is valid.
    --       - Check that the baker key matches the one claimed in the block.
    --       - Check that the block proof is valid (i.e. that the baker is entitled to bake the block for the given slot)
    --   - If the parent block is pending (awaiting its parent to become alive)
    --       - If possible check the claimed baker key is valid in the baking committee.
    --       - If possible check that the block proof is valid (i.e. that the baker is entitled to bake the block for the given slot).
    --       - Check that block the claimed signature is valid and stems from a valid baker.
    --       - Check that the transactions can be verified with reference to the last finalized block.
    receiveBlock :: PendingBlock -> m (UpdateResult, Maybe VerifiedPendingBlock)

    -- | Inserts a 'PendingBlock' given the provided 'VerifiedPendingBlock'.
    --  Execute block must check that the parent block is alive.
    --  before adding the block to the tree.
    executeBlock :: VerifiedPendingBlock -> m UpdateResult

    -- | Receive and execute a 'PendingBlock'. This is used for importing blocks into the tree.
    receiveExecuteBlock :: PendingBlock -> m UpdateResult

    -- | Add a transaction to the transaction table.
    --  This must gracefully handle transactions from other (older) protocol versions.
    receiveTransaction :: BlockItem -> m UpdateResult

    -- | Add a transaction that has previously been verified (by 'preverifyTransaction') to the
    --  transaction table. This must gracefully handle transactions from other (older) protocol
    --  versions. However, if the verification result was derived from a previous Skov instance,
    --  the caller is responsible for ensuring that the verification result is still applicable to
    --  the new instance.
    addPreverifiedTransaction :: BlockItem -> TV.OkResult -> m UpdateResult

    -- | Finalize a block where the finalization record is known to be for the
    --  next finalization index and have a valid finalization proof.  This
    --  checks that the block being finalized is live.
    --   * If the block being finalized is live, it is finalized and the block pointer is returned.
    --   * If the block is already finalized or dead, 'ResultInvalid' is returned
    --   * If the block is unknown or pending, 'ResultUnverifiable' is returned.
    --  Note that this function is intended to be called by the finalization implementation,
    --  and will not call the finalization implementation itself.
    trustedFinalize :: FinalizationRecord -> m (Either UpdateResult (BlockPointerType m))

    -- | Handle a catch-up status message.
    handleCatchUpStatus :: CatchUpStatus -> Int -> m (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)

    -- | Clean up the Skov state upon a protocol update, removing all blocks that
    --  are made obsolete by the protocol update. This should maintain all
    --  invariants normally maintained by the Skov state, e.g., transaction table
    --  invariants.
    clearSkovOnProtocolUpdate :: m ()

    -- | Release any resources maintained by Skov and no longer needed after a
    --  new skov instance is started after a protocol update. This is intended to be called
    --  **after** 'clearSkovOnProtocolUpdate'.
    terminateSkov :: m ()

    -- | Purge uncommitted transactions from the transaction table.  This can be called
    --  periodically to clean up transactions that are not committed to any block.
    purgeTransactions :: m ()

    -- | Record the final block state, derived from the last finalized block to
    --  prepare for the construction of the new genesis for the chain after the
    --  protocol update. This state is not associated with any specific block of
    --  the chain. This function is only meant to be used during a protocol
    --  update.
    rememberFinalState :: BlockState m -> m ()

instance (Monad (t m), MonadTrans t, SkovQueryMonad m) => SkovQueryMonad (MGSTrans t m) where
    resolveBlock = lift . resolveBlock
    isBlockKnownAndLive = lift . isBlockKnownAndLive
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    lastFinalizedBlockWithRecord = lift lastFinalizedBlockWithRecord
    blockAtFinIndex = lift . blockAtFinIndex
    recordAtFinIndex = lift . recordAtFinIndex
    nextFinalizationIndex = lift nextFinalizationIndex
    getGenesisData = lift getGenesisData
    genesisBlock = lift genesisBlock
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop
    getBlocksAtHeight = lift . getBlocksAtHeight
    getFirstFinalizedOfEpoch = lift . getFirstFinalizedOfEpoch
    queryBlockState = lift . queryBlockState
    queryTransactionStatus = lift . queryTransactionStatus
    queryNonFinalizedTransactions = lift . queryNonFinalizedTransactions
    queryNumberOfNonFinalizedTransactions = lift queryNumberOfNonFinalizedTransactions
    queryNextAccountNonce = lift . queryNextAccountNonce
    blockLastFinalizedIndex = lift . blockLastFinalizedIndex
    getCatchUpStatus = lift . getCatchUpStatus
    getRuntimeParameters = lift getRuntimeParameters
    isShutDown = lift isShutDown
    getProtocolUpdateStatus = lift getProtocolUpdateStatus
    getConsensusStatistics = lift getConsensusStatistics
    preverifyTransaction = lift . preverifyTransaction

{- - INLINE resolveBlock - -}
{- - INLINE isFinalized - -}
{- - INLINE lastFinalizedBlock - -}
{- - INLINE lastFinalizedBlockWithRecord - -}
{- - INLINE blockAtFinIndex - -}
{- - INLINE recordAtFinIndex - -}
{- - INLINE getBirkParameters - -}
{- - INLINE getGenesisData - -}
{- - INLINE genesisBlock - -}
{- - INLINE getCurrentHeight - -}
{- - INLINE branchesFromTop - -}
{- - INLINE getBlocksAtHeight - -}
{- - INLINE queryBlockState - -}
{- - INLINE queryTransactionStatus - -}
{- - INLINE queryNonFinalizedTransactions - -}
{- - INLINE queryNextAccountNonce - -}
{- - INLINE blockLastFinalizedIndex - -}
{- - INLINE getCatchUpStatus - -}
{- - INLINE getRuntimeParameters - -}

deriving via (MGSTrans MaybeT m) instance (SkovQueryMonad m) => SkovQueryMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance (SkovQueryMonad m) => SkovQueryMonad (ExceptT e m)

instance (MonadLogger (t m), MonadTrans t, SkovMonad m) => SkovMonad (MGSTrans t m) where
    receiveBlock = lift . receiveBlock
    executeBlock = lift . executeBlock
    receiveExecuteBlock = lift . receiveExecuteBlock
    receiveTransaction = lift . receiveTransaction
    addPreverifiedTransaction bi res = lift $ addPreverifiedTransaction bi res
    trustedFinalize = lift . trustedFinalize
    handleCatchUpStatus peerCUS = lift . handleCatchUpStatus peerCUS
    terminateSkov = lift terminateSkov
    clearSkovOnProtocolUpdate = lift clearSkovOnProtocolUpdate
    purgeTransactions = lift purgeTransactions

    rememberFinalState = lift . rememberFinalState

deriving via (MGSTrans MaybeT m) instance (SkovMonad m) => SkovMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance (SkovMonad m) => SkovMonad (ExceptT e m)

-- | Get the 'Timestamp' of the genesis block.
getGenesisTime :: (SkovQueryMonad m) => m Timestamp
getGenesisTime = gdGenesisTime <$> getGenesisData

-- | Get the 'FinalizationParameters'.
getFinalizationParameters :: (SkovQueryMonad m) => m FinalizationParameters
getFinalizationParameters = gdFinalizationParameters <$> getGenesisData

-- | Get the 'UTCTime' corresponding to a particular slot.
getSlotTime :: (SkovQueryMonad m) => Slot -> m UTCTime
getSlotTime s = do
    genData <- getGenesisData
    return $ posixSecondsToUTCTime $ 0.001 * (fromIntegral (tsMillis $ gdGenesisTime genData) + fromIntegral (durationMillis $ gdSlotDuration genData) * fromIntegral s)

-- | Perform the monadic action unless the consensus is already shut down.
unlessShutDown :: (SkovQueryMonad m) => m UpdateResult -> m UpdateResult
unlessShutDown a =
    isShutDown >>= \case
        True -> return ResultConsensusShutDown
        False -> a

-- * Generic instance of SkovQueryMonad based on a TreeStateMonad.

newtype SkovQueryMonadT m a = SkovQueryMonadT {runSkovQueryMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SkovQueryMonadT where
    {- - INLINE lift - -}
    lift = SkovQueryMonadT

deriving via (MGSTrans SkovQueryMonadT m) instance (MonadProtocolVersion m) => MonadProtocolVersion (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (GlobalStateTypes m) => GlobalStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (AccountOperations m) => AccountOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (ContractStateOperations m) => ContractStateOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (ModuleQuery m) => ModuleQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (BlockStateQuery m) => BlockStateQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (BlockPointerMonad m) => BlockPointerMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (AccountNonceQuery m) => AccountNonceQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (TS.TreeStateMonad m) => TS.TreeStateMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (BlockStateStorage m) => BlockStateStorage (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (BlockStateOperations m) => BlockStateOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance (TimeMonad m) => TimeMonad (SkovQueryMonadT m)

instance
    ( TS.TreeStateMonad m,
      TimeMonad m,
      IsConsensusV0 (MPV m)
    ) =>
    SkovQueryMonad (SkovQueryMonadT m)
    where
    {- - INLINE resolveBlock - -}
    resolveBlock = lift . doResolveBlock
    isBlockKnownAndLive = lift . doIsBlockKnownAndLive

    {- - INLINE isFinalized - -}
    isFinalized = lift . doIsFinalized

    {- - INLINE blockAtFinIndex - -}
    blockAtFinIndex = lift . TS.getFinalizedAtIndex

    {- - INLINE recordAtFinIndex - -}
    recordAtFinIndex = lift . TS.getRecordAtIndex

    {- - INLINE lastFinalizedBlock - -}
    lastFinalizedBlock = lift (fst <$> TS.getLastFinalized)

    {- - INLINE lastFinalizedBlockWithRecord - -}
    lastFinalizedBlockWithRecord = lift TS.getLastFinalized

    {- - INLINE nextFinalizationIndex - -}
    nextFinalizationIndex = lift TS.getNextFinalizationIndex

    {- - INLINE getGenesisData - -}
    getGenesisData = lift TS.getGenesisData

    {- - INLINE genesisBlock - -}
    genesisBlock = lift TS.getGenesisBlockPointer

    {- - INLINE getCurrentHeight - -}
    getCurrentHeight = lift doGetCurrentHeight

    {- - INLINE branchesFromTop - -}
    branchesFromTop = lift doBranchesFromTop

    {- - INLINE getBlocksAtHeight - -}
    getBlocksAtHeight = lift . doGetBlocksAtHeight

    getFirstFinalizedOfEpoch = lift . doGetFirstFinalizedOfEpoch

    {- - INLINE queryBlockState - -}
    queryBlockState = lift . blockState

    {- - INLINE blockLastFinalizedIndex - -}
    blockLastFinalizedIndex = lift . doBlockLastFinalizedIndex

    {- - INLINE getCatchUpStatus - -}
    getCatchUpStatus = doGetCatchUpStatus

    {- - INLINE queryTransactionStatus - -}
    queryTransactionStatus = lift . TS.lookupTransaction

    {- - INLINE queryNonFinalizedTransactions - -}
    queryNonFinalizedTransactions addr = lift $ do
        txs <- TS.getAccountNonFinalized addr minNonce
        return $! map getHash (concatMap (Map.keys . snd) txs)

    {- - INLINE queryNumberOfNonFinalizedTransactions - -}
    queryNumberOfNonFinalizedTransactions = lift TS.numberOfNonFinalizedTransactions

    {- - INLINE queryNextAccountNonce - -}
    queryNextAccountNonce = lift . getNextAccountNonce

    isShutDown = lift doIsShutDown
    getProtocolUpdateStatus = lift doGetProtocolUpdateStatus

    preverifyTransaction = lift . doVerifyTransaction
