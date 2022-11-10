{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.Skov.Monad(
    module Concordium.Skov.CatchUp.Types,
    module Concordium.Skov.Monad
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Concordium.Skov.Query

import Concordium.Types
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus)
import Concordium.GlobalState.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState (BlockStateQuery, AccountOperations, BlockStateStorage, BlockStateOperations, ContractStateOperations, ModuleQuery)
import Concordium.GlobalState.Statistics (ConsensusStatistics)
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Classes as C
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.CatchUp.Types
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.TransactionVerification as TV

data UpdateResult
    = ResultSuccess
    -- ^Message received, validated and processed
    | ResultSerializationFail
    -- ^Message deserialization failed
    | ResultInvalid
    -- ^The message was determined to be invalid
    | ResultPendingBlock
    -- ^The message was received, but is awaiting a block to complete processing
    | ResultPendingFinalization
    -- ^The message was received, but is awaiting a finalization record to complete processing
    | ResultAsync
    -- ^The message was received, but is being processed asynchronously
    | ResultDuplicate
    -- ^The message duplicates a previously received message
    | ResultStale
    -- ^The message may have been valid in the past, but is no longer relevant
    | ResultDuplicateNonce
    -- ^The sequence number for that account or update type was already used in another transaction
    | ResultNonceTooLarge
    -- ^The sequence number for that account or update type is larger than the next sequence number
    | ResultNonexistingSenderAccount
    -- ^An account corresponding to the transaction's sender does not exist in the focus block
    | ResultVerificationFailed
    -- ^Verifying the transaction's signature failed
    | ResultExpiryTooLate
    -- ^The transaction expiry time is too far in the future
    | ResultTooLowEnergy
    -- ^The stated transaction energy is too low
    | ResultIncorrectFinalizationSession
    -- ^The message refers to a different/unknown finalization session
    | ResultUnverifiable
    -- ^The message could not be validated with the current state
    | ResultContinueCatchUp
    -- ^The peer should be marked as pending unless catch up is already in progress
    | ResultEarlyBlock
    -- ^The block was sent too early and should be dropped
    | ResultMissingImportFile
    -- ^The file provided for importing blocks is missing
    | ResultConsensusShutDown
    -- ^The message was not processed because consensus has been shut down
    | ResultInvalidGenesisIndex
    -- ^The message is for an unknown genesis index
    | ResultDuplicateAccountRegistrationID
    -- ^An account already exists to the corresponding registration id of the 'CredentialDeployment'.
    | ResultCredentialDeploymentInvalidIP
    -- ^The identity provider was not valid
    | ResultCredentialDeploymentInvalidAR
    -- ^The anonymity revokers was not valid
    | ResultCredentialDeploymentInvalidSignatures
    -- ^The 'CredentialDeployment' contained invalid identity provider signatures.
    | ResultCredentialDeploymentExpired
    -- ^The 'CredentialDeployment' contained an expired 'validTo'.
    | ResultChainUpdateSequenceNumberTooOld
    -- ^The 'ChainUpdate' contained an invalid nonce
    | ResultChainUpdateInvalidEffectiveTime
    -- ^The 'ChainUpdate' contained an invalid effective time.
    | ResultChainUpdateInvalidSignatures
    -- ^The 'ChainUpdate' contained invalid signatures.
    | ResultEnergyExceeded
    -- ^The stated energy of the 'Transaction' exceeds the maximum allowed.
    | ResultInsufficientFunds
    -- ^The sender did not have enough funds to cover the costs.
    deriving (Eq, Show)

-- |Maps the underlying 'TransactionVerificationResult' to the according 'UpdateResult' type.
-- See the 'VerificationResult' for more information.
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
-- 'NotOk' mappings
transactionVerificationResultToUpdateResult (TV.NotOk (TV.CredentialDeploymentDuplicateAccountRegistrationID _)) = ResultDuplicateAccountRegistrationID
transactionVerificationResultToUpdateResult (TV.NotOk TV.CredentialDeploymentInvalidSignatures) = ResultCredentialDeploymentInvalidSignatures
transactionVerificationResultToUpdateResult (TV.NotOk (TV.ChainUpdateSequenceNumberTooOld _)) = ResultChainUpdateSequenceNumberTooOld
transactionVerificationResultToUpdateResult (TV.NotOk TV.ChainUpdateEffectiveTimeBeforeTimeout) = ResultChainUpdateInvalidEffectiveTime
transactionVerificationResultToUpdateResult (TV.NotOk TV.CredentialDeploymentExpired) = ResultCredentialDeploymentExpired
transactionVerificationResultToUpdateResult (TV.NotOk TV.NormalTransactionDepositInsufficient) = ResultTooLowEnergy
transactionVerificationResultToUpdateResult (TV.NotOk TV.NormalTransactionEnergyExceeded) = ResultEnergyExceeded
transactionVerificationResultToUpdateResult (TV.NotOk (TV.NormalTransactionDuplicateNonce _)) = ResultDuplicateNonce
transactionVerificationResultToUpdateResult (TV.NotOk TV.Expired) = ResultStale
transactionVerificationResultToUpdateResult (TV.NotOk TV.InvalidPayloadSize) = ResultSerializationFail

class (Monad m, Eq (BlockPointerType m), HashableTo BlockHash (BlockPointerType m), BlockPointerData (BlockPointerType m), BlockPointerMonad m, BlockStateQuery m, MonadProtocolVersion m)
        => SkovQueryMonad m where
    -- |Look up a block in the table given its hash.
    resolveBlock :: BlockHash -> m (Maybe (BlockPointerType m))
    -- |Check whether the block is known to us and part of the live tree (i.e.,
    -- not dead or pending). If only block existence is needed then this will be
    -- more efficient than using 'resolveBlock' and checking for a 'Just'
    -- response.
    isBlockKnownAndLive :: BlockHash -> m Bool
    -- |Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointerType m)
    -- |Determine the last finalized block and return it together with the finalization record
    -- that finalizes it..
    lastFinalizedBlockWithRecord :: m (BlockPointerType m, FinalizationRecord)
    -- |Retrieve the finalized block at a given finalization index, if any.
    blockAtFinIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m))
    -- |Retrieve the finalization record at a given finalization index, if any.
    recordAtFinIndex :: FinalizationIndex -> m (Maybe FinalizationRecord)
    -- |Determine the next index for finalization.
    nextFinalizationIndex :: m FinalizationIndex
    -- |Get the genesis configuration.
    getGenesisData :: m GenesisConfiguration
    -- |Get the genesis block pointer.
    genesisBlock :: m (BlockPointerType m)
    -- |Get the height of the highest blocks in the tree.
    -- Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight
    -- |Get the blocks in the branches of the tree grouped by descending height.
    -- That is the first element of the list is all of the blocks at 'getCurrentHeight',
    -- the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockPointerType m]]
    -- |Get a list of all the blocks at a given height in the tree.
    getBlocksAtHeight :: BlockHeight -> m [BlockPointerType m]
    -- |Get a block's state.
    queryBlockState :: BlockPointerType m -> m (BlockState m)
    -- |Get the outcomes of a transaction.
    queryTransactionStatus :: TransactionHash -> m (Maybe TransactionStatus)
    -- |Get non-finalized transactions for an account, ordered by increasing nonce.
    queryNonFinalizedTransactions :: AccountAddressEq -> m [TransactionHash]
    -- |Get best guess for next account nonce.
    -- The second argument is 'True' if and only if all transactions from this account are finalized.
    queryNextAccountNonce :: AccountAddressEq -> m (Nonce, Bool)
    -- |Get the finalization index of a block's last finalized block.
    blockLastFinalizedIndex :: BlockPointerType m -> m FinalizationIndex
    -- |Get a catch-up status message. The flag indicates if the
    -- message should be a catch-up request.
    getCatchUpStatus :: Bool -> m CatchUpStatus
    -- |Get the 'RuntimeParameters'.
    getRuntimeParameters :: m RuntimeParameters
    default getRuntimeParameters :: (TS.TreeStateMonad m) => m RuntimeParameters
    getRuntimeParameters = TS.getRuntimeParameters
    -- |Determine if consensus has been shut down. This is the case if a protocol update has
    -- taken effect as of the last finalized block.
    isShutDown :: m Bool
    -- |Return the current protocol update, or any pending updates if none has
    -- yet taken effect.
    getProtocolUpdateStatus :: m ProtocolUpdateStatus

    getConsensusStatistics :: m ConsensusStatistics
    default getConsensusStatistics :: (TS.TreeStateMonad m) => m ConsensusStatistics
    getConsensusStatistics = TS.getConsensusStatistics

    -- |Verify a transaction that was received separately from a block.
    -- The return value consists of:
    -- 
    -- * A 'Bool' that is 'True' if the transaction is already in the non-finalized pool.
    --
    -- * The 'TV.VerificationResult' of verifying the transaction.
    preverifyTransaction :: BlockItem -> m (Bool, TV.VerificationResult)

data MessageType
    = MessageBlock
    | MessageFinalization
    | MessageFinalizationRecord
    | MessageCatchUpStatus
    deriving (Eq, Show)

class (SkovQueryMonad m, TimeMonad m, MonadLogger m) => SkovMonad m where
    -- |Store a block in the block table and add it to the tree
    -- if possible. This also checks that the block is not early in the sense that its received
    -- time predates its slot time by more than the early block threshold.
    storeBlock :: PendingBlock -> m UpdateResult
    -- |Add a transaction to the transaction table.
    -- This must gracefully handle transactions from other (older) protocol versions.
    receiveTransaction :: BlockItem -> m UpdateResult
    -- |Add a transaction that has previously been verified (by 'preverifyTransaction') to the
    -- transaction table. This must gracefully handle transactions from other (older) protocol
    -- versions. However, if the verification result was derived from a previous Skov instance,
    -- the caller is responsible for ensuring that the verification result is still applicable to
    -- the new instance.

    addPreverifiedTransaction :: BlockItem -> TV.OkResult -> m UpdateResult
    -- |Finalize a block where the finalization record is known to be for the
    -- next finalization index and have a valid finalization proof.  This
    -- checks that the block being finalized is live.
    --  * If the block being finalized is live, it is finalized and the block pointer is returned.
    --  * If the block is already finalized or dead, 'ResultInvalid' is returned
    --  * If the block is unknown or pending, 'ResultUnverifiable' is returned.
    -- Note that this function is intended to be called by the finalization implementation,
    -- and will not call the finalization implementation itself.
    trustedFinalize :: FinalizationRecord -> m (Either UpdateResult (BlockPointerType m))
    -- |Handle a catch-up status message.
    handleCatchUpStatus :: CatchUpStatus -> Int -> m (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)
    -- |Clean up the Skov state upon a protocol update, removing all blocks that
    -- are made obsolete by the protocol update. This should maintain all
    -- invariants normally maintained by the Skov state, e.g., transaction table
    -- invariants.
    clearSkovOnProtocolUpdate :: m ()
    -- |Release any resources maintained by Skov and no longer needed after a
    -- new skov instance is started after a protocol update. This is intended to be called
    -- **after** 'clearSkovOnProtocolUpdate'.
    terminateSkov :: m ()
    -- |Purge uncommitted transactions from the transaction table.  This can be called
    -- periodically to clean up transactions that are not committed to any block.
    purgeTransactions :: m ()

    -- |Record the final block state, derived from the last finalized block to
    -- prepare for the construction of the new genesis for the chain after the
    -- protocol update. This state is not associated with any specific block of
    -- the chain. This function is only meant to be used during a protocol
    -- update.
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
    queryBlockState = lift . queryBlockState
    queryTransactionStatus = lift . queryTransactionStatus
    queryNonFinalizedTransactions = lift . queryNonFinalizedTransactions
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

deriving via (MGSTrans MaybeT m) instance SkovQueryMonad m => SkovQueryMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovQueryMonad m => SkovQueryMonad (ExceptT e m)

instance (MonadLogger (t m), MonadTrans t, SkovMonad m) => SkovMonad (MGSTrans t m) where
    storeBlock b = lift $ storeBlock b
    receiveTransaction = lift . receiveTransaction
    addPreverifiedTransaction bi res = lift $ addPreverifiedTransaction bi res
    trustedFinalize = lift . trustedFinalize
    handleCatchUpStatus peerCUS = lift . handleCatchUpStatus peerCUS
    terminateSkov = lift terminateSkov
    clearSkovOnProtocolUpdate = lift clearSkovOnProtocolUpdate
    purgeTransactions = lift purgeTransactions

    rememberFinalState = lift . rememberFinalState
    {- - INLINE storeBlock - -}
    {- - INLINE receiveTransaction - -}
    {- - INLINE trustedFinalize - -}
    {- - INLINE handleCatchUpStatus - -}

deriving via (MGSTrans MaybeT m) instance SkovMonad m => SkovMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovMonad m => SkovMonad (ExceptT e m)

-- |Get the 'Timestamp' of the genesis block.
getGenesisTime :: (SkovQueryMonad m) => m Timestamp
getGenesisTime = gdGenesisTime <$> getGenesisData

-- |Get the 'FinalizationParameters'.
getFinalizationParameters :: (SkovQueryMonad m) => m FinalizationParameters
getFinalizationParameters = gdFinalizationParameters <$> getGenesisData

-- |Get the 'UTCTime' corresponding to a particular slot.
getSlotTime :: (SkovQueryMonad m) => Slot -> m UTCTime
getSlotTime s = do
        genData <- getGenesisData
        return $ posixSecondsToUTCTime $ 0.001 * (fromIntegral (tsMillis $ gdGenesisTime genData) + fromIntegral (durationMillis $ gdSlotDuration genData) * fromIntegral s)

-- |Perform the monadic action unless the consensus is already shut down.
unlessShutDown :: (SkovQueryMonad m) => m UpdateResult -> m UpdateResult
unlessShutDown a = isShutDown >>= \case
        True -> return ResultConsensusShutDown
        False -> a

-- * Generic instance of SkovQueryMonad based on a TreeStateMonad.

newtype SkovQueryMonadT m a = SkovQueryMonadT { runSkovQueryMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SkovQueryMonadT where
  {- - INLINE lift - -}
  lift = SkovQueryMonadT

deriving via (MGSTrans SkovQueryMonadT m) instance MonadProtocolVersion m => MonadProtocolVersion (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance GlobalStateTypes m => GlobalStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance AccountOperations m => AccountOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance ContractStateOperations m => ContractStateOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance ModuleQuery m => ModuleQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateQuery m => BlockStateQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockPointerMonad m => BlockPointerMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance TS.TreeStateMonad m => TS.TreeStateMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateStorage m => BlockStateStorage (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateOperations m => BlockStateOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance TimeMonad m => TimeMonad (SkovQueryMonadT m)

instance (TS.TreeStateMonad m, TimeMonad m)
          => SkovQueryMonad (SkovQueryMonadT m) where
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

    {- - INLINE queryNextAccountNonce - -}
    queryNextAccountNonce = lift . TS.getNextAccountNonce

    isShutDown = lift doIsShutDown
    getProtocolUpdateStatus = lift doGetProtocolUpdateStatus

    preverifyTransaction = lift . doVerifyTransaction

deriving via SkovQueryMonadT (GlobalStateM pv c r g s m)
      instance (Monad m,
                TimeMonad m,
                MonadProtocolVersion (BlockStateM pv c r g s m),
                BlockStateQuery (BlockStateM pv c r g s m),
                BlockStateStorage (BlockStateM pv c r g s m),
                TS.TreeStateMonad (TreeStateBlockStateM pv g c r s m)
                ) => SkovQueryMonad (GlobalStateM pv c r g s m)
