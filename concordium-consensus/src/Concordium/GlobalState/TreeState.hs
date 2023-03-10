{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.TreeState (
    module Concordium.GlobalState.Classes,
    module Concordium.GlobalState.Types,
    module Concordium.GlobalState.Block,
    module Concordium.GlobalState.TreeState,
) where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import Data.Time
import Lens.Micro.Platform

import Concordium.GlobalState.Block (BlockData (..), BlockPendingData (..), PendingBlock (..))
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer (BlockPointerData (..))
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Types
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.Types.Execution (TransactionIndex)
import Concordium.Types.HashableTo
import Concordium.Types.Transactions as Transactions
import Concordium.Types.Updates hiding (getUpdateKeysCollection)

import qualified Concordium.GlobalState.Block as B
import qualified Concordium.TransactionVerification as TVer

data BlockStatus bp pb
    = BlockAlive !bp
    | BlockDead
    | BlockFinalized !bp !FinalizationRecord
    | BlockPending !pb
    deriving (Eq)

instance Show (BlockStatus bp pb) where
    show (BlockAlive _) = "Alive"
    show (BlockDead) = "Dead"
    show (BlockFinalized _ _) = "Finalized"
    show (BlockPending _) = "Pending"

-- |Branches of a tree represented as a sequence of lists of block pointers. All pointers within a
-- list point to blocks of equal height above the last finalized block. The sequence is ordered by
-- the block height. The blocks in the branches should be exactly the live blocks.  If a block is in
-- the branches, then either it is at the lowest level and its parent is the last finalized block,
-- or its parent is also in the branches at the level below.
type Branches m = Seq.Seq [BlockPointerType m]

-- |Result of trying to add a transaction to the transaction table.
data AddTransactionResult
    = -- |Transaction is a duplicate of the given transaction.
      -- Contains the duplicate `BlockItem` and the cached `VerificationResult` iff
      -- the transaction has status `Received` or ´Committed´.
      Duplicate !BlockItem (Maybe TVer.VerificationResult)
    | -- |The transaction was newly added.
      -- Contains the `BlockItem` that was added and the cached `VerificationResult`.
      Added !BlockItem !TVer.VerificationResult
    | -- |The nonce of the transaction is not later than the last finalized transaction for the sender.
      -- The transaction is not added to the table.
      ObsoleteNonce
    | -- |The transaction was not added as it could not be deemed verifiable.
      -- The `NotAdded` contains the `VerificationResult`
      NotAdded !TVer.VerificationResult
    deriving (Eq, Show)

-- |Status of a "recent" block. Recent here means that instead of looking up the
-- full status of a block that is older than the last finalized one, we return
-- only the fact that it is older than last finalized. This performs better in
-- cases where this is the only information that is needed. It avoids loading
-- finalized blocks from the database.
data RecentBlockStatus bp pb
    = -- |The block is either pending, dead, or no older than the last finalized block.
      RecentBlock !(BlockStatus bp pb)
    | -- |The block is known, and is strictly older than the last finalized block.
      OldFinalized
    | -- |The block is unknown
      Unknown
    deriving (Eq, Show)

-- |Status of a transaction.
data TransactionStatus
    = -- |Transaction is either pending (i.e. in no blocks) or committed (i.e. in a live block).
      Live !LiveTransactionStatus
    | -- |Transaction is finalized in a given block.
      Finalized
        { -- |The commit point of the block that that the transaction is part of.
          ftsCommitPoint :: !CommitPoint,
          -- |The hash of the block that this transaction is part of.
          ftsBlockHash :: !BlockHash,
          -- |Index of the transaction in the finalized block.
          -- The 'TransactionIndex' can be used to query the outcome
          -- via the associated block state.
          ftsFinResult :: !TransactionIndex
        }

-- |Monad that provides operations for working with the low-level tree state.
-- These operations are abstracted where possible to allow for a range of implementation
-- choices.
class
    ( Eq (BlockPointerType m),
      Ord (BlockPointerType m),
      HashableTo BlockHash (BlockPointerType m),
      BlockData (BlockPointerType m),
      BlockPointerData (BlockPointerType m),
      BlockStateStorage m,
      BlockPointerMonad m,
      B.EncodeBlock (MPV m) (BlockPointerType m),
      Monad m,
      MonadProtocolVersion m
    ) =>
    TreeStateMonad m
    where
    -- * 'PendingBlock' operations

    -- |Create and sign a 'PendingBlock`.
    makePendingBlock ::
        -- |Key for signing the new block
        BakerSignPrivateKey ->
        -- |Block slot (must be non-zero)
        Slot ->
        -- |Hash of parent block
        BlockHash ->
        -- |Identifier of block baker
        BakerId ->
        -- |Block proof
        BlockProof ->
        -- |Block nonce
        BlockNonce ->
        -- |Finalization data
        BlockFinalizationData ->
        -- |List of transactions
        [BlockItem] ->
        -- |Statehash of the block.
        StateHash ->
        -- |TransactionOutcomesHash of block.
        TransactionOutcomesHash ->
        -- |Block receive time
        UTCTime ->
        m PendingBlock

    -- * Operations on the block table

    -- |Get the current status of a block.
    getBlockStatus :: BlockHash -> m (Maybe (BlockStatus (BlockPointerType m) PendingBlock))

    -- |Get a 'RecentBlockStatus'. See documentation of 'RecentBlockStatus' for
    -- the meaning of the return value.
    getRecentBlockStatus :: BlockHash -> m (RecentBlockStatus (BlockPointerType m) PendingBlock)

    -- |Make a live 'BlockPointer' from a 'PendingBlock'.
    -- The parent and last finalized pointers must be correct.
    makeLiveBlock ::
        -- |Block to make live
        PendingBlock ->
        -- |Parent block pointer
        BlockPointerType m ->
        -- |Last finalized block pointer
        BlockPointerType m ->
        -- |Block state
        BlockState m ->
        -- |Block arrival time
        UTCTime ->
        -- |Energy cost of the transactions in the block.
        Energy ->
        m (BlockPointerType m)

    -- |Mark a block as dead. This should only be used directly if there are no other state invariants
    -- which should be maintained. See 'markLiveBlockDead' for an alternative method which maintains more invariants.
    markDead :: BlockHash -> m ()

    -- |Mark a live block as dead. In addition, purge the block state and maintain invariants in the
    -- transaction table by purging all transaction outcomes that refer to this block.
    -- This has a default implementation in terms of 'markDead', 'purgeBlockState' and 'markDeadTransaction'.
    markLiveBlockDead :: BlockPointerType m -> m ()
    markLiveBlockDead bp = do
        let bh = getHash bp
        -- Mark the block dead
        markDead bh
        -- remove the block state
        purgeBlockState =<< blockState bp
        -- and remove the status of all transactions in this block
        mapM_ (markDeadTransaction bh) (blockTransactions bp)

    -- | Depending on the implementation, `markFinalized` may return a value of this type. The
    -- primary intent is to allow the persistent implementation to pass a serialized block to
    -- `wrapupFinalization` which will write it to disk, without relying on monadic state.
    type MarkFin m

    -- |Mark a block as finalized (by a particular 'FinalizationRecord').
    --
    -- Precondition: The block must be alive.
    --
    -- The finalization is considered fully done when 'wrapUpFinalization' is
    -- called. In between calling 'markFinalized' and 'wrapUpFinalization' calls
    -- to 'getBlockStatus' may return inconsistent results and thus should not
    -- be used.
    markFinalized :: BlockHash -> FinalizationRecord -> m (MarkFin m)

    -- |Mark a block as pending (i.e. awaiting parent)
    markPending :: PendingBlock -> m ()

    -- * Queries on genesis block

    -- |Get the genesis 'BlockPointer'.
    getGenesisBlockPointer :: m (BlockPointerType m)

    -- |Get the 'GenesisData'.
    getGenesisData :: m GenesisConfiguration

    -- * Operations on the finalization list

    -- |Get the last finalized block.
    getLastFinalized :: m (BlockPointerType m, FinalizationRecord)

    -- |Get the slot number of the last finalized block
    getLastFinalizedSlot :: m Slot
    getLastFinalizedSlot = blockSlot . fst <$> getLastFinalized

    -- |Get the height of the last finalized block
    getLastFinalizedHeight :: m BlockHeight
    getLastFinalizedHeight = bpHeight . fst <$> getLastFinalized

    -- |Get the next finalization index.
    getNextFinalizationIndex :: m FinalizationIndex
    getNextFinalizationIndex = (+ 1) . finalizationIndex . snd <$> getLastFinalized

    -- |Add a block and finalization record to the finalization list.
    -- The block must be the one finalized by the record, and the finalization
    -- index must be the next finalization index.  These are not checked.
    addFinalization :: BlockPointerType m -> FinalizationRecord -> m ()

    -- |Get the block that is finalized at the given index.
    -- Returns 'Nothing' if no such block exists.
    getFinalizedAtIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m))

    -- |Get the finalization record at the given index, if any.
    getRecordAtIndex :: FinalizationIndex -> m (Maybe FinalizationRecord)

    -- |Get the block that is finalized at the given height, if any.
    getFinalizedAtHeight :: BlockHeight -> m (Maybe (BlockPointerType m))

    -- |Persist finalization, if the tree state implementation supports it
    wrapupFinalization :: FinalizationRecord -> [(MarkFin m, FinTrans m)] -> m ()

    -- * Operations on branches

    -- |Get the branches.
    getBranches :: m (Branches m)

    -- |Set the branches.
    putBranches :: Branches m -> m ()

    -- * Operations on blocks that are pending the arrival of other blocks

    --

    -- $pendingBlocks
    -- Pending blocks are conceptually stored in a min priority search queue,
    -- where multiple blocks may have the same key, which is their parent,
    -- and the priority is the block's slot number.
    -- When a block arrives (possibly dead), its pending children are removed
    -- from the queue and handled.  This uses 'takePendingChildren'.
    -- When a block is finalized, all pending blocks with a lower or equal slot
    -- number can be handled (they will become dead, since they can no longer
    -- join the tree).  This uses 'takeNextPendingUntil'.
    --
    -- |Return a list of the blocks that are pending the given parent block,
    -- removing them from the pending table.
    takePendingChildren :: BlockHash -> m [PendingBlock]

    -- |Add a pending block, that is pending on the arrival of its parent.
    addPendingBlock :: PendingBlock -> m ()

    -- |Return the next block that is pending its parent with slot number
    -- less than or equal to the given value, removing it from the pending
    -- table.  Returns 'Nothing' if there is no such pending block.
    takeNextPendingUntil :: Slot -> m (Maybe PendingBlock)

    -- * Operations on the pending transaction table

    --

    -- $pendingTransactions
    -- We maintain a 'PendingTransactionTable' for a particular block that is
    -- the focus block.  (Ideally, this should be the best block, however, it
    -- shouldn't be a problem if it's not.)
    -- |Return the focus block.
    getFocusBlock :: m (BlockPointerType m)

    -- |Update the focus block.
    putFocusBlock :: BlockPointerType m -> m ()

    -- |Get the pending transactions after execution of the focus block.
    getPendingTransactions :: m PendingTransactionTable

    -- |Set the pending transactions after execution of the focus block.
    putPendingTransactions :: PendingTransactionTable -> m ()

    -- * Operations on the transaction table

    -- |Get non-finalized transactions for the given account starting at the given nonce (inclusive).
    -- These are returned as an ordered list of pairs of nonce and non-empty set of transactions
    -- with that nonce. Transaction groups are ordered by increasing nonce.
    getAccountNonFinalized ::
        AccountAddressEq ->
        Nonce ->
        m [(Nonce, Map.Map Transaction TVer.VerificationResult)]

    -- |Get the successor of the largest known account for the given account
    -- The function should return 'True' in the second component if and only if
    -- all (known) transactions from this account are finalized.
    getNextAccountNonce :: AccountAddressEq -> m (Nonce, Bool)

    -- |Get a credential which has not yet been finalized, i.e., it is correct for this function
    -- to return 'Nothing' if the requested credential has already been finalized.
    getCredential :: TransactionHash -> m (Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult))

    -- |Get non-finalized chain updates of a given type starting at the given sequence number
    -- (inclusive). This are returned as an ordered list of pairs of sequence number and
    -- non-empty set of updates with that sequence number. Update groups are ordered by
    -- increasing sequence number.
    getNonFinalizedChainUpdates ::
        UpdateType ->
        UpdateSequenceNumber ->
        m [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]

    -- | Depending on the implementation, `finalizeTransactions` may return a value of this
    -- type. The primary intent is to allow the persistent implementation to pass a list of
    -- transaction hashes and statuses to `wrapupFinalization` which will write them to disk,
    -- without relying on monadic state.
    type FinTrans m

    -- |Finalize a list of transactions on a given block. Per account, the transactions must be in
    -- continuous sequence by nonce, starting from the next available non-finalized
    -- nonce.
    finalizeTransactions :: BlockHash -> Slot -> [BlockItem] -> m (FinTrans m)

    -- |Mark a transaction as committed on a block with the given slot number,
    -- as well as add any additional outcomes for the given block (outcomes are given
    -- as the index of the transaction in the given block).
    -- This will prevent it from being purged while the slot number exceeds
    -- that of the last finalized block.
    commitTransaction :: Slot -> BlockHash -> BlockItem -> TransactionIndex -> m ()

    -- |@addCommitTransaction tr verResCtx timestamp slot@ verifies a transaction within the
    -- given context and adds the transaction and marks it committed
    -- for the given slot number. If the transaction was deemed verifiable in the future.
    -- By default the transaction is created in the 'Received' state,
    -- but if the transaction is already in the table the outcomes are retained.
    -- See documentation of 'AddTransactionResult' for meaning of the return value.
    -- The time is indicative of the receive time of the transaction. It is used to prioritize transactions
    -- when constructing a block.
    -- Before the transaction is added the transaction will be verified. If it is not ok then it will return
    -- `NotAdded` together with the verification result.
    -- The scheduler will need to consult the resulting `VerificationResult` and based on that carry out the correct
    -- verification before executing the transaction.
    addCommitTransaction :: BlockItem -> Context (BlockState m) -> Timestamp -> Slot -> m AddTransactionResult

    -- |Add a transaction that has already been verified with the supplied verification result.
    -- This is called for adding transactions that are not already part of a block, so the
    -- transaction will not become committed to any block.
    -- By default the transaction is created in the 'Received' state,
    -- but if the transaction is already in the table the outcomes are retained.
    -- See documentation of 'AddTransactionResult' for meaning of the return value.
    addVerifiedTransaction :: BlockItem -> TVer.OkResult -> m AddTransactionResult

    -- |Purge a transaction from the transaction table if its last committed slot
    -- number does not exceed the slot number of the last finalized block.
    -- (A transaction that has been committed to a finalized block should not be purged.)
    -- Returns @True@ if and only if the transaction is purged.
    purgeTransaction :: BlockItem -> m Bool

    -- |Get the `VerificationResult` for a `BlockItem` if such one exist.
    -- A `VerificationResult` exists for `Received` and `Committed` transactions while
    -- finalized transactions will yield a `Nothing`.
    getNonFinalizedTransactionVerificationResult :: BlockItem -> m (Maybe TVer.VerificationResult)

    -- |Purge transactions from the transaction table and pending transactions.
    -- Transactions are purged only if they are not included in a live block, and
    -- have either expired or arrived longer ago than the transaction keep alive time.
    --
    -- If the first argument is @False@, the transaction table is only purged if
    -- 'rpInsertionsBeforeTransactionPurged' transactions have been inserted since
    -- the last purge.  If it is true, the table is purged regardless.
    --
    -- WARNING: This function violates the independence of the tree state components.
    -- In particular, the following invariants are assumed and maintained:
    --
    --   * Every 'BlockItem' in the transaction table that is not included in a live
    --     or finalized block is referenced in the pending transaction table.  That is,
    --     for a basic transaction the '_pttWithSender' table contains an entry for
    --     the sender where the nonce of the transaction falls within the range,
    --     and for a credential deployment the transaction hash is included in '_pttDeployCredential'.
    --
    --   * The low nonce for each entry in '_pttWithSender' is at least the last finalized
    --     nonce recorded in the account's non-finalized transactions in the transaction
    --     table.
    --
    --   * The finalization list must reflect the current last finalized block.
    --
    --   * The pending transaction table only references transactions that are in the
    --     transaction table.  That is, the high nonce in a range is a tight bound and
    --     the deploy credential hashes correspond to transactions in the table.
    --
    --   * No non-finalized block is considered live or will become live if its slot
    --     is less than or equal to the slot number of the last finalized block.
    --
    --   * If a transaction is known to be in any block that is not finalized or dead,
    --     then 'commitTransaction' or 'addCommitTransaction' has been called with a
    --     slot number at least as high as the slot number of the block.
    purgeTransactionTable ::
        -- | Whether to ignore the amount of insertions and forcedly perform a purge
        Bool ->
        -- | Current time
        UTCTime ->
        m ()

    -- |Mark a transaction as no longer on a given block. This is used when a block is
    -- marked as dead.
    markDeadTransaction :: BlockHash -> BlockItem -> m ()

    -- |Lookup a transaction status by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe TransactionStatus)

    -- |Get the number of non-finalized transactions stored in the transaction table.
    numberOfNonFinalizedTransactions :: m Int

    -- * Operations on statistics

    -- |Get the current consensus statistics.
    getConsensusStatistics :: m ConsensusStatistics

    -- |Set the consensus statistics.
    putConsensusStatistics :: ConsensusStatistics -> m ()

    -- |Get other runtime parameters that are implementation detail, and hence do
    -- not belong to genesis data.
    getRuntimeParameters :: m RuntimeParameters

    -- * Operations related to shutdown upon protocol update.

    -- |Remove all blocks that are made redundant by the protocol update
    -- taking effect. This is intended to be called after the first block
    -- after the effective time is finalized. It will
    --
    --   - Clear all non-finalized blocks from the block table.
    --   - Remove all blocks from the pending table.
    --   - Mark all non-finalized but committed transactions
    clearOnProtocolUpdate :: m ()

    -- |Do any cleanup of resources that are no longer needed after the protocol
    -- update has been processed.
    clearAfterProtocolUpdate :: m ()

    -- |Record the final block state, derived from the last finalized block to
    -- prepare for the construction of the new genesis for the chain after the
    -- protocol update. This state is not associated with any specific block of
    -- the chain. This function is only meant to be used during a protocol
    -- update.
    storeFinalState :: BlockState m -> m ()

instance (Monad (t m), MonadTrans t, TreeStateMonad m) => TreeStateMonad (MGSTrans t m) where
    makePendingBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash = lift . makePendingBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash
    getBlockStatus = lift . getBlockStatus
    getRecentBlockStatus = lift . getRecentBlockStatus
    makeLiveBlock b parent lastFin st time = lift . makeLiveBlock b parent lastFin st time
    markDead = lift . markDead
    type MarkFin (MGSTrans t m) = MarkFin m
    markFinalized bh = lift . markFinalized bh
    markPending = lift . markPending
    getGenesisBlockPointer = lift getGenesisBlockPointer
    getGenesisData = lift getGenesisData
    getLastFinalized = lift getLastFinalized
    getLastFinalizedSlot = lift getLastFinalizedSlot
    getLastFinalizedHeight = lift getLastFinalizedHeight
    getNextFinalizationIndex = lift getNextFinalizationIndex
    addFinalization bp = lift . addFinalization bp
    getFinalizedAtIndex = lift . getFinalizedAtIndex
    getRecordAtIndex = lift . getRecordAtIndex
    getFinalizedAtHeight = lift . getFinalizedAtHeight
    wrapupFinalization finRec = lift . wrapupFinalization finRec
    getBranches = lift getBranches
    putBranches = lift . putBranches
    takePendingChildren = lift . takePendingChildren
    addPendingBlock = lift . addPendingBlock
    takeNextPendingUntil = lift . takeNextPendingUntil
    getFocusBlock = lift getFocusBlock
    putFocusBlock = lift . putFocusBlock
    getPendingTransactions = lift getPendingTransactions
    putPendingTransactions = lift . putPendingTransactions
    getAccountNonFinalized acc = lift . getAccountNonFinalized acc
    getNextAccountNonce = lift . getNextAccountNonce
    getCredential = lift . getCredential
    getNonFinalizedChainUpdates uty = lift . getNonFinalizedChainUpdates uty
    type FinTrans (MGSTrans t m) = FinTrans m
    finalizeTransactions bh slot = lift . finalizeTransactions bh slot
    commitTransaction slot bh tr = lift . commitTransaction slot bh tr
    addCommitTransaction tr ctx ts slot = lift $ addCommitTransaction tr ctx ts slot
    addVerifiedTransaction tr vr = lift $ addVerifiedTransaction tr vr
    purgeTransaction = lift . purgeTransaction
    markDeadTransaction bh = lift . markDeadTransaction bh
    lookupTransaction = lift . lookupTransaction
    numberOfNonFinalizedTransactions = lift numberOfNonFinalizedTransactions
    getConsensusStatistics = lift getConsensusStatistics
    putConsensusStatistics = lift . putConsensusStatistics
    getRuntimeParameters = lift getRuntimeParameters
    purgeTransactionTable tm = lift . (purgeTransactionTable tm)
    getNonFinalizedTransactionVerificationResult = lift . getNonFinalizedTransactionVerificationResult
    clearOnProtocolUpdate = lift clearOnProtocolUpdate
    clearAfterProtocolUpdate = lift clearAfterProtocolUpdate
    storeFinalState = lift . storeFinalState
    {-# INLINE makePendingBlock #-}
    {-# INLINE getBlockStatus #-}
    {-# INLINE makeLiveBlock #-}
    {-# INLINE markDead #-}
    {-# INLINE markFinalized #-}
    {-# INLINE markPending #-}
    {-# INLINE getGenesisBlockPointer #-}
    {-# INLINE getGenesisData #-}
    {-# INLINE getLastFinalized #-}
    {-# INLINE getLastFinalizedSlot #-}
    {-# INLINE getLastFinalizedHeight #-}
    {-# INLINE getNextFinalizationIndex #-}
    {-# INLINE addFinalization #-}
    {-# INLINE getFinalizedAtIndex #-}
    {-# INLINE getRecordAtIndex #-}
    {-# INLINE getFinalizedAtHeight #-}
    {-# INLINE getBranches #-}
    {-# INLINE putBranches #-}
    {-# INLINE takePendingChildren #-}
    {-# INLINE addPendingBlock #-}
    {-# INLINE takeNextPendingUntil #-}
    {-# INLINE getFocusBlock #-}
    {-# INLINE putFocusBlock #-}
    {-# INLINE getPendingTransactions #-}
    {-# INLINE putPendingTransactions #-}
    {-# INLINE getAccountNonFinalized #-}
    {-# INLINE getNextAccountNonce #-}
    {-# INLINE getCredential #-}
    {-# INLINE getNonFinalizedChainUpdates #-}
    {-# INLINE finalizeTransactions #-}
    {-# INLINE commitTransaction #-}
    {-# INLINE addCommitTransaction #-}
    {-# INLINE addVerifiedTransaction #-}
    {-# INLINE purgeTransaction #-}
    {-# INLINE lookupTransaction #-}
    {-# INLINE numberOfNonFinalizedTransactions #-}
    {-# INLINE markDeadTransaction #-}
    {-# INLINE getConsensusStatistics #-}
    {-# INLINE putConsensusStatistics #-}
    {-# INLINE getRuntimeParameters #-}
    {-# INLINE purgeTransactionTable #-}
    {-# INLINE getNonFinalizedTransactionVerificationResult #-}
    {-# INLINE clearOnProtocolUpdate #-}
    {-# INLINE clearAfterProtocolUpdate #-}

deriving via (MGSTrans MaybeT m) instance TreeStateMonad m => TreeStateMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance TreeStateMonad m => TreeStateMonad (ExceptT e m)

-- |The Context that a transaction is verified within
-- in the reader based instance.
-- The `Context` contains the `BlockState`, the maximum energy of a block and
-- also whether the transaction was received individually or as part of a block.
--
-- The `Context` is used for verifying the transaction in a deferred manner.
-- That is, the verification process will only take place if the transaction is not already contained
-- in the `TransactionTable`.
-- Note. The `Context` is created when the transaction is received by `doReceiveTransactionInternal` and
-- the actual verification is carried out within `addCommitTransaction` when it has been checked
-- that the transaction does not already exist in the `TransactionTable`.
data Context t = Context
    { _ctxBs :: t,
      _ctxMaxBlockEnergy :: !Energy,
      -- |Whether the transaction was received from a block or individually.
      _isTransactionFromBlock :: Bool
    }

makeLenses ''Context

-- |Helper type for defining 'TransactionVerifierT'. While we only instantiate @r@ with
-- @Context (BlockState m)@, it is simpler to derive the 'MonadTrans' instance using the present
-- definition.
newtype TransactionVerifierT' (r :: Type) (m :: Type -> Type) (a :: Type) = TransactionVerifierT {runTransactionVerifierT :: r -> m a}
    deriving (Functor, Applicative, Monad, MonadReader r) via (ReaderT r m)
    deriving (MonadTrans) via (ReaderT r)

deriving via (ReaderT r) m instance (MonadProtocolVersion m) => MonadProtocolVersion (TransactionVerifierT' r m)
deriving via (ReaderT r) m instance BlockStateTypes (TransactionVerifierT' r m)

type TransactionVerifierT m = TransactionVerifierT' (Context (BlockState m)) m

instance
    ( TreeStateMonad m,
      r ~ Context (BlockState m)
    ) =>
    TVer.TransactionVerifier (TransactionVerifierT' r m)
    where
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider ipId = do
        ctx <- ask
        lift (getIdentityProvider (ctx ^. ctxBs) ipId)
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers arrIds = do
        ctx <- ask
        lift (getAnonymityRevokers (ctx ^. ctxBs) arrIds)
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = do
        ctx <- ask
        lift (getCryptographicParameters (ctx ^. ctxBs))
    {-# INLINE registrationIdExists #-}
    registrationIdExists regId = do
        ctx <- ask
        lift $ isJust <$> getAccountByCredId (ctx ^. ctxBs) (ID.toRawCredRegId regId)
    {-# INLINE getAccount #-}
    getAccount aaddr = do
        ctx <- ask
        fmap snd <$> lift (getAccount (ctx ^. ctxBs) aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = do
        ctx <- ask
        lift (getNextUpdateSequenceNumber (ctx ^. ctxBs) uType)
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = do
        ctx <- ask
        lift (getUpdateKeysCollection (ctx ^. ctxBs))
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce acc = do
        ctx <- ask
        -- If the transaction was received as part of a block
        -- then we check the account nonce from the `BlockState` in the context
        -- Otherwise if the transaction was received individually then we
        -- check the transaction table for the nonce.
        if ctx ^. isTransactionFromBlock
            then lift (getAccountNonce acc)
            else do
                aaddr <- lift (getAccountCanonicalAddress acc)
                lift (fst <$> getNextAccountNonce (accountAddressEmbed aaddr))
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        ctx <- ask
        rate <- lift $ _erEnergyRate <$> getExchangeRates (ctx ^. ctxBs)
        return (computeCost rate v)
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        ctx <- ask
        return (ctx ^. ctxMaxBlockEnergy)
    {-# INLINE checkExactNonce #-}
    checkExactNonce = do
        ctx <- ask
        pure $ not (ctx ^. isTransactionFromBlock)
