{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
module Concordium.GlobalState.TreeState(
    module Concordium.GlobalState.Classes,
    module Concordium.GlobalState.Types,
    module Concordium.GlobalState.Block,
    module Concordium.GlobalState.TreeState
) where

import qualified Data.Sequence as Seq
import Data.Time
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Concordium.GlobalState.Block (BlockData(..), BlockPendingData (..), PendingBlock(..))
import Concordium.GlobalState.BlockPointer (BlockPointerData(..))
import Concordium.GlobalState.Types
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions as Transactions
import Concordium.Types.Execution(TransactionIndex)
import Concordium.GlobalState.Statistics
import Concordium.Types.HashableTo
import Concordium.Types
import Concordium.GlobalState.AccountTransactionIndex

data BlockStatus bp pb =
    BlockAlive !bp
    | BlockDead
    | BlockFinalized !bp !FinalizationRecord
    | BlockPending !pb
  deriving(Eq)

instance Show (BlockStatus bp pb) where
    show (BlockAlive _) = "Alive"
    show (BlockDead) = "Dead"
    show (BlockFinalized _ _) = "Finalized"
    show (BlockPending _) = "Pending"

-- |Branches of a tree represented as a sequence, ordered by height above the last
-- finalized block, of lists of block pointers.  The blocks in the branches should
-- be exactly the live blocks.  If a block is in the branches, then either it is at
-- the lowest level and its parent is the last finalized block, or its parent is also
-- in the branches at the level below.
type Branches m = Seq.Seq [BlockPointerType m]

-- |Result of trying to add a transaction to the transaction table.
data AddTransactionResult =
  -- |Transaction is a duplicate of the given transaction.
  Duplicate !BlockItem |
  -- |The transaction was newly added.
  Added !BlockItem |
  -- |The nonce of the transaction is not later than the last finalized transaction for the sender.
  -- The transaction is not added to the table.
  ObsoleteNonce
  deriving(Eq, Show)

-- |Monad that provides operations for working with the low-level tree state.
-- These operations are abstracted where possible to allow for a range of implementation
-- choices.
class (Eq (BlockPointerType m),
       Ord (BlockPointerType m),
       HashableTo BlockHash (BlockPointerType m),
       BlockData (BlockPointerType m),
       BlockPointerData (BlockPointerType m),
       BlockPendingData (PendingBlockType m),
       BlockStateStorage m,
       BlockPointerMonad m,
       PerAccountDBOperations m,
       Monad m)
      => TreeStateMonad m where

    -- * 'PendingBlock' operations
    -- |Create and sign a 'PendingBlock`.
    makePendingBlock ::
        BakerSignPrivateKey -- ^Key for signing the new block
        -> Slot             -- ^Block slot (must be non-zero)
        -> BlockHash        -- ^Hash of parent block
        -> BakerId          -- ^Identifier of block baker
        -> BlockProof       -- ^Block proof
        -> BlockNonce       -- ^Block nonce
        -> BlockFinalizationData
                            -- ^Finalization data
        -> [BlockItem]      -- ^List of transactions
        -> UTCTime          -- ^Block receive time
        -> m (PendingBlockType m)

    -- * Operations on the block table
    -- |Get the current status of a block.
    getBlockStatus :: BlockHash -> m (Maybe (BlockStatus (BlockPointerType m) (PendingBlockType m)))
    -- |Make a live 'BlockPointer' from a 'PendingBlock'.
    -- The parent and last finalized pointers must be correct.
    makeLiveBlock ::
        PendingBlockType m                       -- ^Block to make live
        -> BlockPointerType m                    -- ^Parent block pointer
        -> BlockPointerType m                    -- ^Last finalized block pointer
        -> BlockState m                      -- ^Block state
        -> ATIStorage m                      -- ^This block's account -> transaction index.
        -> UTCTime                           -- ^Block arrival time
        -> Energy                            -- ^Energy cost of the transactions in the block.
        -> m (BlockPointerType m)
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

    -- |Mark a block as finalized (by a particular 'FinalizationRecord').
    --
    -- Precondition: The block must be alive.
    markFinalized :: BlockHash -> FinalizationRecord -> m ()
    -- |Mark a block as pending (i.e. awaiting parent)
    markPending :: PendingBlockType m -> m ()
    -- * Queries on genesis block
    -- |Get the genesis 'BlockPointer'.
    getGenesisBlockPointer :: m (BlockPointerType m)
    -- |Get the 'GenesisData'.
    getGenesisData :: m GenesisData
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
    getNextFinalizationIndex = (+1) . finalizationIndex . snd <$> getLastFinalized
    -- |Add a block and finalization record to the finalization list.
    -- The block must be the one finalized by the record, and the finalization
    -- index must be the next finalization index.  These are not checked.
    addFinalization :: BlockPointerType m -> FinalizationRecord -> m ()
    -- |Get the block that is finalized at the given index together with the record
    -- that finalizes it.
    -- Returns 'Nothing' if no such pair exists.
    getFinalizedAtIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m, FinalizationRecord))

    -- |Get the block that is finalized at the given height, if any.
    getFinalizedAtHeight :: BlockHeight -> m (Maybe (BlockPointerType m))

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
    takePendingChildren :: BlockHash -> m [PendingBlockType m]
    -- |Add a pending block, that is pending on the arrival of its parent.
    addPendingBlock :: PendingBlockType m -> m ()
    -- |Return the next block that is pending its parent with slot number
    -- less than or equal to the given value, removing it from the pending
    -- table.  Returns 'Nothing' if there is no such pending block.
    takeNextPendingUntil :: Slot -> m (Maybe (PendingBlockType m))

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
      AccountAddress
      -> Nonce
      -> m [(Nonce, Set.Set Transaction)]

    -- |Get the successor of the largest known account for the given account
    -- The function should return 'True' in the second component if and only if
    -- all (known) transactions from this account are finalized.
    getNextAccountNonce :: AccountAddress -> m (Nonce, Bool)

    -- |Get a credential which has not yet been finalized, i.e., it is correct for this function
    -- to return 'Nothing' if the requested credential has already been finalized.
    getCredential :: TransactionHash -> m (Maybe CredentialDeploymentWithMeta)

    -- |Add a transaction to the transaction table.
    -- Does nothing if the transaction's nonce preceeds the next available nonce
    -- for the account at the last finalized block, or if a transaction with the same
    -- hash is already in the table.
    -- Otherwise, adds the transaction to the table and the non-finalized transactions
    -- for its account.
    -- A return value of @True@ indicates that the transaction was added (and not already
    -- present).  A return value of @False@ indicates that the transaction was not added,
    -- either because it was already present or the nonce has already been finalized.
    addTransaction :: BlockItem -> m Bool
    addTransaction tr = process <$> addCommitTransaction tr 0
      where process (Added _) = True
            process _ = False
    -- |Finalize a list of transactions on a given block. Per account, the transactions must be in
    -- continuous sequence by nonce, starting from the next available non-finalized
    -- nonce.
    finalizeTransactions :: BlockHash -> Slot -> [BlockItem] -> m ()
    -- |Mark a transaction as committed on a block with the given slot number,
    -- as well as add any additional outcomes for the given block (outcomes are given
    -- as the index of the transaction in the given block).
    -- This will prevent it from being purged while the slot number exceeds
    -- that of the last finalized block.
    commitTransaction :: Slot -> BlockHash -> BlockItem -> TransactionIndex -> m ()
    -- |@addCommitTransaction tr slot@ adds a transaction and marks it committed
    -- for the given slot number. By default the transaction is created in the 'Received' state,
    -- but if the transaction is already in the table the outcomes are retained.
    -- See documentation of 'AddTransactionResult' for meaning of the return value.
    -- The time is indicative of the receive time of the transaction. It is used to prioritize transactions
    -- when constructing a block.
    addCommitTransaction :: BlockItem -> Slot -> m AddTransactionResult
    -- |Purge a transaction from the transaction table if its last committed slot
    -- number does not exceed the slot number of the last finalized block.
    -- (A transaction that has been committed to a finalized block should not be purged.)
    -- Returns @True@ if and only if the transaction is purged.
    purgeTransaction :: BlockItem -> m Bool

    -- |Mark a transaction as no longer on a given block. This is used when a block is
    -- marked as dead.
    markDeadTransaction :: BlockHash -> BlockItem -> m ()
    -- |Lookup a transaction status by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe TransactionStatus)
    -- * Operations on statistics
    -- |Get the current consensus statistics.
    getConsensusStatistics :: m ConsensusStatistics
    -- |Set the consensus statistics.
    putConsensusStatistics :: ConsensusStatistics -> m ()

    -- |Get other runtime parameters that are implementation detail, and hence do
    -- not belong to genesis data.
    getRuntimeParameters :: m RuntimeParameters

instance (Monad (t m), MonadTrans t, TreeStateMonad m) => TreeStateMonad (MGSTrans t m) where
    makePendingBlock key slot parent bid pf n lastFin trs time = lift $ makePendingBlock key slot parent bid pf n lastFin trs time
    getBlockStatus = lift . getBlockStatus
    makeLiveBlock b parent lastFin st ati time energy = lift $ makeLiveBlock b parent lastFin st ati time energy
    markDead = lift . markDead
    markFinalized bh fr = lift $ markFinalized bh fr
    markPending = lift . markPending
    getGenesisBlockPointer = lift getGenesisBlockPointer
    getGenesisData = lift getGenesisData
    getLastFinalized = lift getLastFinalized
    getLastFinalizedSlot = lift getLastFinalizedSlot
    getLastFinalizedHeight = lift getLastFinalizedHeight
    getNextFinalizationIndex = lift getNextFinalizationIndex
    addFinalization bp fr = lift $ addFinalization bp fr
    getFinalizedAtIndex = lift . getFinalizedAtIndex
    getFinalizedAtHeight = lift . getFinalizedAtHeight
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
    addTransaction tr = lift $ addTransaction tr
    finalizeTransactions bh slot = lift . finalizeTransactions bh slot
    commitTransaction slot bh tr = lift . commitTransaction slot bh tr
    addCommitTransaction tr slot = lift $ addCommitTransaction tr slot
    purgeTransaction = lift . purgeTransaction
    markDeadTransaction bh = lift . markDeadTransaction bh
    lookupTransaction = lift . lookupTransaction
    getConsensusStatistics = lift getConsensusStatistics
    putConsensusStatistics = lift . putConsensusStatistics
    getRuntimeParameters = lift getRuntimeParameters

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
    {-# INLINE addTransaction #-}
    {-# INLINE finalizeTransactions #-}
    {-# INLINE commitTransaction #-}
    {-# INLINE addCommitTransaction #-}
    {-# INLINE purgeTransaction #-}
    {-# INLINE lookupTransaction #-}
    {-# INLINE markDeadTransaction #-}
    {-# INLINE getConsensusStatistics #-}
    {-# INLINE putConsensusStatistics #-}
    {-# INLINE getRuntimeParameters #-}

deriving via (MGSTrans MaybeT m) instance TreeStateMonad m => TreeStateMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance TreeStateMonad m => TreeStateMonad (ExceptT e m)
