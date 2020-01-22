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
    module Concordium.GlobalState.Block,
    module Concordium.GlobalState.TreeState
) where

import qualified Data.Sequence as Seq
import Data.Time
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.ByteString as ByteString

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.BlockState

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
type Branches m = Seq.Seq [BlockPointer m]

-- |Result of trying to add a transaction to the transaction table.
data AddTransactionResult =
  -- |Transaction is a duplicate of the given transaction.
  Duplicate !Transaction |
  -- |The transaction was newly added.
  Added !Transaction |
  -- |The nonce of the transaction is not later than the last finalized transaction for the sender.
  -- The transaction is not added to the table.
  ObsoleteNonce
  deriving(Eq, Show)

-- |Monad that provides operations for working with the low-level tree state.
-- These operations are abstracted where possible to allow for a range of implementation
-- choices.
class (Eq (BlockPointer m),
       Ord (BlockPointer m),
       HashableTo BlockHash (BlockPointer m),
       BlockData (BlockPointer m),
       BlockPointerData (BlockPointer m),
       BlockPendingData (PendingBlock m),
       BlockStateStorage m,
       Monad m)
      => TreeStateMonad m where

    -- |Get the 'BlockState' of a 'BlockPointer'.
    blockState :: BlockPointer m -> m (BlockState m)

    -- |Get the parent of a 'BlockPointer'
    bpParent :: BlockPointer m -> m (BlockPointer m)

    -- |Get the last finalized of a 'BlockPointer'
    bpLastFinalized :: BlockPointer m -> m (BlockPointer m)

    -- * 'PendingBlock' operations
    -- |Create and sign a 'PendingBlock`.
    makePendingBlock ::
        BakerSignPrivateKey -- ^Key for signing the new block
        -> Slot             -- ^Block slot (must be non-zero)
        -> BlockHash        -- ^Hash of parent block
        -> BakerId          -- ^Identifier of block baker
        -> BlockProof       -- ^Block proof
        -> BlockNonce       -- ^Block nonce
        -> BlockHash        -- ^Hash of last finalized block
        -> [Transaction]    -- ^List of transactions
        -> UTCTime          -- ^Block receive time
        -> m (PendingBlock m)
    -- |Create a 'PendingBlock' from the raw block data.
    -- If deserialisation fails, it returns an error.
    -- Otherwise, it returns the block.
    importPendingBlock ::
        ByteString.ByteString
                            -- ^Block data
        -> UTCTime          -- ^Block received time
        -> m (Either String (PendingBlock m))

    -- * Operations on the block table
    -- |Get the current status of a block.
    getBlockStatus :: BlockHash -> m (Maybe (BlockStatus (BlockPointer m) (PendingBlock m)))
    -- |Make a live 'BlockPointer' from a 'PendingBlock'.
    -- The parent and last finalized pointers must be correct.
    makeLiveBlock ::
        PendingBlock m                       -- ^Block to make live
        -> BlockPointer m                    -- ^Parent block pointer
        -> BlockPointer m                    -- ^Last finalized block pointer
        -> BlockState m                      -- ^Block state
        -> UTCTime                           -- ^Block arrival time
        -> Energy                            -- ^Energy cost of the transactions in the block.
        -> m (BlockPointer m)
    -- |Mark a block as dead.
    markDead :: BlockHash -> m ()
    -- |Mark a block as finalized (by a particular 'FinalizationRecord').
    --
    -- Precondition: The block must be alive.
    markFinalized :: BlockHash -> FinalizationRecord -> m ()
    -- |Mark a block as pending (i.e. awaiting parent or finalization of
    --  last finalized block)
    markPending :: PendingBlock m -> m ()
    -- * Queries on genesis block
    -- |Get the genesis 'BlockPointer'.
    getGenesisBlockPointer :: m (BlockPointer m)
    -- |Get the 'GenesisData'.
    getGenesisData :: m GenesisData
    -- * Operations on the finalization list
    -- |Get the last finalized block.
    getLastFinalized :: m (BlockPointer m, FinalizationRecord)
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
    addFinalization :: BlockPointer m -> FinalizationRecord -> m ()
    -- |Get the finalization record for a particular finalization index (if available), with the finalized block.
    getFinalizationAtIndex :: FinalizationIndex -> m (Maybe (FinalizationRecord, BlockPointer m))
    -- |Get a list of all (validated) finalization records with blocks from the given index
    getFinalizationFromIndex :: FinalizationIndex -> m [(FinalizationRecord, BlockPointer m)]
    getFinalizationFromIndex i = getFinalizationAtIndex i >>= \case
            Nothing -> return []
            Just f -> (f :) <$> getFinalizationFromIndex (i+1)
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
    takePendingChildren :: BlockHash -> m [PendingBlock m]
    -- |Add a pending block, that is pending on the arrival of its parent.
    addPendingBlock :: PendingBlock m -> m ()
    -- |Return the next block that is pending its parent with slot number
    -- less than or equal to the given value, removing it from the pending
    -- table.  Returns 'Nothing' if there is no such pending block.
    takeNextPendingUntil :: Slot -> m (Maybe (PendingBlock m))
    -- * Operations on blocks that are pending the finalization of their
    -- last finalized block
    --
    -- $awaitingLastFinalized
    -- The blocks awaiting their last finalized block to become finalized are
    -- conceptually stored in a min priority queue, where the priority is the
    -- height of the block's indicated last finalized block.
    -- When a block is finalized, all of the blocks that are awaiting last
    -- finalized blocks of at most that height can be processed.
    --
    -- |Add a block that is awaiting finalization of its last finalized block.
    addAwaitingLastFinalized :: BlockHeight         -- ^Height of block's last finalized block
                                -> PendingBlock m   -- ^Block that is pending
                                -> m ()
    -- |Take the next awaiting-last-finalized block where the height of the
    -- block that is awaiting finalization is less than or equal to the given
    -- value.
    takeAwaitingLastFinalizedUntil :: BlockHeight -> m (Maybe (PendingBlock m))
    -- * Operations on the finalization pool
    -- |Get the finalization pool at the given finalization index.
    getFinalizationPoolAtIndex :: FinalizationIndex -> m [FinalizationRecord]
    -- |Set the finalization pool at the given finalization index.
    putFinalizationPoolAtIndex :: FinalizationIndex -> [FinalizationRecord] -> m ()
    -- |Add a finalization record to the finalization pool.
    addFinalizationRecordToPool :: FinalizationRecord -> m ()
    -- * Operations on the pending transaction table
    --
    -- $pendingTransactions
    -- We maintain a 'PendingTransactionTable' for a particular block that is
    -- the focus block.  (Ideally, this should be the best block, however, it
    -- shouldn't be a problem if it's not.)
    -- |Return the focus block.
    getFocusBlock :: m (BlockPointer m)
    -- |Update the focus block.
    putFocusBlock :: BlockPointer m -> m ()
    -- |Get the pending transactions after execution of the focus block.
    getPendingTransactions :: m PendingTransactionTable
    -- |Set the pending transactions after execution of the focus block.
    putPendingTransactions :: PendingTransactionTable -> m ()

    -- * Operations on the transaction table
    -- |Get non-finalized transactions for the given account starting at the given nonce (inclusive).
    -- These are returned as an ordered list of pairs of nonce and non-empty set of transactions
    -- with that nonce.
    getAccountNonFinalized :: AccountAddress -> Nonce -> m [(Nonce, Set.Set Transaction)]
    -- |Add a transaction to the transaction table.
    -- Does nothing if the transaction's nonce preceeds the next available nonce
    -- for the account at the last finalized block, or if a transaction with the same
    -- hash is already in the table.
    -- Otherwise, adds the transaction to the table and the non-finalized transactions
    -- for its account.
    -- A return value of @True@ indicates that the transaction was added (and not already
    -- present).  A return value of @False@ indicates that the transaction was not added,
    -- either because it was already present or the nonce has already been finalized.
    addTransaction :: Transaction -> m Bool
    addTransaction tr = process <$> addCommitTransaction tr 0
      where process (Added _) = True
            process _ = False
    -- |Finalize a list of transactions.  Per account, the transactions must be in
    -- continuous sequence by nonce, starting from the next available non-finalized
    -- nonce.
    finalizeTransactions :: [Transaction] -> m ()
    -- |Mark a transaction as committed on a block with the given slot number.
    -- This will prevent it from being purged while the slot number exceeds
    -- that of the last finalized block.
    commitTransaction :: Slot -> Transaction -> m ()
    -- |@addCommitTransaction tr slot@ adds a transaction and marks it committed
    -- for the given slot number.
    -- See documentation of 'AddTransactionResult' for meaning of the return value.
    addCommitTransaction :: Transaction -> Slot -> m AddTransactionResult
    -- |Purge a transaction from the transaction table if its last committed slot
    -- number does not exceed the slot number of the last finalized block.
    -- (A transaction that has been committed to a finalized block should not be purged.)
    -- Returns @True@ if the transaction is purged.
    purgeTransaction :: Transaction -> m Bool
    -- |Lookup a transaction by its hash.  As well as the transaction, returns
    -- a @Bool@ indicating whether the transaction is already finalized.
    lookupTransaction :: TransactionHash -> m (Maybe (Transaction, Bool))
    -- |Replace the transactions in a pending block with an identical set of
    -- transactions.  (If the transactions are not identical, the hash will
    -- not be correct.)  This is intended for de-duplicating transactions.
    -- Ideally, this should be handled better.
    updateBlockTransactions :: [Transaction] -> PendingBlock m -> m (PendingBlock m)

    -- * Operations on statistics
    -- |Get the current consensus statistics.
    getConsensusStatistics :: m ConsensusStatistics
    -- |Set the consensus statistics.
    putConsensusStatistics :: ConsensusStatistics -> m ()

    -- |Get other runtime parameters that are implementation detail, and hence do
    -- not belong to genesis data.
    getRuntimeParameters :: m RuntimeParameters

instance (Monad (t m), MonadTrans t, TreeStateMonad m) => TreeStateMonad (MGSTrans t m) where
    blockState = lift . blockState
    bpParent = lift . bpParent
    bpLastFinalized = lift . bpLastFinalized
    makePendingBlock key slot parent bid pf n lastFin trs time = lift $ makePendingBlock key slot parent bid pf n lastFin trs time
    importPendingBlock bdata rectime = lift $ importPendingBlock bdata rectime
    getBlockStatus = lift . getBlockStatus
    makeLiveBlock b parent lastFin st time energy = lift $ makeLiveBlock b parent lastFin st time energy
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
    getFinalizationAtIndex fi = lift $ getFinalizationAtIndex fi
    getFinalizationFromIndex fi = lift $ getFinalizationFromIndex fi
    getBranches = lift getBranches
    putBranches = lift . putBranches
    takePendingChildren = lift . takePendingChildren
    addPendingBlock = lift . addPendingBlock
    takeNextPendingUntil = lift . takeNextPendingUntil
    addAwaitingLastFinalized bh pb = lift $ addAwaitingLastFinalized bh pb
    takeAwaitingLastFinalizedUntil = lift . takeAwaitingLastFinalizedUntil
    getFinalizationPoolAtIndex = lift . getFinalizationPoolAtIndex
    putFinalizationPoolAtIndex fi frs = lift $ putFinalizationPoolAtIndex fi frs
    addFinalizationRecordToPool = lift . addFinalizationRecordToPool
    getFocusBlock = lift getFocusBlock
    putFocusBlock = lift . putFocusBlock
    getPendingTransactions = lift getPendingTransactions
    putPendingTransactions = lift . putPendingTransactions
    getAccountNonFinalized acc = lift . getAccountNonFinalized acc
    addTransaction  = lift . addTransaction
    finalizeTransactions = lift . finalizeTransactions
    commitTransaction slot tr = lift $ commitTransaction slot tr
    addCommitTransaction tr slot = lift $ addCommitTransaction tr slot
    purgeTransaction = lift . purgeTransaction
    lookupTransaction = lift . lookupTransaction
    updateBlockTransactions trs b = lift $ updateBlockTransactions trs b
    getConsensusStatistics = lift getConsensusStatistics
    putConsensusStatistics = lift . putConsensusStatistics
    getRuntimeParameters = lift getRuntimeParameters

    {-# INLINE blockState #-}
    {-# INLINE makePendingBlock #-}
    {-# INLINE importPendingBlock #-}
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
    {-# INLINE getFinalizationAtIndex #-}
    {-# INLINE getFinalizationFromIndex #-}
    {-# INLINE getBranches #-}
    {-# INLINE putBranches #-}
    {-# INLINE takePendingChildren #-}
    {-# INLINE addPendingBlock #-}
    {-# INLINE takeNextPendingUntil #-}
    {-# INLINE addAwaitingLastFinalized #-}
    {-# INLINE takeAwaitingLastFinalizedUntil #-}
    {-# INLINE getFinalizationPoolAtIndex #-}
    {-# INLINE putFinalizationPoolAtIndex #-}
    {-# INLINE addFinalizationRecordToPool #-}
    {-# INLINE getFocusBlock #-}
    {-# INLINE putFocusBlock #-}
    {-# INLINE getPendingTransactions #-}
    {-# INLINE putPendingTransactions #-}
    {-# INLINE getAccountNonFinalized #-}
    {-# INLINE addTransaction #-}
    {-# INLINE finalizeTransactions #-}
    {-# INLINE commitTransaction #-}
    {-# INLINE addCommitTransaction #-}
    {-# INLINE purgeTransaction #-}
    {-# INLINE lookupTransaction #-}
    {-# INLINE updateBlockTransactions #-}
    {-# INLINE getConsensusStatistics #-}
    {-# INLINE putConsensusStatistics #-}
    {-# INLINE getRuntimeParameters #-}

deriving via (MGSTrans MaybeT m) instance TreeStateMonad m => TreeStateMonad (MaybeT m)
-- deriving via (MGSTrans (RWST r w s) m) instance (TreeStateMonad m, Monoid w) => TreeStateMonad (RWST r w s m)
deriving via (MGSTrans (ExceptT e) m) instance TreeStateMonad m => TreeStateMonad (ExceptT e m)
