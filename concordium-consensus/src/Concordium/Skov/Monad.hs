{-# LANGUAGE
    DerivingVia,
    StandaloneDeriving #-}
module Concordium.Skov.Monad where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.BlockState (BlockStateQuery)
import Concordium.GlobalState.TreeState (BlockPointer, BlockPointerData, BlockState, MGSTrans(..), PendingBlock)
import Concordium.Logger
import Concordium.TimeMonad

import Concordium.Scheduler.TreeStateEnvironment(ExecutionResult)

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
    | ResultIncorrectFinalizationSession
    -- ^The message refers to a different/unknown finalization session
    | ResultUnverifiable
    -- ^The message could not be validated with the current state
    | ResultContinueCatchUp
    -- ^The peer should be marked as pending unless catch up is already in progress
    | ResultEarlyBlock
    -- ^The block was sent too early and should be dropped
    deriving (Show)

class (Monad m, Eq (BlockPointer m), BlockPointerData (BlockPointer m), BlockStateQuery m) => SkovQueryMonad m where
    -- |Look up a block in the table given its hash
    resolveBlock :: BlockHash -> m (Maybe (BlockPointer m))
    -- |Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointer m)
    -- |Retrieves the birk parameters for a slot, given a branch (in the form of a block pointer.)
    --  Retrieves AdvanceTime and StableTime directly from genesis block
    getBirkParameters :: Slot -> BlockPointer m -> m BirkParameters
    -- |Get the genesis data.
    getGenesisData :: m GenesisData
    -- |Get the genesis block pointer.
    genesisBlock :: m (BlockPointer m)
    -- |Get the height of the highest blocks in the tree.
    -- Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight
    -- |Get the blocks in the branches of the tree grouped by descending height.
    -- That is the first element of the list is all of the blocks at 'getCurrentHeight',
    -- the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockPointer m]]
    -- |Get a list of all the blocks at a given height in the tree.
    getBlocksAtHeight :: BlockHeight -> m [BlockPointer m]
    -- |Get a block's state.
    queryBlockState :: BlockPointer m -> m (BlockState m)
    -- |Get the outcomes of a transaction.
    queryTransactionStatus :: TransactionHash -> m (Maybe TransactionStatus)
    -- |Get non-finalized transactions for an account, ordered by increasing nonce.
    queryNonFinalizedTransactions :: AccountAddress -> m [TransactionHash]


class (SkovQueryMonad m, TimeMonad m, LoggerMonad m) => SkovMonad m where
    -- |Store a block in the block table and add it to the tree
    -- if possible.
    storeBlock :: PendingBlock m -> m UpdateResult
    -- |Store a block in the block table that has just been baked.
    -- This assumes the block is valid and that there can be nothing
    -- pending for it (children or finalization).
    storeBakedBlock ::
        PendingBlock m        -- ^The block to add
        -> BlockPointer m     -- ^Parent pointer
        -> BlockPointer m     -- ^Last finalized pointer
        -> ExecutionResult m  -- ^Result of the execution of the block.
        -> m (BlockPointer m)
    -- |Add a transaction to the transaction table.
    receiveTransaction :: Transaction -> m UpdateResult
    -- |Add a finalization record.  This should (eventually) result
    -- in a block being finalized.
    finalizeBlock :: FinalizationRecord -> m UpdateResult

instance (Monad (t m), MonadTrans t, SkovQueryMonad m) => SkovQueryMonad (MGSTrans t m) where
    resolveBlock = lift . resolveBlock
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    getBirkParameters slot bp = lift $ getBirkParameters slot bp
    getGenesisData = lift getGenesisData
    genesisBlock = lift genesisBlock
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop
    getBlocksAtHeight = lift . getBlocksAtHeight
    queryBlockState = lift . queryBlockState
    queryTransactionStatus = lift . queryTransactionStatus
    queryNonFinalizedTransactions = lift . queryNonFinalizedTransactions
    {-# INLINE resolveBlock #-}
    {-# INLINE isFinalized #-}
    {-# INLINE lastFinalizedBlock #-}
    {-# INLINE getBirkParameters #-}
    {-# INLINE getGenesisData #-}
    {-# INLINE genesisBlock #-}
    {-# INLINE getCurrentHeight #-}
    {-# INLINE branchesFromTop #-}
    {-# INLINE getBlocksAtHeight #-}
    {-# INLINE queryBlockState #-}
    {-# INLINE queryTransactionStatus #-}
    {-# INLINE queryNonFinalizedTransactions #-}

instance (Monad (t m), MonadTrans t, SkovMonad m) => SkovMonad (MGSTrans t m) where
    storeBlock b = lift $ storeBlock b
    storeBakedBlock pb parent lastFin result = lift $ storeBakedBlock pb parent lastFin result
    receiveTransaction = lift . receiveTransaction
    finalizeBlock fr = lift $ finalizeBlock fr

deriving via (MGSTrans MaybeT m) instance SkovQueryMonad m => SkovQueryMonad (MaybeT m)
deriving via (MGSTrans MaybeT m) instance SkovMonad m => SkovMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovQueryMonad m => SkovQueryMonad (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance SkovMonad m => SkovMonad (ExceptT e m)

getGenesisTime :: (SkovQueryMonad m) => m Timestamp
getGenesisTime = genesisTime <$> getGenesisData

getFinalizationParameters :: (SkovQueryMonad m) => m FinalizationParameters
getFinalizationParameters = genesisFinalizationParameters <$> getGenesisData

getSlotTime :: (SkovQueryMonad m) => Slot -> m UTCTime
getSlotTime s = do
        genData <- getGenesisData
        return $ posixSecondsToUTCTime (fromIntegral (genesisTime genData + genesisSlotDuration genData * fromIntegral s))
