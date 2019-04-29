{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Concordium.Skov.Monad where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Concordium.Types
import Concordium.GlobalState.Block
-- import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.TreeState(BlockPointer, BlockPointerData, BlockState)
import Concordium.Logger
import Concordium.TimeMonad

class (Monad m, TimeMonad m, LoggerMonad m, Eq (BlockPointer m), BlockPointerData (BlockPointer m)) => SkovMonad m where
    -- |Look up a block in the table given its hash
    resolveBlock :: BlockHash -> m (Maybe (BlockPointer m))
    -- |Store a block in the block table and add it to the tree
    -- if possible.
    storeBlock :: Block -> m BlockHash
    -- |Store a block in the block table that has just been baked.
    -- This assumes the block is valid and that there can be nothing
    -- pending for it (children or finalization).
    storeBakedBlock ::
        PendingBlock        -- ^The block to add
        -> BlockPointer m     -- ^Parent pointer
        -> BlockPointer m     -- ^Last finalized pointer
        -> BlockState m       -- ^State
        -> m (BlockPointer m)
    -- |Add a transaction to the transaction table.
    receiveTransaction :: Transaction -> m ()
    -- |Add a finalization record.  This should (eventually) result
    -- in a block being finalized.
    finalizeBlock :: FinalizationRecord -> m ()
    -- |Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointer m)
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

instance SkovMonad m => SkovMonad (MaybeT m) where
    resolveBlock = lift . resolveBlock
    storeBlock = lift . storeBlock
    storeBakedBlock pb parent lastFin state = lift $ storeBakedBlock pb parent lastFin state
    receiveTransaction = lift . receiveTransaction
    finalizeBlock = lift . finalizeBlock
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    getGenesisData = lift getGenesisData
    genesisBlock = lift genesisBlock
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop
    getBlocksAtHeight = lift . getBlocksAtHeight

getBirkParameters :: (SkovMonad m) => Slot -> m BirkParameters
getBirkParameters _ = genesisBirkParameters <$> getGenesisData

getGenesisTime :: (SkovMonad m) => m Timestamp
getGenesisTime = genesisTime <$> getGenesisData

getFinalizationParameters :: (SkovMonad m) => m FinalizationParameters
getFinalizationParameters = genesisFinalizationParameters <$> getGenesisData

getSlotTime :: (SkovMonad m) => Slot -> m UTCTime
getSlotTime s = do
        genData <- getGenesisData
        return $ posixSecondsToUTCTime (fromIntegral (genesisTime genData + genesisSlotDuration genData * fromIntegral s))
