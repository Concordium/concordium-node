{-# LANGUAGE DataKinds #-}
-- |This module defines the low-level interface to the persistent tree state.
module Concordium.KonsensusV1.TreeState.LowLevel where

import Concordium.Types
import Concordium.Types.Execution

import Concordium.KonsensusV1.Types
import Concordium.KonsensusV1.TreeState.Types

type BlockStateRef = ()

data StoredBlock (pv :: ProtocolVersion) = StoredBlock
    { sbInfo :: !BlockMetadata,
      sbBlock :: !SignedBlock,
      sbStatePointer :: !BlockStateRef
    }

data FinalizedTransactionStatus = FinalizedTransactionStatus
    { -- |Height of the finalized block that contains this transaction
      ftsBlockHeight :: !BlockHeight,
      -- |Index of the transaction in the block.
      ftsIndex :: !TransactionIndex
    }
    deriving (Eq, Show)

data RoundStatus = RoundStatus {
    rsCurrentRound :: !Round
}

class (Monad m) => TreeStateStoreMonad m where
    -- |Get a finalized block by block hash.
    lookupBlock :: BlockHash -> m (Maybe (StoredBlock (MPV m)))

    -- |Determine if a block is present in the finalized block table.
    memberBlock :: BlockHash -> m Bool

    -- |Get the first (i.e. genesis) block.
    lookupFirstBlock :: m (Maybe (StoredBlock (MPV m)))

    -- |Get the last (finalized) block.
    lookupLastBlock :: m (Maybe (StoredBlock (MPV m)))

    -- |Look up a block by height.
    lookupBlockByHeight :: BlockHeight -> m (Maybe (StoredBlock (MPV m)))

    -- |Look up a transaction by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe FinalizedTransactionStatus)

    -- |Determine if a block is present in the finalized transaction table.
    memberTransaction :: TransactionHash -> m Bool

    -- |Store the list of blocks and their transactions.
    -- (This should write the blocks as a single database transaction.)
    writeBlocks :: [StoredBlock (MPV m)] -> FinalizationEntry -> m ()

    -- |Look up the finalization entry for the last finalized block.
    lookupLatestFinalizationEntry :: m (Maybe FinalizationEntry)

    -- |Look up the status of the current round.
    lookupCurrentRoundStatus :: m RoundStatus

    -- |Write the status of the current round.
    writeCurrentRoundStatus :: RoundStatus -> m ()

    -- |From the last block backwards, remove blocks and their associated transactions
    -- from the database until the predicate returns 'True'. If any blocks are rolled back,
    -- this also removes the latest finalization entry.
    -- This returns 'True' if and only if roll-back occurred.
    rollBackBlocksUntil :: (StoredBlock (MPV m) -> m Bool) -> m Bool

