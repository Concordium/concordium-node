{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module defines the low-level interface to the persistent tree state.
module Concordium.KonsensusV1.TreeState.LowLevel where

import Data.Maybe
import Data.Serialize

import Concordium.Types
import Concordium.Types.Execution

import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo

type BlockStateRef = ()

data StoredBlock (pv :: ProtocolVersion) = StoredBlock
    { stbInfo :: !BlockMetadata,
      stbBlock :: !SignedBlock,
      stbStatePointer :: !BlockStateRef
    }

instance IsProtocolVersion pv => Serialize (StoredBlock pv) where
    put StoredBlock{..} = do
        putWord8 0 -- Version byte
        put stbInfo
        put stbStatePointer
        putSignedBlock stbBlock
    get =
        getWord8 >>= \case
            0 -> do
                stbInfo <- get
                stbStatePointer <- get
                stbBlock <-
                    getSignedBlock
                        (protocolVersion @pv)
                        (utcTimeToTransactionTime $ bmReceiveTime stbInfo)
                return StoredBlock{..}
            v -> fail $ "Unsupported StoredBlock version: " ++ show v

instance BlockData (StoredBlock pv) where
    type BakedBlockDataType (StoredBlock pv) = BakedBlockDataType SignedBlock
    blockRound = blockRound . stbBlock
    blockEpoch = blockEpoch . stbBlock
    blockTimestamp = blockTimestamp . stbBlock
    blockBakedData = blockBakedData . stbBlock
    blockTransactions = blockTransactions . stbBlock
    blockStateHash = blockStateHash . stbBlock

instance HashableTo BlockHash (StoredBlock pv) where
    getHash = getHash . stbBlock

data FinalizedTransactionStatus = FinalizedTransactionStatus
    { -- |Height of the finalized block that contains this transaction
      ftsBlockHeight :: !BlockHeight,
      -- |Index of the transaction in the block.
      ftsIndex :: !TransactionIndex
    }
    deriving (Eq, Show)

instance Serialize FinalizedTransactionStatus where
    put FinalizedTransactionStatus{..} = do
        put ftsBlockHeight
        put ftsIndex
    get = do
        ftsBlockHeight <- get
        ftsIndex <- get
        return FinalizedTransactionStatus{..}

data RoundStatus = RoundStatus
    { rsCurrentRound :: !Round
    }
instance Serialize RoundStatus where
    put RoundStatus{..} = put rsCurrentRound
    get = do
        rsCurrentRound <- get
        return RoundStatus{..}

class (Monad m) => TreeStateStoreMonad m where
    -- |Get a finalized block by block hash.
    lookupBlock :: BlockHash -> m (Maybe (StoredBlock (MPV m)))

    -- |Determine if a block is present in the finalized block table.
    memberBlock :: BlockHash -> m Bool
    memberBlock = fmap isJust . lookupBlock

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
    memberTransaction = fmap isJust . lookupTransaction

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
