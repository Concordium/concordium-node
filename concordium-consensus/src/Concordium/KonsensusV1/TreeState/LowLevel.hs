{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module defines the low-level interface to the persistent tree state.
module Concordium.KonsensusV1.TreeState.LowLevel where

import Data.Serialize

import Concordium.Types

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo

-- |A reference to the block state for a particular block.
type BlockStateRef (pv :: ProtocolVersion) = BlobRef (BlockStatePointers pv)

-- |A stored block as retained by the low-level tree state store.
-- Note: we serialize blocks with a version byte to allow future flexibility in how blocks are
-- stored.
data StoredBlock (pv :: ProtocolVersion) = StoredBlock
    { -- |Metadata about the block.
      stbInfo :: !BlockMetadata,
      -- |The block itself.
      stbBlock :: !(Block pv),
      -- |Pointer to the state in the block state storage.
      stbStatePointer :: !(BlockStateRef pv)
    }

instance IsProtocolVersion pv => Serialize (StoredBlock pv) where
    put StoredBlock{..} = do
        putWord8 0 -- Version byte
        put stbInfo
        put (getHash stbBlock :: BlockHash)
        put stbStatePointer
        putBlock stbBlock
    get =
        getWord8 >>= \case
            0 -> do
                stbInfo <- get
                blockHash <- get
                stbStatePointer <- get
                stbBlock <-
                    unsafeGetBlockKnownHash
                        (utcTimeToTransactionTime $ bmReceiveTime stbInfo)
                        blockHash
                return StoredBlock{..}
            v -> fail $ "Unsupported StoredBlock version: " ++ show v

instance BlockData (StoredBlock pv) where
    type BakedBlockDataType (StoredBlock pv) = BakedBlockDataType SignedBlock
    blockRound = blockRound . stbBlock
    blockEpoch = blockEpoch . stbBlock
    blockTimestamp = blockTimestamp . stbBlock
    blockBakedData = blockBakedData . stbBlock
    blockTransactions = blockTransactions . stbBlock
    blockTransactionCount = blockTransactionCount . stbBlock
    blockStateHash = blockStateHash . stbBlock

instance HashableTo BlockHash (StoredBlock pv) where
    getHash = getHash . stbBlock

instance HasBlockMetadata (StoredBlock pv) where
    blockMetadata = stbInfo

-- |'MonadTreeStateStore' defines the interface to the low-level tree state database.
-- An implementation should guarantee atomicity, consistency and isolation for these operations.
-- Durability is also expected from a persistent implementation.
-- The write operations in particular may involve updating multiple tables and should guarantee
-- transactional behaviour.
class (Monad m) => MonadTreeStateStore m where
    -- |Get a finalized block by block hash.
    lookupBlock :: BlockHash -> m (Maybe (StoredBlock (MPV m)))

    -- |Determine if a block is present in the finalized block table.
    memberBlock :: BlockHash -> m Bool

    -- |Get the first (i.e. genesis) block.
    -- (The implementation can assume that this block has height 0.)
    lookupFirstBlock :: m (Maybe (StoredBlock (MPV m)))
    lookupFirstBlock = lookupBlockByHeight 0

    -- |Get the last finalized block.
    lookupLastBlock :: m (Maybe (StoredBlock (MPV m)))

    -- |Look up a block by height.
    lookupBlockByHeight :: BlockHeight -> m (Maybe (StoredBlock (MPV m)))

    -- |Look up a transaction by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe FinalizedTransactionStatus)

    -- |Determine if a transaction is present in the finalized transaction table.
    memberTransaction :: TransactionHash -> m Bool

    -- |Store the list of blocks and their transactions, updating the last finalization entry to
    -- the supplied value.  (This should write the blocks as a single database transaction.)
    writeBlocks :: [StoredBlock (MPV m)] -> FinalizationEntry -> m ()

    -- |Look up the finalization entry for the last finalized block.
    lookupLatestFinalizationEntry :: m (Maybe FinalizationEntry)

    -- |Look up the status of the current round.
    lookupCurrentRoundStatus :: m PersistentRoundStatus

    -- |Write the status of the current round.
    -- There is ever only one 'PersistentRoundStatus', hence when progressing
    -- rounds the current 'PersistentRoundStatus' will be overwritten by a new one.
    --
    -- This is done independently of finalizing blocks i.e. 'writeBlocks'
    -- as in the case of a restart then the protocol requires the latest 'PersistentRoundStatus'
    -- in order to progress to the next round (in accordance to)
    -- to the consensus protocol. By accordance to the protocol it is meant that the consensus
    -- should be able to be restarted without double signing and sending a signature message for quorum-/timeout certificate for the
    -- current round.
    --
    -- On a potential rollback of the database then the consensus will initiate catchup and
    -- a new round status will be created when the consensus is fully caught up.
    writeCurrentRoundStatus :: PersistentRoundStatus -> m ()

    -- |From the last block backwards, remove blocks and their associated transactions
    -- from the database until the predicate returns 'True'. If any blocks are rolled back,
    -- this also removes the latest finalization entry.
    -- This returns @Right Int@ where the 'Int' indicates how many blocks were rolled back.
    -- If an error occurred attempting to roll back, @Left reason@ is returned.
    rollBackBlocksUntil :: (StoredBlock (MPV m) -> m Bool) -> m (Either String Int)
