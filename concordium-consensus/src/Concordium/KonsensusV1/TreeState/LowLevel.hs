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
--
-- The following invariants apply:
--     * Every block in the store is either finalized or certified.
--     * The genesis block is always finalized and has height 0.
--     * No other genesis blocks are stored in the database.
--     * The latest finalization entry is always present if there is more than one finalized block.
--     * If present, the latest finalization entry finalizes the last finalized block.
--     * The transactions indexed in the store are exactly the transactions of finalized blocks.
class (Monad m) => MonadTreeStateStore m where
    -- |Get a block by block hash.
    lookupBlock :: BlockHash -> m (Maybe (StoredBlock (MPV m)))

    -- |Determine if a block is present in the block table.
    memberBlock :: BlockHash -> m Bool

    -- |Get the first (i.e. genesis) block.
    -- (The implementation can assume that this block has height 0.)
    lookupFirstBlock :: m (Maybe (StoredBlock (MPV m)))
    lookupFirstBlock = lookupBlockByHeight 0

    -- |Get the last finalized block.
    lookupLastFinalizedBlock :: m (Maybe (StoredBlock (MPV m)))

    -- |Look up a finalized block by height.
    lookupBlockByHeight :: BlockHeight -> m (Maybe (StoredBlock (MPV m)))

    -- |Look up a transaction by its hash.
    lookupTransaction :: TransactionHash -> m (Maybe FinalizedTransactionStatus)

    -- |Determine if a transaction is present in the finalized transaction table.
    memberTransaction :: TransactionHash -> m Bool

    -- |Record a finalization entry that finalizes a list of blocks, and mark the blocks and their
    -- transactions as finalized.
    -- This has the following effects:
    --
    -- 1. The QCs for all rounds up to and including the round of the new last finalized block
    --    are removed.
    -- 2. Each block where the QC was removed is also removed from the block table, unless it is in
    --    the list of newly-finalized blocks.
    -- 3. Each newly-finalized block that didn't have a QC is written to the block table.
    -- 4. The finalization entry is updated to be the new finalization entry.
    -- 5. Each newly-finalized block is added to the finalized blocks by height index.
    -- 6. The transactions in the newly-finalized blocks are added to the finalized transactions.
    --
    -- The following preconditions are required to ensure the database invariants are maintained:
    --
    --   * The list of blocks is non-empty, consists of consecutive non-finalized blocks
    --     that form a chain.
    --   * The finalization entry is for the last of these blocks.
    writeFinalizedBlocks :: [StoredBlock (MPV m)] -> FinalizationEntry -> m ()

    -- |Write a certified block that does not finalize other blocks.
    -- This has the following effects:
    --
    -- 1. The new certified block is written to the block table.
    -- 2. The QC for the newly-certified block is written to the QC table.
    --
    -- The following preconditions are required to ensure the database invariants are maintained:
    --
    --   * The quorum certificate is for the supplied block.
    --   * The parent block is the (previous) highest certified block.
    writeCertifiedBlock ::
        -- |The newly-certified block.
        StoredBlock (MPV m) ->
        -- |The quorum certificate for the block.
        QuorumCertificate ->
        m ()

    -- |Write a certified block that does finalize other blocks. This is equivalent to calling
    -- 'writeFinalizedBlocks' followed by 'writeCertifiedBlock', but uses a single transaction.
    --
    -- The following preconditions are required to ensure the database invariants are maintained:
    --
    --   * The list of blocks is non-empty, consists of consecutive non-finalized blocks
    --     that form a chain.
    --   * The last of these blocks is the parent of the newly-certified block.
    --   * The finalization entry is for the parent block and the successor QC is for the
    --     newly-certified block.
    writeCertifiedBlockWithFinalization ::
        -- |List of blocks that are newly finalized, in increasing order of height.
        [StoredBlock (MPV m)] ->
        -- |The newly-certified block.
        StoredBlock (MPV m) ->
        -- |A finalization entry that finalizes the last of the finalized blocks, with the successor
        -- quorum certificate being for the newly-certified block.
        FinalizationEntry ->
        m ()

    -- |Look up the finalization entry for the last finalized block.
    lookupLatestFinalizationEntry :: m (Maybe FinalizationEntry)

    -- |Look up all of the certified (non-finalized) blocks, with their quorum certificates.
    -- The list is in order of increasing round number.
    lookupCertifiedBlocks :: m [(StoredBlock (MPV m), QuorumCertificate)]

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
