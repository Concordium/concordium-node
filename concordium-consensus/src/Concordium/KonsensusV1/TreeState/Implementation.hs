{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes the API of the treestate.
--  The tree state is stores the data required for ensuring progressing rounds
--  in the consensus protocol (V1).
--  The tree state consists of a transient part and a persistent part.
--  This module contains the relevant updates to the transient part, while some
--  queries makes use of 'Concordium.KonsensusV1.TreeState.LowLevel'.
--
--  State should be retained in memory until it is either finalized or marked dead.
--
--  The 'Concordium.KonsensusV1.TreeState.LowLevel' module exposes the API
--  required for persisting state. In particular this module assumes that
--  the caller persists data upon finalization via the said module above.
module Concordium.KonsensusV1.TreeState.Implementation where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Typeable
import GHC.Stack
import Lens.Micro.Platform

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import qualified Concordium.Genesis.Data.BaseV1 as Base
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Option
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Utils
import Concordium.Utils.InterpolationSearch

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters (RuntimeParameters (..))
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache, insertDeadCache, memberDeadCache)
import qualified Concordium.GlobalState.PurgeTransactions as Purge
import qualified Concordium.GlobalState.Statistics as Stats
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import qualified Concordium.TransactionVerification as TVer

-- | Exception occurring from a violation of tree state invariants.
newtype TreeStateInvariantViolation = TreeStateInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception TreeStateInvariantViolation where
    displayException (TreeStateInvariantViolation reason) =
        "Tree state invariant violation: "
            ++ show reason

-- | The block table yields blocks that are
--  either alive or pending.
--  Furthermore it holds a fixed size cache of hashes
--  of blocks marked as dead.
data BlockTable pv = BlockTable
    { -- | Cache for dead blocks.
      --  See documentation of 'DeadCache' for an
      --  elaborate description of it.
      --
      --  But caching the dead blocks is beneficial
      --  when receiving blocks and evaluating if they can
      --  become part of the tree or not (accepted or not).
      --  Hence with a cache of dead blocks we can immediately
      --  reject a received block if it points to a block marked as dead
      --  as opposed to the case where it otherwise would be seen as being "unknown"
      --  if we simply expunged dead blocks.
      _deadBlocks :: !DeadCache,
      -- | Map of live blocks.
      --  Blocks are removed from this map by two means:
      --
      --  * When a block becomes finalized it is removed from the table, as it is persisted on disk.
      --
      --  * When a block is being marked as dead.
      --
      --  Note that non-finalized certified blocks exist both in the live map and on the disk.
      _liveMap :: !(HM.HashMap BlockHash (BlockPointer pv))
    }
    deriving (Show)

makeLenses ''BlockTable

-- | Create the empty block table.
emptyBlockTable :: BlockTable pv
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- | The 'PendingTransactions' consists of a "focus block", which is a live block, and a pending
--  transaction table that is with respect to the focus block.
--
--  The focus block, is the block that the consensus layer will extend a new block from.
--  Thus it must be the case, that the focus block is always a successor of the latest finalized block,
--  usually the focus block will the best block unless branching and pruning occurs.
--  Hence the focus block can be rolled back if we are pruning a branch of the tree and
--  we are starting to extend the tree from a predecessor block.
--
--  The 'PendingTransactionTable' is special for the focus block, as it records
--  the pending transactions for that given block (the focus block).
--
--  Hence, it must always be the case that a nonce from the perspective of the focus block
--  is the same as recorded in the 'PendingTransactionTable'.
data PendingTransactions pv = PendingTransactions
    { -- | The block with respect to which the pending transactions are considered pending.
      _focusBlock :: !(BlockPointer pv),
      -- | The table of pending transactions with respect to the focus block.
      _pendingTransactionTable :: !TT.PendingTransactionTable
    }

-- We make it classy such that we can provide an instance @HasPendingTransactions (SkovData pv) pv@
-- making it easier to work with from a 'SkovData' context.
makeClassy ''PendingTransactions

-- | Data required to support 'TreeState'.
--
--  INVARIANTS:
--
--  * The current epoch (as recorded by '_currentEpoch') is either:
--
--       * the epoch of the last finalized block (as recorded by '_lastFinalized'); or
--
--       * 1 + the epoch of the last finalized block. (This is the case exactly when the last
--         finalized block's timestamp is past the epoch transition trigger time for the epoch.)
--
--  * The current epoch is at least the epoch of every live or finalized block.
--
--  * The current round is at least the round of every live or finalized block.
data SkovData (pv :: ProtocolVersion) = SkovData
    { -- | Status for the current round that is persisted to the low-level database.
      --  This is used to record when we have signed messages to ensure that we avoid double
      --  signing across node restarts.
      _persistentRoundStatus :: !PersistentRoundStatus,
      -- | The round status which holds data
      --  associated with the current round of the
      --  consensus protocol.
      _roundStatus :: !(RoundStatus pv),
      -- | Transactions.
      --  The transaction table tracks the following:
      --  * Live transactions: mapping from a 'TransactionHash' to the status of the transaction,
      --    which is either received (not associated with a block) or committed (associated with a block).
      --  * Non finalized account transactions
      --  * Non finalized chain updates
      --  See the documentation of 'TransactionTable' for more elaborate explanation of the three
      --  structures within the 'TransactionTable'.
      _transactionTable :: !TT.TransactionTable,
      -- | The purge counter for the 'TransactionTable'
      _transactionTablePurgeCounter :: !Int,
      -- | Pending transactions
      _skovPendingTransactions :: !(PendingTransactions pv),
      -- | Runtime parameters.
      _runtimeParameters :: !RuntimeParameters,
      -- | Blocks which have been included in the tree or marked as dead.
      _blockTable :: !(BlockTable pv),
      -- | Branches of the tree ordered by height above the last finalized block
      _branches :: !(Seq.Seq [BlockPointer pv]),
      -- | For non-finalized rounds, tracks which bakers we have seen legally-signed blocks with
      --  live parent blocks from. This is used for duplicate detection.
      _roundExistingBlocks :: !(Map.Map Round (Map.Map BakerId BlockSignatureWitness)),
      -- | For non finalized rounds, keep track of which rounds where we have successfully
      --  checked a 'QuorumCertificate'.
      _roundExistingQCs :: !(Map.Map Round QuorumCertificateCheckedWitness),
      -- | Genesis metadata
      _genesisMetadata :: !GenesisMetadata,
      -- | Pointer to the last finalized block.
      _lastFinalized :: !(BlockPointer pv),
      -- | A finalization entry that finalizes the last finalized block, unless that is the
      --  genesis block.
      _latestFinalizationEntry :: !(Option FinalizationEntry),
      -- | Baker and finalizer information with respect to the epoch of the last finalized block.
      --  Note: this is distinct from the current epoch.
      _skovEpochBakers :: !EpochBakers,
      -- | The current consensus statistics.
      _statistics :: !Stats.ConsensusStatistics,
      -- | Received timeouts messages in the current round.
      _currentTimeoutMessages :: !(Option TimeoutMessages),
      -- | The 'QuorumMessage's for the current 'Round'.
      --  This should be cleared whenever the consensus runner advances to a new round.
      _currentQuorumMessages :: !QuorumMessages,
      -- | Present exactly when the consensus is shutdown for a protocol update.
      --  This is the first finalized block with the 'ss1ShutdownTriggered' flag set.
      --  That is, a protocol update was effective at the time of this block, it is the
      --  trigger block in the present epoch, and it is finalized. Note that the block
      --  may or may not be finalized explicitly, but such a block is uniquely determined
      --  across all nodes. (Note that the ultimate last finalized block may not be uniquely
      --  determined, because some rounds could have both QCs and TCs.)
      _terminalBlock :: !(Option (BlockPointer pv))
    }

makeLenses ''SkovData

instance HasPendingTransactions (SkovData pv) pv where
    pendingTransactions = skovPendingTransactions
    {-# INLINE pendingTransactions #-}

instance HasEpochBakers (SkovData pv) where
    epochBakers = skovEpochBakers
    {-# INLINE epochBakers #-}

-- | Getter for accessing the genesis hash for the current genesis.
currentGenesisHash :: SimpleGetter (SkovData pv) BlockHash
currentGenesisHash = genesisMetadata . to gmCurrentGenesisHash

-- | Whether consensus is shutdown.
--  This is @True@ when:
--  * A protocol update was effective in the trigger block in this epoch.
--  * The trigger block is finalized.
isConsensusShutdown :: SimpleGetter (SkovData pv) Bool
isConsensusShutdown = terminalBlock . to isPresent

-- | Run the given action unless the consensus is already shut down.
unlessShutdown :: (MonadState (SkovData pv) m) => m () -> m ()
unlessShutdown a = do
    isShutdown <- use isConsensusShutdown
    unless isShutdown a

-- | Lens for accessing the witness that a baker signed a block in a particular round.
roundBakerExistingBlock :: Round -> BakerId -> Lens' (SkovData pv) (Maybe BlockSignatureWitness)
roundBakerExistingBlock rnd bakerId = roundExistingBlocks . at' rnd . nonEmpty . at' bakerId

-- | Remove all entries from 'roundExistingBlocks' with round less than or equal to the supplied
--  round.
purgeRoundExistingBlocks :: (MonadState (SkovData pv) m) => Round -> m ()
purgeRoundExistingBlocks rnd = roundExistingBlocks %=! snd . Map.split rnd

-- | Lens for accessing the witness that we have checked a 'QuorumCertificate' for a particular 'Round'.
roundExistingQuorumCertificate :: Round -> Lens' (SkovData pv) (Maybe QuorumCertificateCheckedWitness)
roundExistingQuorumCertificate rnd = roundExistingQCs . at' rnd

-- | Record that we have checked a 'QuorumCertificate' in the 'roundExistingQCs'.
--  The certificate should be for a round that is later than the last finalized round.
recordCheckedQuorumCertificate :: (MonadState (SkovData pv) m) => QuorumCertificate -> m ()
recordCheckedQuorumCertificate qc =
    roundExistingQuorumCertificate (qcRound qc) ?=! witness
  where
    !witness = toQuorumCertificateWitness qc

-- | Remove all entries from 'roundExistingQCs' with a 'Round' less than to the
--  supplied 'Round'.
purgeRoundExistingQCs :: (MonadState (SkovData pv) m) => Round -> m ()
purgeRoundExistingQCs rnd = roundExistingQCs %=! snd . Map.split (rnd - 1)

-- | Create an initial 'SkovData pv'
--  This constructs a 'SkovData pv' from a genesis block
--  which is suitable to grow the tree from.
--
--  In the case that this is from a genesis, then an empty transaction table
--  and empty pending transaction table must be provided.
--
--  Invariants:
--  In case that a non-empty transaction table is supplied, then we're migrating
--  from an existing state, thus the 'TransactionTable' and 'PendingTransactionTable'
--  passed in must both be from the former state.
--   * If there are any transactions in the 'TransactionTable' then the corresponding 'PendingTransactionsTable'
--     must take these transactions into account. Refer to 'PendingTransactionTable' for more details.
--   * The caller must make sure, that the supplied 'TransactionTable' does NOT contain any @Committed@ transactions
--     and all transactions have their commit point set to 0.
mkInitialSkovData ::
    -- | The 'RuntimeParameters'
    RuntimeParameters ->
    -- | Genesis metadata. State hash should match the hash of the state.
    GenesisMetadata ->
    -- | Genesis state
    PBS.HashedPersistentBlockState pv ->
    -- | The base timeout
    Duration ->
    -- | Bakers at the genesis block
    EpochBakers ->
    -- | 'TransactionTable' to initialize the 'SkovData' with.
    TT.TransactionTable ->
    -- | 'PendingTransactionTable' to initialize the 'SkovData' with.
    TT.PendingTransactionTable ->
    -- | The initial 'SkovData'
    SkovData pv
mkInitialSkovData rp genMeta genState _currentTimeout _skovEpochBakers transactionTable' pendingTransactionTable' =
    let genesisBlock = GenesisBlock genMeta
        genesisTime = timestampToUTCTime $ Base.genesisTime (gmParameters genMeta)
        genesisBlockMetadata =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = genesisTime,
                  bmArriveTime = genesisTime,
                  bmEnergyCost = 0,
                  bmTransactionsSize = 0
                }
        genesisBlockPointer =
            BlockPointer
                { bpInfo = genesisBlockMetadata,
                  bpBlock = genesisBlock,
                  bpState = genState
                }
        _persistentRoundStatus = initialPersistentRoundStatus
        _roundStatus = initialRoundStatus _currentTimeout genesisBlockPointer
        _transactionTable = transactionTable'
        _transactionTablePurgeCounter = 0
        _skovPendingTransactions =
            PendingTransactions
                { _pendingTransactionTable = pendingTransactionTable',
                  _focusBlock = genesisBlockPointer
                }
        _runtimeParameters = rp
        _blockTable = emptyBlockTable
        _branches = Seq.empty
        _roundExistingBlocks = Map.empty
        _roundExistingQCs = Map.empty
        _genesisMetadata = genMeta
        _lastFinalized = genesisBlockPointer
        _latestFinalizationEntry = Absent
        _statistics = Stats.initialConsensusStatistics
        _currentTimeoutMessages = Absent
        _currentQuorumMessages = emptyQuorumMessages
        _terminalBlock = Absent
    in  SkovData{..}

-- * Operations on the block table

-- | Get the 'BlockPointer' for a block hash that is live (not finalized).
--  Returns 'Nothing' if the block is not in the live (non-finalized) blocks.
getLiveBlock :: BlockHash -> SkovData pv -> Maybe (BlockPointer pv)
getLiveBlock blockHash sd = sd ^? blockTable . liveMap . ix blockHash

-- | Get the 'BlockPointer' for a block hash that is live or the last finalized block.
--  Returns 'Nothing' if the block is neither live nor the last finalized block.
getLiveOrLastFinalizedBlock :: BlockHash -> SkovData pv -> Maybe (BlockPointer pv)
getLiveOrLastFinalizedBlock blockHash sd
    | blockHash == getHash (sd ^. lastFinalized) = Just $! sd ^. lastFinalized
    | otherwise = getLiveBlock blockHash sd

-- | Get the 'BlockStatus' of a block that is available in memory based on the 'BlockHash'.
--  (This includes live and pending blocks, but not finalized blocks, except for the last finalized
--  block.)
--  If the block could not be found in memory then this will return 'Nothing' otherwise
--  'Just BlockStatus'.
--  This function should not be called directly, instead use either
--  'getBlockStatus' or 'getRecentBlockStatus'.
getMemoryBlockStatus :: BlockHash -> SkovData pv -> Maybe (BlockStatus pv)
getMemoryBlockStatus blockHash sd
    -- Check if it's last finalized
    | getHash (sd ^. lastFinalized) == blockHash = Just $! BlockFinalized (sd ^. lastFinalized)
    -- Check if it's the focus block
    | getHash (sd ^. focusBlock) == blockHash = Just $! BlockAlive (sd ^. focusBlock)
    -- Check if it's a live block
    | Just bp <- sd ^? blockTable . liveMap . ix blockHash = Just $! BlockAlive bp
    -- Check if it's in the dead block cache
    | memberDeadCache blockHash (sd ^. blockTable . deadBlocks) = Just BlockDead
    -- Otherwise, we don't know
    | otherwise = Nothing

-- | Create a block pointer from a stored block.
mkBlockPointer :: (MonadIO m) => LowLevel.StoredBlock pv -> m (BlockPointer pv)
mkBlockPointer sb@LowLevel.StoredBlock{..} = do
    bpState <- liftIO mkHashedPersistentBlockState
    return BlockPointer{bpInfo = stbInfo, bpBlock = stbBlock, ..}
  where
    mkHashedPersistentBlockState = do
        hpbsPointers <- newIORef $! BlobStore.blobRefToBufferedRef stbStatePointer
        let hpbsHash = blockStateHash sb
        return $! PBS.HashedPersistentBlockState{..}

-- | Get the 'BlockStatus' of a block based on the provided 'BlockHash'.
--  Note. if one does not care about old finalized blocks then
--  use 'getRecentBlockStatus' instead as it circumvents a full lookup from disk.
getBlockStatus :: (LowLevel.MonadTreeStateStore m, MonadIO m) => BlockHash -> SkovData (MPV m) -> m (BlockStatus (MPV m))
getBlockStatus blockHash sd = case getMemoryBlockStatus blockHash sd of
    Just bs -> return bs
    Nothing ->
        LowLevel.lookupBlock blockHash >>= \case
            Nothing -> return BlockUnknown
            Just storedBlock -> do
                blockPointer <- mkBlockPointer storedBlock
                return $! BlockFinalized blockPointer

-- | Get the 'RecentBlockStatus' of a block based on the provided 'BlockHash'.
--  Use this instead of 'getBlockStatus' if the contents and resulting state are not needed
--  for blocks beyond the last finalized block.
getRecentBlockStatus :: (LowLevel.MonadTreeStateStore m) => BlockHash -> SkovData (MPV m) -> m (RecentBlockStatus (MPV m))
getRecentBlockStatus blockHash sd = case getMemoryBlockStatus blockHash sd of
    Just bs -> return $! RecentBlock bs
    Nothing -> do
        LowLevel.memberBlock blockHash >>= \case
            True -> return OldFinalized
            False -> return $ RecentBlock BlockUnknown

-- | Get a finalized block by height.
--  This will return 'Nothing' for a block that is either not finalized or unknown.
getFinalizedBlockAtHeight :: (LowLevel.MonadTreeStateStore m, MonadIO m) => BlockHeight -> m (Maybe (BlockPointer (MPV m)))
getFinalizedBlockAtHeight height = do
    LowLevel.lookupBlockByHeight height >>= \case
        Nothing -> return Nothing
        Just sb -> return <$> Just =<< mkBlockPointer sb

-- | Get a list of the live or finalized blocks at a particular height. The order of the blocks in
--  the resulting list is unspecified. If there are no blocks at the given height, this will return
--  the empty list.
getBlocksAtHeight ::
    (LowLevel.MonadTreeStateStore m, MonadIO m) =>
    BlockHeight ->
    SkovData (MPV m) ->
    m [BlockPointer (MPV m)]
getBlocksAtHeight height sd = case compare height lastFinHeight of
    LT -> toList <$> getFinalizedBlockAtHeight height
    EQ -> return [sd ^. lastFinalized]
    GT -> return $ sd ^. branches . ix (fromIntegral $ height - lastFinHeight - 1)
  where
    lastFin = sd ^. lastFinalized
    lastFinHeight = bmHeight (blockMetadata lastFin)

-- | Get the first finalized block in a given epoch. The epoch can be specified either directly or
--  by a block belong to the epoch.
getFirstFinalizedBlockOfEpoch ::
    (LowLevel.MonadTreeStateStore m, MonadIO m) =>
    -- | Target epoch or a block in the target epoch.
    Either Epoch (BlockPointer (MPV m)) ->
    SkovData (MPV m) ->
    m (Maybe (BlockPointer (MPV m)))
getFirstFinalizedBlockOfEpoch epochOrBlock sd
    | targetEpoch > blockEpoch lastFin = return Nothing
    | otherwise = do
        gen <- unsafeFinalizedAtHeight 0
        let low = (0, gen)
        let high = case epochOrBlock of
                Right guess
                    | blockHeight guess <= blockHeight lastFin ->
                        (blockHeight guess, (blockEpoch guess, guess))
                _ -> (blockHeight lastFin, (blockEpoch lastFin, lastFin))
        fmap snd <$> interpolationSearchFirstM unsafeFinalizedAtHeight targetEpoch low high
  where
    targetEpoch = case epochOrBlock of
        Left epoch -> epoch
        Right block -> blockEpoch block
    unsafeFinalizedAtHeight h =
        getFinalizedBlockAtHeight h <&> \case
            Nothing -> error $ "Missing finalized block at height " ++ show h
            Just b -> (blockEpoch b, b)
    lastFin = sd ^. lastFinalized

-- | Turn a 'PendingBlock' into a live block.
--  This marks the block as 'MemBlockAlive' in the block table, records the arrive time of the block,
--  and returns the resulting 'BlockPointer'.
--  The hash of the block state MUST match the block state hash of the block; this is not checked.
--  [Note: this does not affect the '_branches' of the 'SkovData'.]
makeLiveBlock ::
    (MonadState (SkovData pv) m) =>
    -- | Pending block to make live
    PendingBlock ->
    -- | Block state associated with the block
    PBS.HashedPersistentBlockState pv ->
    BlockHeight ->
    UTCTime ->
    -- | Energy used in executing the block
    Energy ->
    m (BlockPointer pv)
makeLiveBlock pb st height arriveTime energyCost = do
    let bp =
            BlockPointer
                { bpInfo =
                    BlockMetadata
                        { bmReceiveTime = pbReceiveTime pb,
                          bmArriveTime = arriveTime,
                          bmHeight = height,
                          bmEnergyCost = energyCost,
                          bmTransactionsSize = fromIntegral $ sum (biSize <$> blockTransactions pb)
                        },
                  bpBlock = NormalBlock (pbBlock pb),
                  bpState = st
                }
    blockTable . liveMap . at' (getHash pb) ?=! bp
    return bp

-- | Marks a block as dead.
--  This expunges the block from memory
--  and registers the block in the dead cache.
--  [Note: this does not affect the '_branches' of the 'SkovData'.]
markBlockDead :: (MonadState (SkovData pv) m) => BlockHash -> m ()
markBlockDead blockHash = do
    blockTable . liveMap . at' blockHash .=! Nothing
    blockTable . deadBlocks %=! insertDeadCache blockHash

-- | Mark a live block as dead. In addition, purge the block state and maintain invariants in the
--  transaction table by purging all transaction outcomes that refer to this block.
--  [Note: this does not affect the '_branches' of the 'SkovData'.]
markLiveBlockDead ::
    ( MonadState (SkovData pv) m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv
    ) =>
    BlockPointer pv ->
    m ()
markLiveBlockDead bp = do
    let bh = getHash bp
    markBlockDead bh
    purgeBlockState $ bpState bp
    mapM_ (markTransactionDead bh) (blockTransactions bp)

-- | Update the transaction table to reflect that a list of blocks are finalized.
--  This removes them the in-memory transaction table.
--  The caller is expected to ensure that they are written to the low-level storage.
markLiveBlocksFinal ::
    (MonadState (SkovData pv) m) =>
    -- | Blocks to mark final i.e. removing them from the live block map.
    --  Note that the order of the blocks does not matter for this operation.
    [BlockPointer pv] ->
    m ()
markLiveBlocksFinal blockPointers =
    blockTable
        . liveMap
        %=! \liveBlocks -> foldl' (\blocks bp -> HM.delete (getHash bp) blocks) liveBlocks blockPointers

-- | Get the parent block of a live or finalized block. (For the genesis block, this will return
--  the block itself.)
--  Note that it is assumed that the parent is either live or finalized as otherwise this
--  function will raise an error.
parentOf ::
    (LowLevel.MonadTreeStateStore m, MonadIO m, MonadState (SkovData (MPV m)) m) =>
    BlockPointer (MPV m) ->
    m (BlockPointer (MPV m))
parentOf block
    | Present blockData <- blockBakedData block = do
        get >>= getBlockStatus (blockParent blockData) <&> \case
            BlockAliveOrFinalized bp -> bp
            _ ->
                error $
                    "parentOf: Parent block ("
                        ++ show (blockParent blockData)
                        ++ ") of "
                        ++ show (getHash @BlockHash block)
                        ++ " is not live or finalized."
    | otherwise = return block

-- | Get the parent block of a live (non-finalized) block.
--  By definition, the parent block must either also be live or be the last finalized block.
--
--  If the block is not live, this function may fail with an error.
parentOfLive :: (HasCallStack) => SkovData pv -> BlockPointer pv -> BlockPointer pv
parentOfLive sd block
    | let lastFin = sd ^. lastFinalized,
      parentHash == getHash lastFin =
        lastFin
    | Just parent <- sd ^. blockTable . liveMap . at' parentHash = parent
    | otherwise =
        error $
            "parentOfLive: parent block ("
                ++ show parentHash
                ++ ") of "
                ++ show (getHash @BlockHash block)
                ++ " is neither live nor last-finalized"
  where
    parentHash
        | Present blockData <- blockBakedData block = blockParent blockData
        | otherwise = error "parentOfLive: unexpected genesis block"

-- | Get the parent of a block where the parent is a finalized block.
--  This will produce an error if the supplied block is the genesis block, or the parent block is
--  not finalized.
parentOfFinalized :: (LowLevel.MonadTreeStateStore m, MonadIO m) => BlockPointer (MPV m) -> m (BlockPointer (MPV m))
parentOfFinalized block = do
    let parentHash
            | Present blockData <- blockBakedData block = blockParent blockData
            | otherwise = error "parentOfFinalized: unexpected genesis block"
    LowLevel.lookupBlock parentHash >>= \case
        Nothing ->
            error $
                "parentOfFinalized: parent block ("
                    ++ show parentHash
                    ++ ") of "
                    ++ show (getHash @BlockHash block)
                    ++ " is not finalized"
        Just storedBlock -> mkBlockPointer storedBlock

-- | Get the last finalized block from the perspective of a block. That is, follow the QC chain
--  back until we reach two blocks in consecutive rounds and the same epoch.
--  While the number of steps we need to go back is theoretically arbitrary, in practice it will
--  be small. Assuming a timeout base of 10 seconds and a grow factor of 1.2, the timeout would have
--  to pass 1 day if we need to go back 50 blocks, and 1 billion years if we need to go back 200
--  blocks.
lastFinalizedOf ::
    (LowLevel.MonadTreeStateStore m, MonadIO m, MonadState (SkovData (MPV m)) m) =>
    BlockPointer (MPV m) ->
    m (BlockPointer (MPV m))
lastFinalizedOf = go <=< parentOf
  where
    go block
        | blockRound block == 0 = return block
        | otherwise = do
            parent <- parentOf block
            if blockRound block == blockRound parent + 1 && blockEpoch block == blockEpoch parent
                then return parent
                else go parent

-- | Determine if one block is an ancestor of another.
--  A block is considered to be an ancestor of itself.
isAncestorOf ::
    (LowLevel.MonadTreeStateStore m, MonadIO m, MonadState (SkovData (MPV m)) m) =>
    -- | The block to check whether it's an ancestor of the other or not.
    BlockPointer (MPV m) ->
    -- | The block to carry out the ancestor check with respect to.
    BlockPointer (MPV m) ->
    m Bool
isAncestorOf b1 maybeAncestor = case compare (blockHeight b1) (blockHeight maybeAncestor) of
    GT -> return False
    EQ -> return $ (getHash b1 :: BlockHash) == getHash maybeAncestor
    LT -> do
        parent <- parentOf maybeAncestor
        b1 `isAncestorOf` parent

-- * Operations on the branches

-- | Add a newly-live block to the non-finalized branches.
--  This assumes that the parent block is either the last finalized block or already among the
--  non-finalized branches.
--  The block should not already be present in the branches; if it is, a duplicate entry may be
--  added.
--  Also note that the block heights must be consecutive otherwise this function will raise an error.
--  The latter note is enforced by the way we add pending blocks, i.e. pending blocks are awaiting
--  their parent before becoming live.
addToBranches ::
    (MonadState (SkovData pv) m) =>
    -- | The block to add to the current branches.
    BlockPointer pv ->
    m ()
addToBranches block = do
    lfbHeight <- use $ lastFinalized . to blockHeight
    let insertIndex = fromIntegral $ blockHeight block - lfbHeight - 1
    brs <- use branches
    case compare insertIndex (Seq.length brs) of
        LT -> branches . ix insertIndex %=! (block :)
        EQ -> branches %=! (Seq.|> [block])
        GT ->
            error $
                "Attempted to add a block at invalid height (" ++ show (blockHeight block) ++ ")"

-- | Get the blocks in the branches of the tree grouped by descending height.
--  That is the first element of the list is all of the blocks at 'getCurrentHeight',
--  the next is those at @getCurrentHeight - 1@, etc.
branchesFromTop :: SkovData pv -> [[BlockPointer pv]]
branchesFromTop = revSeqToList . _branches
  where
    revSeqToList Seq.Empty = []
    revSeqToList (r Seq.:|> t) = t : revSeqToList r

-- * Operations on the transaction table

-- | Lookup a transaction in the transaction table if it is live.
--  This will give a 'Nothing' result for finalized transactions.
lookupLiveTransaction :: TransactionHash -> SkovData pv -> Maybe TT.LiveTransactionStatus
lookupLiveTransaction tHash sd =
    sd ^? transactionTable . TT.ttHashMap . at tHash . traversed . _2

-- | Lookup a transaction in the transaction table, including finalized transactions.
lookupTransaction :: (LowLevel.MonadTreeStateStore m) => TransactionHash -> SkovData pv -> m (Maybe TransactionStatus)
lookupTransaction tHash sd = case lookupLiveTransaction tHash sd of
    Just liveRes -> return $ Just $ Live liveRes
    Nothing -> fmap Finalized <$> LowLevel.lookupTransaction tHash

-- | Get non-finalized transactions for the given account starting at the given nonce (inclusive).
--  These are returned as an ordered list of pairs of nonce and non-empty map of transactions and
--  their associated verification result with that nonce.
--  Transaction groups are ordered by increasing nonce.
--  Note. that there can be gaps in the nonces
--  as a transaction can be received via a pending block
getNonFinalizedAccountTransactions ::
    -- | Account to retrieve.
    AccountAddressEq ->
    -- | Starting nonce.
    Nonce ->
    SkovData pv ->
    [(Nonce, Map.Map Transaction TVer.VerificationResult)]
getNonFinalizedAccountTransactions addr nnce sd =
    case sd ^. transactionTable . TT.ttNonFinalizedTransactions . at' addr of
        Nothing -> []
        Just anfts -> case atnnce of
            Nothing -> Map.toAscList beyond
            Just s -> (nnce, s) : Map.toAscList beyond
          where
            (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. TT.anftMap)

-- | Get the non finalized chain updates.
--  This returns a map from update sequence numbers to the
--  the corresponding chain updates groups.
--  The chain update groups are ordered by increasing
--  sequence number.
--  Note. that there can be gaps in the sequence numbers
--  as a transaction can be received via a pending block.
getNonFinalizedChainUpdates ::
    -- | The 'UpdateType' to retrieve.
    UpdateType ->
    -- | The starting sequence number.
    UpdateSequenceNumber ->
    SkovData pv ->
    -- | The resulting list of chain updates.
    [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]
getNonFinalizedChainUpdates uType updateSequenceNumber sd = do
    case sd ^. transactionTable . TT.ttNonFinalizedChainUpdates . at' uType of
        Nothing -> []
        Just nfcus -> case atsn of
            Nothing -> Map.toAscList beyond
            Just s -> (updateSequenceNumber, s) : Map.toAscList beyond
          where
            (_, atsn, beyond) = Map.splitLookup updateSequenceNumber (nfcus ^. TT.nfcuMap)

-- | Get a non finalized credential by its 'TransactionHash'
--  This returns 'Nothing' in the case that the credential has already been finalized.
getNonFinalizedCredential ::
    -- | 'TransactionHash' for the transaction that contained the 'CredentialDeployment'.
    TransactionHash ->
    -- | The 'SkovData pv' to query the non finalized credential from.
    SkovData pv ->
    Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult)
getNonFinalizedCredential txhash sd = do
    case sd ^? transactionTable . TT.ttHashMap . ix txhash of
        Just (WithMetadata{wmdData = CredentialDeployment{..}, ..}, status) ->
            case status of
                TT.Received _ verRes -> Just (WithMetadata{wmdData = biCred, ..}, verRes)
                TT.Committed _ verRes _ -> Just (WithMetadata{wmdData = biCred, ..}, verRes)
        _ -> Nothing

-- | Get the next account nonce for an account.
--  Returns a tuple consisting of the successor of the
--  current account nonce and a boolean value indicating
--  that there are no pending or committed (but only finalized) transactions
--  tied to this account.
getNextAccountNonce ::
    ( BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m)
    ) =>
    -- | The 'AccountAddressEq' to get the next available nonce for.
    --  This will work for account aliases as this is an 'AccountAddressEq'
    --  and not just a 'AccountAddress'.
    AccountAddressEq ->
    -- | The 'SkovData pv' to query the next account nonce from.
    SkovData (MPV m) ->
    -- | The resulting account nonce and whether it is finalized or not.
    m (Nonce, Bool)
getNextAccountNonce addr sd =
    fetchFromTransactionTable >>= \case
        Just ttResult -> return ttResult
        Nothing -> fetchFromLastFinalizedBlock
  where
    fetchFromTransactionTable = return $! TT.nextAccountNonce addr (sd ^. transactionTable)
    fetchFromLastFinalizedBlock = do
        macct <- getAccount (sd ^. lastFinalized . to bpState) (aaeAddress addr)
        nextNonce <- fromMaybe minNonce <$> mapM (getAccountNonce . snd) macct
        return (nextNonce, True)
{-# INLINE getNextAccountNonce #-}

-- | Finalizes a list of transactions in the in-memory transaction table.
--  This removes the transactions (and any others with the same account and nonce, or update type
--  and sequence number) and updates the next account nonce/update type sequence number.
--  Per account, the transactions must be in
--  continuous sequence by nonce, starting from the next available non-finalized
--  nonce. This does not write the transactions to the low-level tree state database, but just
--  updates the in-memory transaction table accordingly.
finalizeTransactions ::
    ( MonadState (SkovData (MPV m)) m,
      MonadThrow m
    ) =>
    -- | The transactions to remove from the state.
    [BlockItem] ->
    m ()
finalizeTransactions = mapM_ removeTrans
  where
    removeTrans WithMetadata{wmdData = NormalTransaction tr, ..} = do
        let nonce = transactionNonce tr
            sender = accountAddressEmbed (transactionSender tr)
        tt <- use transactionTable
        let mAnft = tt ^? TT.ttNonFinalizedTransactions . ix sender
        case mAnft of
            Nothing -> throwM . TreeStateInvariantViolation $ "When finalizing transactions there was no recorded next nonce for sender " <> show sender
            Just anft -> do
                let nfn = anft ^. TT.anftMap . at' nonce . non Map.empty
                    wmdtr = WithMetadata{wmdData = tr, ..}
                unless (Map.member wmdtr nfn) $
                    throwM . TreeStateInvariantViolation $
                        "Tried to finalize transaction which is not known to be in the set of \
                        \non-finalized transactions for the sender "
                            ++ show sender
                -- Remove the transaction, and any other transactions with the same (account, nonce),
                -- from the transaction table.
                -- They can never be part of any other block after this point.
                forM_ (Map.keys nfn) $
                    \deadTransaction -> transactionTable . TT.ttHashMap . at' (getHash deadTransaction) .= Nothing
                -- Update the non-finalized transactions for the sender
                transactionTable
                    . TT.ttNonFinalizedTransactions
                    . at' sender
                    ?=! ( anft
                            & (TT.anftMap . at' nonce .~ Nothing)
                            & (TT.anftNextNonce .~ nonce + 1)
                        )
    removeTrans WithMetadata{wmdData = CredentialDeployment{}, ..} = do
        transactionTable . TT.ttHashMap . at' wmdHash .= Nothing
    removeTrans WithMetadata{wmdData = ChainUpdate cu, ..} = do
        let sn = updateSeqNumber (uiHeader cu)
            uty = updateType (uiPayload cu)
        nfcu <- use (transactionTable . TT.ttNonFinalizedChainUpdates . at' uty . non TT.emptyNFCU)
        unless (nfcu ^. TT.nfcuNextSequenceNumber == sn) $
            throwM . TreeStateInvariantViolation $
                "The recorded next sequence number for update type "
                    ++ show uty
                    ++ " ("
                    ++ show (nfcu ^. TT.nfcuNextSequenceNumber)
                    ++ ") doesn't match the one that is going to be finalized ("
                    ++ show sn
                    ++ ")"
        let nfsn = nfcu ^. TT.nfcuMap . at' sn . non Map.empty
            wmdcu = WithMetadata{wmdData = cu, ..}
        unless (Map.member wmdcu nfsn) $
            throwM . TreeStateInvariantViolation $
                "Tried to finalize a chain update that is not known to be in the set of \
                \non-finalized chain updates of type "
                    ++ show uty
        -- Remove the transaction from the in-memory table, together with
        -- any other updates with the same sequence number, since they weren't finalized
        forM_ (Map.keys nfsn) $
            \deadUpdate -> transactionTable . TT.ttHashMap . at' (getHash deadUpdate) .= Nothing
        -- Update the non-finalized chain updates
        transactionTable
            . TT.ttNonFinalizedChainUpdates
            . at' uty
            ?=! (nfcu & (TT.nfcuMap . at' sn .~ Nothing) & (TT.nfcuNextSequenceNumber .~ sn + 1))

-- | Mark a live transaction as committed in a particular block.
--  This does nothing if the transaction is not live.
--  A committed transaction cannot be purged while it is committed for a round after the round of
--  the last finalized block.
commitTransaction ::
    (MonadState (SkovData pv) m) =>
    -- | Round of the block
    Round ->
    -- | The 'BlockHash' that the transaction should
    --  be committed to.
    BlockHash ->
    -- | The 'TransactionIndex' in the block.
    TransactionIndex ->
    -- | The transaction to commit.
    BlockItem ->
    m ()
commitTransaction rnd bh ti transaction =
    transactionTable
        . TT.ttHashMap
        . at' (getHash transaction)
        . traversed
        . _2
        %=! TT.addResult bh rnd ti

-- | Add a transaction to the transaction table if its nonce/sequence number is at least the next
--  non-finalized nonce/sequence number. The return value is 'True' if and only if the transaction
--  was added.
--  When adding a transaction from a block, use the 'Round' of the block. Otherwise use round @0@.
--  The transaction must not already be present.
addTransaction ::
    (MonadState (SkovData (MPV m)) m) =>
    Round ->
    BlockItem ->
    TVer.VerificationResult ->
    m Bool
addTransaction rnd transaction verRes = do
    added <- transactionTable %%=! TT.addTransaction transaction (TT.commitPoint rnd) verRes
    when added $ transactionTablePurgeCounter += 1
    return added

-- | Mark the provided transaction as dead for the provided 'BlockHash'.
markTransactionDead ::
    (MonadState (SkovData pv) m) =>
    -- | The 'BlockHash' where the transaction was committed.
    BlockHash ->
    -- | The 'BlockItem' to mark as dead.
    BlockItem ->
    m ()
markTransactionDead blockHash transaction =
    transactionTable
        . TT.ttHashMap
        . at' (getHash transaction)
        . mapped
        . _2
        %= TT.markDeadResult blockHash

-- | Purge transactions from the transaction table and pending transactions.
--  Transactions are purged only if they are not included in a live block, and
--  have either expired or arrived longer ago than the transaction keep alive time.
--
--  If the first argument is @False@, the transaction table is only purged if
--  'rpInsertionsBeforeTransactionPurged' transactions have been inserted since
--  the last purge.  If it is true, the table is purged regardless.
--
--    * Every 'BlockItem' in the transaction table that is not included in a live
--      or finalized block is referenced in the pending transaction table.  That is,
--      for a basic transaction the '_pttWithSender' table contains an entry for
--      the sender where the nonce of the transaction falls within the range,
--      and for a credential deployment the transaction hash is included in '_pttDeployCredential'.
--
--    * The low nonce for each entry in '_pttWithSender' is at least the last finalized
--      nonce recorded in the account's non-finalized transactions in the transaction
--      table.
--
--    * The pending transaction table only references transactions that are in the
--      transaction table.  That is, the high nonce in a range is a tight bound and
--      the deploy credential hashes correspond to transactions in the table.
--
--    * No non-finalized block is considered live or will become live if its round
--      is less than or equal to the slot number of the last finalized block.
--
--    * If a transaction is known to be in any block that is not finalized or dead,
--      then 'commitTransaction' or 'addCommitTransaction' has been called with a
--      slot number at least as high as the slot number of the block.
purgeTransactionTable ::
    (MonadState (SkovData pv) m) =>
    -- | Whether to force the purging.
    Bool ->
    -- | The current time.
    UTCTime ->
    m ()
purgeTransactionTable force currentTime = do
    purgeCount <- use transactionTablePurgeCounter
    RuntimeParameters{..} <- use runtimeParameters
    when (force || purgeCount > rpInsertionsBeforeTransactionPurge) $ do
        transactionTablePurgeCounter .= 0
        lfb <- use lastFinalized
        let lastFinalizedRound = blockRound $! bpBlock lfb
        transactionTable' <- use transactionTable
        pendingTransactions' <- use pendingTransactionTable
        let
            currentTransactionTime = utcTimeToTransactionTime currentTime
            oldestArrivalTime =
                if currentTransactionTime > rpTransactionsKeepAliveTime
                    then currentTransactionTime - rpTransactionsKeepAliveTime
                    else 0
            currentTimestamp = utcTimeToTimestamp currentTime
            (newTT, newPT) = Purge.purgeTables (TT.commitPoint lastFinalizedRound) oldestArrivalTime currentTimestamp transactionTable' pendingTransactions'
        transactionTable .=! newTT
        pendingTransactionTable .=! newPT

-- ** Operations on the pending transaction table

-- | Update the focus block and the pending transaction table.
--
--  PRECONDITION: The new focus block must be a live block, or the last finalized block.
updateFocusBlockTo ::
    forall m.
    (MonadState (SkovData (MPV m)) m) =>
    -- | New focus block
    BlockPointer (MPV m) ->
    m ()
updateFocusBlockTo newFocusBlock = do
    -- 'parent' will be a function that gets the parent of live blocks in the present state.
    parent <- gets parentOfLive
    -- 'updatePTs' rolls back the branch to the old pending block from the nearest common ancestor
    -- with the new pending block, then rolls forward the branch to the new pending block.
    let updatePTs oldBranchBlock newBranchBlock forwardBlocks pendingTable =
            case compare (blockHeight oldBranchBlock) (blockHeight newBranchBlock) of
                LT ->
                    -- If the new branch height is greater than the old branch height, then go to
                    -- the parent of the new branch, and add the new branch block to the stack of
                    -- blocks to roll forward the the transactions of.
                    updatePTs
                        oldBranchBlock
                        (parent newBranchBlock)
                        (newBranchBlock : forwardBlocks)
                        pendingTable
                EQ
                    | oldBranchBlock == newBranchBlock ->
                        -- We've reached the common ancestor, so forward the transactions of the
                        -- blocks in the stack.
                        foldl'
                            (\p f -> TT.forwardPTT (blockTransactions f) p)
                            pendingTable
                            forwardBlocks
                    | otherwise -> do
                        -- The branches are at the same height, but we've not yet hit the common
                        -- ancestor, so roll back the transactions of the old block, add the new
                        -- block to the stack of blocks to roll forward, and proceed with the
                        -- parents of both blocks.
                        updatePTs
                            (parent oldBranchBlock)
                            (parent newBranchBlock)
                            (newBranchBlock : forwardBlocks)
                            (TT.reversePTT (blockTransactions oldBranchBlock) pendingTable)
                GT ->
                    -- If the new branch height is lower than the old branch height, roll back the
                    -- transactions from the old block and continue with the parent of the old
                    -- branch.
                    updatePTs
                        (parent oldBranchBlock)
                        newBranchBlock
                        forwardBlocks
                        (TT.reversePTT (blockTransactions oldBranchBlock) pendingTable)
    oldFocusBlock <- use focusBlock
    pendingTransactions . pendingTransactionTable %=! updatePTs oldFocusBlock newFocusBlock []
    focusBlock .=! newFocusBlock

-- * Bakers

-- | Get the bakers and finalizers for a given epoch if they are available.
--  They are always available for an epoch within 1 of the epoch of the last finalized block.
--  Additionally, they may be available for future epochs in the same payday as the last finalized
--  block.
--  Returns 'Nothing' if the bakers and finalizers are not available.
getBakersForEpoch :: Epoch -> SkovData pv -> Maybe BakersAndFinalizers
getBakersForEpoch e s
    | e == curEpoch = Just (s ^. currentEpochBakers)
    | e == curEpoch + 1 = Just (s ^. nextEpochBakers)
    | e == curEpoch - 1 = Just (s ^. previousEpochBakers)
    | curEpoch <= e && e < s ^. nextPayday = Just (s ^. currentEpochBakers)
    | otherwise = Nothing
  where
    curEpoch = s ^. lastFinalized . to blockEpoch

-- | Get the bakers at the current epoch.
--  This relies on the fact that the current epoch is either the same as the epoch of the last
--  finalized block, or the next epoch.
bakersForCurrentEpoch :: SkovData pv -> BakersAndFinalizers
bakersForCurrentEpoch sd
    | sd ^. roundStatus . rsCurrentEpoch == sd ^. lastFinalized . to blockEpoch =
        sd ^. currentEpochBakers
    | otherwise =
        assert (sd ^. roundStatus . rsCurrentEpoch == (sd ^. lastFinalized . to blockEpoch) + 1) $
            sd ^. nextEpochBakers

-- * Protocol update

-- | Clear pending and non-finalized blocks from the tree state.
--  'Received' transactions have their 'CommitPoint' reset.
--  Transactions that were 'Committed' (to any non-finalized block) have
--  their status changed to 'Received' and their 'CommitPoint' is reset.
clearOnProtocolUpdate :: (MonadState (SkovData pv) m) => m ()
clearOnProtocolUpdate = do
    -- clear the block table
    blockTable .=! emptyBlockTable
    -- clear the branches
    branches .=! Seq.empty
    -- mark committed transactions as received, since we have dropped any blocks
    -- that they belong to.
    transactionTable
        . TT.ttHashMap
        %=! HM.map
            ( \case
                (bi, TT.Received{..}) -> (bi, TT.Received{TT._tsCommitPoint = 0, ..})
                (bi, TT.Committed{..}) -> (bi, TT.Received{TT._tsCommitPoint = 0, ..})
            )

-- | Clear the transaction table and pending transactions, ensure that the block states are archived,
--  and collapse the block state caches.
clearAfterProtocolUpdate :: (MonadState (SkovData pv) m, BlockStateStorage m, GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv) => m ()
clearAfterProtocolUpdate = do
    -- Clear the transaction table and pending transactions.
    transactionTable .=! TT.emptyTransactionTable
    pendingTransactionTable .=! TT.emptyPendingTransactionTable
    -- Set the focus block to the last finalized block.
    lastFinBlock <- use lastFinalized
    focusBlock .=! lastFinBlock
    -- Archive the last finalized block state.
    archiveBlockState $ bpState lastFinBlock
    -- Archive the terminal block state.
    termBlock <- use terminalBlock
    forM_ termBlock (archiveBlockState . bpState)
    collapseCaches

-- | Sets and persists the 'PersistentRoundStatus' of the 'SkovData'.
setPersistentRoundStatus :: (LowLevel.MonadTreeStateStore m, MonadState (SkovData (MPV m)) m) => PersistentRoundStatus -> m ()
setPersistentRoundStatus = updatePersistentRoundStatus . const

-- | Updates and persists the 'PersistentRoundStatus' of the 'SkovData'.
updatePersistentRoundStatus ::
    (LowLevel.MonadTreeStateStore m, MonadState (SkovData (MPV m)) m) =>
    (PersistentRoundStatus -> PersistentRoundStatus) ->
    m ()
updatePersistentRoundStatus change = do
    newRoundStatus <- persistentRoundStatus <%=! change
    LowLevel.writeCurrentRoundStatus newRoundStatus
