{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module exposes the API of the treestate.
-- The tree state is stores the data required for ensuring progressing rounds
-- in the consensus protocol (V1).
-- The tree state consists of a transient part and a persistent part.
-- This module contains the relevant updates to the transient part, while some
-- queries makes use of 'Concordium.KonsensusV1.TreeState.LowLevel'.
--
-- State should be retained in memory until it is either finalized or marked dead.
--
-- The 'Concordium.KonsensusV1.TreeState.LowLevel' module exposes the API
-- required for persisting state. In particular this module assumes that
-- the caller persists data upon finalization via the said module above.
module Concordium.KonsensusV1.TreeState.Implementation where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Time
import Data.Typeable
import Lens.Micro.Platform

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq

import qualified Concordium.Genesis.Data.BaseV1 as Base
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache, insertDeadCache, memberDeadCache)
import qualified Concordium.GlobalState.PurgeTransactions as Purge
import qualified Concordium.GlobalState.Statistics as Stats
import Concordium.GlobalState.TransactionTable
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TransactionVerification
import qualified Concordium.TransactionVerification as TVer

-- |Exception occurring from a violation of tree state invariants.
newtype TreeStateInvariantViolation = TreeStateInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception TreeStateInvariantViolation where
    displayException (TreeStateInvariantViolation reason) =
        "Tree state invariant violation: "
            ++ show reason

-- |Status of a block that is held in memory i.e.
-- the block is either pending or alive.
-- Parameterized by the 'BlockPointer' and the 'SignedBlock'.
data InMemoryBlockStatus pv
    = -- |The block is awaiting its parent to become part of chain.
      MemBlockPending !PendingBlock
    | -- |The block is alive i.e. head of chain.
      MemBlockAlive !(BlockPointer pv)
    deriving (Show)

-- |The block table yields blocks that are
-- either alive or pending.
-- Furthermore it holds a fixed size cache of hashes
-- of blocks marked as dead.
data BlockTable pv = BlockTable
    { -- |Cache for dead blocks.
      -- See documentation of 'DeadCache' for an
      -- elaborate description of it.
      --
      -- But caching the dead blocks is beneficial
      -- when receiving blocks and evaluating if they can
      -- become part of the tree or not (accepted or not).
      -- Hence with a cache of dead blocks we can immediately
      -- reject a received block if it points to a block marked as dead
      -- as opposed to the case where it otherwise would be seen as being "unknown"
      -- if we simply expunged dead blocks.
      _deadBlocks :: !DeadCache,
      -- |Map of live blocks.
      -- Blocks are removed from this map by two means;
      --
      -- * When a block becomes finalized it is
      -- being persisted (and so is the predecessor live blocks)
      -- and removed from this cache.
      --
      -- * When a block is being marked as dead.
      _liveMap :: !(HM.HashMap BlockHash (InMemoryBlockStatus pv))
    }
    deriving (Show)

makeLenses ''BlockTable

-- |Create the empty block table.
emptyBlockTable :: BlockTable pv
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |The 'PendingTransactions' consists of a "focus block", which is a live block, and a pending
-- transaction table that is with respect to the focus block.
--
-- The focus block, is the block that the consensus layer will extend a new block from.
-- Thus it must be the case, that the focus block is always a successor of the latest finalized block,
-- usually the focus block will the best block unless branching and pruning occurs.
-- Hence the focus block can be rolled back if we are pruning a branch of the tree and
-- we are starting to extend the tree from a predecessor block.
--
-- The 'PendingTransactionTable' is special for the focus block, as it records
-- the pending transactions for that given block (the focus block).
--
-- Hence, itt must always be the case that a nonce from the perspective of the focus block
-- is the same as recorded in the 'PendingTransactionTable'.
data PendingTransactions pv = PendingTransactions
    { -- |The block with respect to which the pending transactions are considered pending.
      _focusBlock :: !(BlockPointer pv),
      -- |The table of pending transactions with respect to the focus block.
      _pendingTransactionTable :: !PendingTransactionTable
    }

-- We make it classy such that we can provide an instance @HasPendingTransactions (SkovData pv) pv@
-- making it easier to work with from a 'SkovData' context.
makeClassy ''PendingTransactions

-- | Pending blocks are conceptually stored in a min priority search queue,
-- where multiple blocks may have the same key, which is their parent,
-- and the priority is the block's round number.
-- When a block arrives (possibly dead), its pending children are removed
-- from the queue and handled.  This uses 'takePendingChildren'.
-- When a block is finalized, all pending blocks with a lower or equal round
-- number can be handled (they will become dead, since they can no longer
-- join the tree).  This uses 'takeNextPendingUntil'.
data PendingBlocks = PendingBlocks
    { -- |Pending blocks i.e. blocks that have not yet been included in the tree.
      -- The entries of the pending blocks are keyed by the 'BlockHash' of their parent block.
      _pendingBlocksTable :: !(HM.HashMap BlockHash [PendingBlock]),
      -- |A priority search queue on the (pending block hash, parent of pending block hash) tuple,
      -- prioritised by the round of the pending block. The queue in particular supports extracting
      -- the pending block with minimal 'Round'. Note that the queue can contain blocks that are
      -- not actually pending, hence it does not have an entry in the '_pendingBlocksTable'.
      -- This will be the case if the block has either become live or marked for dead.
      _pendingBlocksQueue :: !(MPQ.MinPQueue Round (BlockHash, BlockHash))
    }
    deriving (Eq, Show)

makeClassy ''PendingBlocks

-- |A 'PendingBlocks' with no blocks.
emptyPendingBlocks :: PendingBlocks
emptyPendingBlocks =
    PendingBlocks
        { _pendingBlocksTable = HM.empty,
          _pendingBlocksQueue = MPQ.empty
        }

-- |Data required to support 'TreeState'.
data SkovData (pv :: ProtocolVersion) = SkovData
    { -- |The round status which holds data
      -- associated with the current round of the
      -- consensus protocol.
      _roundStatus :: !RoundStatus,
      -- |Transactions.
      -- The transaction table tracks the following:
      -- * Live tranactions: mapping from a 'TransactionHash' to the status of the transaction,
      --   which is either received (not associated with a block) or comitted (associated with a block).
      -- * Non finalized account transactions
      -- * Non finalized chain updates
      -- See the documentation of 'TransactionTable' for more elaborate explanation of the three
      -- structures within the 'TransactionTable'.
      _transactionTable :: !TransactionTable,
      -- |The purge counter for the 'TransactionTable'
      _transactionTablePurgeCounter :: !Int,
      -- |Pending transactions
      _skovPendingTransactions :: !(PendingTransactions pv),
      -- |Runtime parameters.
      _runtimeParameters :: !RuntimeParameters,
      -- |Blocks which have been included in the tree or marked as dead.
      _blockTable :: !(BlockTable pv),
      -- |Branches of the tree by height above the last finalized block
      _branches :: !(Seq.Seq [BlockPointer pv]),
      -- |Genesis metadata
      _genesisMetadata :: !GenesisMetadata,
      -- |Pending blocks
      _skovPendingBlocks :: !PendingBlocks,
      -- |Pointer to the last finalized block.
      _lastFinalized :: !(BlockPointer pv),
      -- |The current consensus statistics.
      _statistics :: !Stats.ConsensusStatistics
    }

makeLenses ''SkovData

instance HasPendingTransactions (SkovData pv) pv where
    pendingTransactions = skovPendingTransactions
    {-# INLINE pendingTransactions #-}

instance HasPendingBlocks (SkovData pv) where
    pendingBlocks = skovPendingBlocks
    {-# INLINE pendingBlocks #-}

-- |Create an initial 'SkovData pv'
-- This constructs a 'SkovData pv' from a genesis block
-- which is suitable to grow the tree from.
mkInitialSkovData ::
    -- |The 'RuntimeParameters'
    RuntimeParameters ->
    -- |Genesis metadata. State hash should match the hash of the state.
    GenesisMetadata ->
    -- |Genesis state
    PBS.HashedPersistentBlockState pv ->
    -- |The base timeout
    Duration ->
    -- |The 'LeadershipElectionNonce'
    LeadershipElectionNonce ->
    -- |The initial 'SkovData'
    SkovData pv
mkInitialSkovData rp genMeta genState baseTimeout len =
    let genesisBlock = GenesisBlock genMeta
        genesisTime = timestampToUTCTime $ Base.genesisTime (gmParameters genMeta)
        genesisBlockMetadata =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = genesisTime,
                  bmArriveTime = genesisTime
                }
        genesisBlockPointer =
            BlockPointer
                { bpInfo = genesisBlockMetadata,
                  bpBlock = genesisBlock,
                  bpState = genState
                }
        _roundStatus = initialRoundStatus baseTimeout len
        _transactionTable = emptyTransactionTable
        _transactionTablePurgeCounter = 0
        _skovPendingTransactions =
            PendingTransactions
                { _pendingTransactionTable = emptyPendingTransactionTable,
                  _focusBlock = genesisBlockPointer
                }
        _runtimeParameters = rp
        _blockTable = emptyBlockTable
        _branches = Seq.empty
        _genesisMetadata = genMeta
        _skovPendingBlocks = emptyPendingBlocks
        _lastFinalized = genesisBlockPointer
        _statistics = Stats.initialConsensusStatistics
    in  SkovData{..}

-- * Operations on the block table

-- |Get the 'BlockStatus' of a block that is available in memory based on the 'BlockHash'.
-- (This includes live and pending blocks, but not finalized blocks, except for the last finalized
-- block.)
-- If the block could not be found in memory then this will return 'Nothing' otherwise
-- 'Just BlockStatus'.
-- This function should not be called directly, instead use either
-- 'getBlockStatus' or 'getRecentBlockStatus'.
getMemoryBlockStatus :: BlockHash -> SkovData pv -> Maybe (BlockStatus pv)
getMemoryBlockStatus blockHash sd
    -- Check if it's last finalized
    | getHash (sd ^. lastFinalized) == blockHash = Just $! BlockFinalized (sd ^. lastFinalized)
    -- Check if it's the focus block
    | getHash (sd ^. focusBlock) == blockHash = Just $! BlockAlive (sd ^. focusBlock)
    -- Check if it's a pending or live block
    | Just status <- sd ^? blockTable . liveMap . ix blockHash = case status of
        MemBlockPending sb -> Just $! BlockPending sb
        MemBlockAlive bp -> Just $! BlockAlive bp
    -- Check if it's in the dead block cache
    | memberDeadCache blockHash (sd ^. blockTable . deadBlocks) = Just BlockDead
    -- Otherwise, we don't know
    | otherwise = Nothing

-- |Create a block pointer from a stored block.
mkBlockPointer :: (LowLevel.MonadTreeStateStore m, MonadIO m) => LowLevel.StoredBlock (MPV m) -> m (BlockPointer (MPV m))
mkBlockPointer sb@LowLevel.StoredBlock{..} = do
    bpState <- liftIO mkHashedPersistentBlockState
    return BlockPointer{bpInfo = stbInfo, bpBlock = stbBlock, ..}
  where
    mkHashedPersistentBlockState = do
        hpbsPointers <- newIORef $ BlobStore.blobRefToBufferedRef stbStatePointer
        let hpbsHash = blockStateHash sb
        return $! PBS.HashedPersistentBlockState{..}

-- |Get the 'BlockStatus' of a block based on the provided 'BlockHash'.
-- Note. if one does not care about old finalized blocks then
-- use 'getRecentBlockStatus' instead as it circumvents a full lookup from disk.
getBlockStatus :: (LowLevel.MonadTreeStateStore m, MonadIO m) => BlockHash -> SkovData (MPV m) -> m (BlockStatus (MPV m))
getBlockStatus blockHash sd = case getMemoryBlockStatus blockHash sd of
    Just bs -> return bs
    Nothing ->
        LowLevel.lookupBlock blockHash >>= \case
            Nothing -> return BlockUnknown
            Just storedBlock -> do
                blockPointer <- mkBlockPointer storedBlock
                return $! BlockFinalized blockPointer

-- |Get the 'RecentBlockStatus' of a block based on the provided 'BlockHash'.
-- Use this instead of 'getBlockStatus' if the contents and resulting state are not needed
-- for blocks beyond the last finalized block.
getRecentBlockStatus :: (LowLevel.MonadTreeStateStore m) => BlockHash -> SkovData (MPV m) -> m (RecentBlockStatus (MPV m))
getRecentBlockStatus blockHash sd = case getMemoryBlockStatus blockHash sd of
    Just bs -> return $! RecentBlock bs
    Nothing -> do
        LowLevel.memberBlock blockHash >>= \case
            True -> return OldFinalized
            False -> return Unknown

-- |Get a finalized block by height.
-- This will return 'Nothing' for a block that is either not finalized or unknown.
getFinalizedBlockAtHeight :: (LowLevel.MonadTreeStateStore m, MonadIO m) => BlockHeight -> m (Maybe (BlockPointer (MPV m)))
getFinalizedBlockAtHeight height = do
    LowLevel.lookupBlockByHeight height >>= \case
        Nothing -> return Nothing
        Just sb -> return <$> Just =<< mkBlockPointer sb

-- |Turn a 'PendingBlock' into a live block.
-- This marks the block as 'MemBlockAlive' in the block table, records the arrive time of the block,
-- and returns the resulting 'BlockPointer'.
-- The hash of the block state MUST match the block state hash of the block; this is not checked.
-- [Note: this does not affect the '_branches' of the 'SkovData'.]
makeLiveBlock :: (MonadState (SkovData pv) m) => PendingBlock -> PBS.HashedPersistentBlockState pv -> BlockHeight -> UTCTime -> m (BlockPointer pv)
makeLiveBlock pb st height arriveTime = do
    let bp =
            BlockPointer
                { bpInfo = BlockMetadata{bmReceiveTime = pbReceiveTime pb, bmArriveTime = arriveTime, bmHeight = height},
                  bpBlock = NormalBlock (pbBlock pb),
                  bpState = st
                }
    blockTable . liveMap . at' (getHash pb) ?=! MemBlockAlive bp
    return bp

-- |Marks a block as dead.
-- This expunges the block from memory
-- and registers the block in the dead cache.
-- [Note: this does not affect the '_branches' of the 'SkovData'.]
markBlockDead :: (MonadState (SkovData pv) m) => BlockHash -> m ()
markBlockDead blockHash = do
    blockTable . liveMap . at' blockHash .=! Nothing
    blockTable . deadBlocks %=! insertDeadCache blockHash

-- |Mark a live block as dead. In addition, purge the block state and maintain invariants in the
-- transaction table by purging all transaction outcomes that refer to this block.
-- [Note: this does not affect the '_branches' of the 'SkovData'.]
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

-- |Mark a block as pending in the block table.
-- (Note, this does not update the pending block table.)
markPending :: (MonadState (SkovData pv) m) => PendingBlock -> m ()
markPending pb = blockTable . liveMap . at' (getHash pb) ?=! MemBlockPending pb

-- * Operations on pending blocks

-- $pendingBlocks
-- Pending blocks are conceptually stored in a min priority search queue,
-- where multiple blocks may have the same key, which is their parent,
-- and the priority is the block's round number.
-- When a block arrives (possibly dead), its pending children are removed
-- from the queue and handled.  This uses 'takePendingChildren'.
-- When a block is finalized, all pending blocks with a lower or equal round
-- number can be handled (they will become dead, since they can no longer
-- join the tree).  This uses 'takeNextPendingUntil'.

-- |Add a block to the pending block table and queue.
-- [Note: this does not affect the '_branches' of the 'SkovData'.]
addPendingBlock :: (MonadState s m, HasPendingBlocks s) => PendingBlock -> m ()
addPendingBlock pb = do
    pendingBlocksQueue %= MPQ.insert theRound (blockHash, parentHash)
    pendingBlocksTable . at' parentHash . non [] %= (pb :)
  where
    blockHash = getHash pb
    theRound = blockRound pb
    parentHash = blockParent pb

-- |Take the set of blocks that are pending a particular parent from the pending block table.
-- Note: this does not remove them from the pending blocks queue; blocks should be removed from
-- the queue as the finalized round progresses.
takePendingChildren :: (MonadState s m, HasPendingBlocks s) => BlockHash -> m [PendingBlock]
takePendingChildren parent = pendingBlocksTable . at' parent . non [] <<.= []

-- |Return the next block that is pending its parent with round number
-- less than or equal to the given value, removing it from the pending
-- table.  Returns 'Nothing' if there is no such pending block.
takeNextPendingUntil :: (MonadState s m, HasPendingBlocks s) => Round -> m (Maybe PendingBlock)
takeNextPendingUntil targetRound = takeNextUntil =<< use pendingBlocksQueue
  where
    takeNextUntil pbq = case MPQ.minViewWithKey pbq of
        Just ((r, (pending, parent)), pbq')
            | r <= targetRound -> do
                (myPB, otherPBs) <-
                    List.partition ((== pending) . getHash)
                        <$> use (pendingBlocksTable . at' parent . non [])
                case myPB of
                    [] -> takeNextUntil pbq' -- Block is no longer actually pending
                    (realPB : _) -> do
                        pendingBlocksTable . at' parent . non [] .= otherPBs
                        pendingBlocksQueue .= pbq'
                        return (Just realPB)
        _ -> do
            pendingBlocksQueue .=! pbq
            return Nothing

-- * Operations on the transaction table

-- |Lookup a transaction in the transaction table if it is live.
-- This will give a 'Nothing' result for finalized transactions.
lookupLiveTransaction :: TransactionHash -> SkovData pv -> Maybe LiveTransactionStatus
lookupLiveTransaction tHash sd =
    sd ^? transactionTable . ttHashMap . at tHash . traversed . _2

-- |Lookup a transaction in the transaction table, including finalized transactions.
lookupTransaction :: (LowLevel.MonadTreeStateStore m) => TransactionHash -> SkovData pv -> m (Maybe TransactionStatus)
lookupTransaction tHash sd = case lookupLiveTransaction tHash sd of
    Just liveRes -> return $ Just $ Live liveRes
    Nothing -> fmap Finalized <$> LowLevel.lookupTransaction tHash

-- |Get non-finalized transactions for the given account starting at the given nonce (inclusive).
-- These are returned as an ordered list of pairs of nonce and non-empty map of transactions and
-- their associated verification result with that nonce.
-- Transaction groups are ordered by increasing nonce.
-- Note. that there can be gaps in the nonces
-- as a transaction can be received via a pending block
getNonFinalizedAccountTransactions ::
    -- |Account to retrieve.
    AccountAddressEq ->
    -- |Starting nonce.
    Nonce ->
    SkovData pv ->
    [(Nonce, Map.Map Transaction TVer.VerificationResult)]
getNonFinalizedAccountTransactions addr nnce sd =
    case sd ^. transactionTable . ttNonFinalizedTransactions . at' addr of
        Nothing -> []
        Just anfts -> case atnnce of
            Nothing -> Map.toAscList beyond
            Just s -> (nnce, s) : Map.toAscList beyond
          where
            (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. anftMap)

-- |Get the non finalized chain updates.
-- This returns a map from update sequence numbers to the
-- the corresponding chain updates groups.
-- The chain update groups are ordered by increasing
-- sequence number.
-- Note. that there can be gaps in the sequence numbers
-- as a transaction can be received via a pending block.
getNonFinalizedChainUpdates ::
    -- |The 'UpdateType' to retrieve.
    UpdateType ->
    -- |The starting sequence number.
    UpdateSequenceNumber ->
    SkovData pv ->
    -- |The resulting list of chain updates.
    [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]
getNonFinalizedChainUpdates uType updateSequenceNumber sd = do
    case sd ^. transactionTable . ttNonFinalizedChainUpdates . at' uType of
        Nothing -> []
        Just nfcus -> case atsn of
            Nothing -> Map.toAscList beyond
            Just s -> (updateSequenceNumber, s) : Map.toAscList beyond
          where
            (_, atsn, beyond) = Map.splitLookup updateSequenceNumber (nfcus ^. nfcuMap)

-- |Get a non finalized credential by its 'TransactionHash'
-- This returns 'Nothing' in the case that the credential has already been finalized.
getNonFinalizedCredential ::
    -- |'TransactionHash' for the transaction that contained the 'CredentialDeployment'.
    TransactionHash ->
    -- |The 'SkovData pv' to query the non finalized credential from.
    SkovData pv ->
    Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult)
getNonFinalizedCredential txhash sd = do
    case sd ^? transactionTable . ttHashMap . ix txhash of
        Just (WithMetadata{wmdData = CredentialDeployment{..}, ..}, status) ->
            case status of
                Received _ verRes -> Just (WithMetadata{wmdData = biCred, ..}, verRes)
                Committed _ verRes _ -> Just (WithMetadata{wmdData = biCred, ..}, verRes)
        _ -> Nothing

-- |Get the next account nonce for an account.
-- Returns a tuple consisting of the successor of the
-- current account nonce and a boolean value indicating
-- that there are no pending or committed (but only finalized) transactions
-- tied to this account.
getNextAccountNonce ::
    -- |The 'AccountAddressEq' to get the next available nonce for.
    -- This will work for account aliases as this is an 'AccountAddressEq'
    -- and not just a 'AccountAddress'.
    AccountAddressEq ->
    -- |The 'SkovData pv' to query the next account nonce from.
    SkovData pv ->
    -- |The resulting account nonce and whether it is finalized or not.
    (Nonce, Bool)
getNextAccountNonce addr sd = case sd ^. transactionTable . ttNonFinalizedTransactions . at' addr of
    Nothing -> (minNonce, True)
    Just anfts ->
        case Map.lookupMax (anfts ^. anftMap) of
            Nothing -> (anfts ^. anftNextNonce, True)
            Just (nonce, _) -> (nonce + 1, False)

-- |Removes a list of transactions from memory.
-- Per account, the transactions must be in
-- continuous sequence by nonce, starting from the next available non-finalized
-- nonce. This does not write the transactions to the low-level tree state database, but just
-- updates the in-memory transaction table accordingly.
removeTransactions ::
    (MonadState (SkovData pv) m, MonadThrow m) =>
    -- |The transactions to remove from the state.
    [BlockItem] ->
    m ()
removeTransactions = mapM_ removeTrans
  where
    removeTrans WithMetadata{wmdData = NormalTransaction tr, ..} = do
        let nonce = transactionNonce tr
            sender = accountAddressEmbed (transactionSender tr)
        anft <- use (transactionTable . ttNonFinalizedTransactions . at' sender . non emptyANFT)
        unless (anft ^. anftNextNonce == nonce) $
            throwM . TreeStateInvariantViolation $
                "The recorded next nonce for the account "
                    ++ show sender
                    ++ " ("
                    ++ show (anft ^. anftNextNonce)
                    ++ ") doesn't match the one that is going to be finalized ("
                    ++ show nonce
                    ++ ")"
        let nfn = anft ^. anftMap . at' nonce . non Map.empty
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
            \deadTransaction -> transactionTable . ttHashMap . at' (getHash deadTransaction) .= Nothing
        -- Update the non-finalized transactions for the sender
        transactionTable
            . ttNonFinalizedTransactions
            . at' sender
            ?=! ( anft
                    & (anftMap . at' nonce .~ Nothing)
                    & (anftNextNonce .~ nonce + 1)
                )
    removeTrans WithMetadata{wmdData = CredentialDeployment{}, ..} =
        transactionTable . ttHashMap . at' wmdHash .= Nothing
    removeTrans WithMetadata{wmdData = ChainUpdate cu, ..} = do
        let sn = updateSeqNumber (uiHeader cu)
            uty = updateType (uiPayload cu)
        nfcu <- use (transactionTable . ttNonFinalizedChainUpdates . at' uty . non emptyNFCU)
        unless (nfcu ^. nfcuNextSequenceNumber == sn) $
            throwM . TreeStateInvariantViolation $
                "The recorded next sequence number for update type "
                    ++ show uty
                    ++ " ("
                    ++ show (nfcu ^. nfcuNextSequenceNumber)
                    ++ ") doesn't match the one that is going to be finalized ("
                    ++ show sn
                    ++ ")"
        let nfsn = nfcu ^. nfcuMap . at' sn . non Map.empty
            wmdcu = WithMetadata{wmdData = cu, ..}
        unless (Map.member wmdcu nfsn) $
            throwM . TreeStateInvariantViolation $
                "Tried to finalize a chain update that is not known to be in the set of \
                \non-finalized chain updates of type "
                    ++ show uty
        -- Remove the transaction from the in-memory table, together with
        -- any other updates with the same sequence number, since they weren't finalized
        forM_ (Map.keys nfsn) $
            \deadUpdate -> transactionTable . ttHashMap . at' (getHash deadUpdate) .= Nothing
        -- Update the non-finalized chain updates
        transactionTable
            . ttNonFinalizedChainUpdates
            . at' uty
            ?=! (nfcu & (nfcuMap . at' sn .~ Nothing) & (nfcuNextSequenceNumber .~ sn + 1))

-- |Mark a live transaction as committed in a particular block.
-- This does nothing if the transaction is not live.
-- A committed transaction cannot be purged while it is committed for a round after the round of
-- the last finalized block.
commitTransaction ::
    (MonadState (SkovData pv) m) =>
    -- |Round of the block
    Round ->
    -- |The 'BlockHash' that the transaction should
    -- be committed to.
    BlockHash ->
    -- |The 'TransactionIndex' in the block.
    TransactionIndex ->
    -- |The transaction to commit.
    BlockItem ->
    m ()
commitTransaction rnd bh ti transaction =
    transactionTable
        . ttHashMap
        . at' (getHash transaction)
        . traversed
        . _2
        %=! addResult bh rnd ti

-- |Add a transaction to the transaction table if its nonce/sequence number is at least the next
-- non-finalized nonce/sequence number. The return value is 'True' if and only if the transaction
-- was added.
-- When adding a transaction from a block, use the 'Round' of the block. Otherwise use round @0@.
-- The transaction must not already be present.
putTransaction :: (MonadState (SkovData pv) m) => Round -> BlockItem -> VerificationResult -> m Bool
putTransaction rnd transaction verRes = do
    added <- transactionTable %%=! addTransaction transaction (commitPoint rnd) verRes
    when added $ transactionTablePurgeCounter += 1
    return added

-- |Mark the provided transaction as dead for the provided 'BlockHash'.
markTransactionDead ::
    (MonadState (SkovData pv) m) =>
    -- |The 'BlockHash' where the transaction was committed.
    BlockHash ->
    -- |The 'BlockItem' to mark as dead.
    BlockItem ->
    m ()
markTransactionDead blockHash transaction =
    transactionTable
        . ttHashMap
        . at' (getHash transaction)
        . mapped
        . _2
        %= markDeadResult blockHash

-- |Purge transactions from the transaction table and pending transactions.
-- Transactions are purged only if they are not included in a live block, and
-- have either expired or arrived longer ago than the transaction keep alive time.
--
-- If the first argument is @False@, the transaction table is only purged if
-- 'rpInsertionsBeforeTransactionPurged' transactions have been inserted since
-- the last purge.  If it is true, the table is purged regardless.
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
--   * The pending transaction table only references transactions that are in the
--     transaction table.  That is, the high nonce in a range is a tight bound and
--     the deploy credential hashes correspond to transactions in the table.
--
--   * No non-finalized block is considered live or will become live if its round
--     is less than or equal to the slot number of the last finalized block.
--
--   * If a transaction is known to be in any block that is not finalized or dead,
--     then 'commitTransaction' or 'addCommitTransaction' has been called with a
--     slot number at least as high as the slot number of the block.
purgeTransactionTable ::
    (MonadState (SkovData pv) m) =>
    -- |Whether to force the purging.
    Bool ->
    -- |The current time.
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
            (newTT, newPT) = Purge.purgeTables (commitPoint lastFinalizedRound) oldestArrivalTime currentTimestamp transactionTable' pendingTransactions'
        transactionTable .=! newTT
        pendingTransactionTable .=! newPT

-- |Clear pending and non-finalized blocks from the tree state.
-- Transactions that were committed (to any non-finalized block) have their status changed to
-- received.
clearOnProtocolUpdate :: (MonadState (SkovData pv) m) => m ()
clearOnProtocolUpdate = do
    -- clear the pending block table
    pendingBlocksTable .=! HM.empty
    pendingBlocksQueue .=! MPQ.empty
    -- clear the block table
    blockTable .=! emptyBlockTable
    -- clear the branches
    branches .=! Seq.empty
    -- mark committed transactions as received, since we have dropped any blocks
    -- that they belong to.
    transactionTable
        . ttHashMap
        %=! HM.map
            ( \case
                (bi, Committed{..}) -> (bi, Received{..})
                s -> s
            )

-- |Clear the transaction table and pending transactions, ensure that the block states are archived,
-- and collapse the block state caches.
clearAfterProtocolUpdate :: (MonadState (SkovData pv) m, BlockStateStorage m, GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv) => m ()
clearAfterProtocolUpdate = do
    -- Clear the transaction table and pending transactions.
    transactionTable .=! emptyTransactionTable
    pendingTransactionTable .=! emptyPendingTransactionTable
    -- Set the focus block to the last finalized block.
    lastFinBlock <- use lastFinalized
    focusBlock .=! lastFinBlock
    -- Archive the last finalized block state.
    archiveBlockState $ bpState lastFinBlock
    collapseCaches
