{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.TreeState.Implementation where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.Kind (Type)
import Data.Time
import Lens.Micro.Platform

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Sequence as Seq

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Updates

import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.BlobStore as BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState (DeadCache, emptyDeadCache, insertDeadCache, memberDeadCache)
import qualified Concordium.GlobalState.PurgeTransactions as Purge
import qualified Concordium.GlobalState.Statistics as Stats
import Concordium.GlobalState.TransactionTable
import qualified Concordium.GlobalState.Types as GSTypes

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Statistics (ConsensusStatistics)
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TransactionVerification
import qualified Concordium.TransactionVerification as TVer
import Concordium.Utils
import qualified Data.List as List

data PendingBlock = PendingBlock
    { pbBlock :: !SignedBlock,
      pbReceiveTime :: !UTCTime
    }
    deriving (Eq)

instance HashableTo BlockHash PendingBlock where
    getHash PendingBlock{..} = getHash pbBlock

instance BlockData PendingBlock where
    type BakedBlockDataType PendingBlock = SignedBlock
    blockRound = blockRound . pbBlock
    blockEpoch = blockEpoch . pbBlock
    blockTimestamp = blockTimestamp . pbBlock
    blockBakedData = blockBakedData . pbBlock
    blockTransactions = blockTransactions . pbBlock
    blockStateHash = blockStateHash . pbBlock

instance BakedBlockData PendingBlock where
    blockQuorumCertificate = blockQuorumCertificate . pbBlock
    blockParent = blockParent . pbBlock
    blockBaker = blockBaker . pbBlock
    blockBakerKey = blockBakerKey . pbBlock
    blockTimeoutCertificate = blockTimeoutCertificate . pbBlock
    blockEpochFinalizationEntry = blockEpochFinalizationEntry . pbBlock
    blockNonce = blockNonce . pbBlock
    blockSignature = blockSignature . pbBlock

-- |Status of a block that is held in memory i.e.
-- the block is either pending or alive.
-- Parameterized by the 'BlockPointer' and the 'SignedBlock'.
data InMemoryBlockStatus pv
    = -- |The block is awaiting its parent to become part of chain.
      MemBlockPending !SignedBlock
    | -- |The block is alive i.e. head of chain.
      MemBlockAlive !(BlockPointer pv)

-- |The block table yields blocks that are
-- either alive or pending.
-- Furthermore it holds a fixed size cache of hashes
-- blocks marked as dead.
data BlockTable pv = BlockTable
    { _deadBlocks :: !DeadCache,
      _liveMap :: HM.HashMap BlockHash (InMemoryBlockStatus pv)
    }

makeLenses ''BlockTable

-- |Create the empty block table.
emptyBlockTable :: BlockTable pv
emptyBlockTable = BlockTable emptyDeadCache HM.empty

-- |Data required to support 'TreeState'.
data SkovData (pv :: ProtocolVersion) = SkovData
    { _roundStatus :: !RoundStatus,
      -- |Transactions.
      _transactionTable :: !TransactionTable,
      -- |The purge counter for the 'TransactionTable'
      _transactionTablePurgeCounter :: !Int,
      -- |Table of pending transactions i.e. transactions that has not yet become part
      -- of a block.
      _pendingTransactions :: !PendingTransactionTable,
      -- |The current focus block.
      -- The focus block is the block with the largest height included in the tree.
      _focusBlock :: !(BlockPointer pv),
      -- |Runtime parameters.
      _runtimeParameters :: !RuntimeParameters,
      -- |Blocks which have been included in the tree or marked as dead.
      _blockTable :: !(BlockTable pv),
      -- |Branches of the tree by height above the last finalized block
      _branches :: !(Seq.Seq [BlockPointer pv]),
      -- |Pending blocks i.e. blocks that have not yet been included in the tree.
      -- The entries of the pending blocks are keyed by the 'BlockHash' of their parent block.
      _pendingBlocksTable :: !(HM.HashMap BlockHash [PendingBlock]),
      -- |A priority search queue on the (pending block hash, parent of pending block hash) tuple,
      -- prioritised by the round of the pending block. The queue in particular supports extracting
      -- the pending block with minimal 'Round'. Note that the  queue can contain spurious pending
      -- blocks, and a block is only actually in the pending blocks if it has an entry in the
      -- '_pendingBlocksTable'.
      _pendingBlocksQueue :: !(MPQ.MinPQueue Round (BlockHash, BlockHash)),
      -- |Pointer to the last finalized block.
      _lastFinalized :: !(BlockPointer pv),
      -- |The current consensus statistics.
      _statistics :: !Stats.ConsensusStatistics
    }

makeLenses ''SkovData

-- |Create an initial 'SkovData pv'
mkInitialSkovData ::
    -- |The 'RuntimeParameters'
    RuntimeParameters ->
    -- |Genesis configuration
    GenesisConfiguration ->
    -- |Genesis state
    PBS.HashedPersistentBlockState pv ->
    -- |The base timeout
    Duration ->
    -- |The 'LeadershipElectionNonce'
    LeadershipElectionNonce ->
    -- |The initial 'SkovData'
    SkovData pv
mkInitialSkovData rp genConf genState baseTimeout len =
    let genesisBlock = GenesisBlock{gbConfiguration = genConf, gbStateHash = getHash genState}
        genesisTime = timestampToUTCTime $ gdGenesisTime genConf
        genesisMetadata =
            BlockMetadata
                { bmHeight = 0,
                  bmReceiveTime = genesisTime,
                  bmArriveTime = genesisTime
                }
        genesisBlockPointer =
            BlockPointer
                { _bpInfo = genesisMetadata,
                  _bpBlock = genesisBlock,
                  _bpState = genState
                }
        _roundStatus = initialRoundStatus baseTimeout len
        _transactionTable = emptyTransactionTable
        _transactionTablePurgeCounter = 0
        _pendingTransactions = emptyPendingTransactionTable
        _focusBlock = genesisBlockPointer
        _runtimeParameters = rp
        _blockTable = emptyBlockTable
        _branches = Seq.empty
        _pendingBlocksTable = HM.empty
        _pendingBlocksQueue = MPQ.empty
        _lastFinalized = genesisBlockPointer
        _statistics = Stats.initialConsensusStatistics
    in  SkovData{..}

-- |A 'SkovData pv' wrapped in an 'IORef', where @pv@ is the
-- current 'ProtocolVersion'
newtype SkovState (pv :: ProtocolVersion) = SkovState (IORef (SkovData pv))

-- |Create the 'HasSkovState' @HasSkovState pv@ constraint.
makeClassy ''SkovState

-- |'TreeStateWrapper' for implementing 'MonadTreeState'.
newtype TreeStateWrapper (pv :: ProtocolVersion) (m :: Type -> Type) (a :: Type) = TreeStateWrapper {runTreeStateWrapper :: m a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, GSTypes.BlockStateTypes)

-- |There must be an instance for 'LowLevel.MonadTreeStateStore m' in the transformer stack somewhere.
deriving instance (LowLevel.MonadTreeStateStore m, MPV m ~ pv) => LowLevel.MonadTreeStateStore (TreeStateWrapper pv m)

-- |'MonadReader' instance for 'TreeStateWrapper'.
deriving instance MonadReader r m => MonadReader r (TreeStateWrapper pv m)

-- |'MonadProtocolVersion' instance for 'TreeStateWrapper'.
instance IsProtocolVersion pv => MonadProtocolVersion (TreeStateWrapper pv m) where
    type MPV (TreeStateWrapper pv m) = pv

-- |Helper function for retrieving the SkovData in the 'TreeStateWrapper pv m' context.
-- This should be used when only reading from the 'SkovData' is required.
-- If one wishes to also update the 'SkovData' then use 'withSkovData'.
-- Note that this requires IO since the actual 'SkovData' is behind an 'IORef' via the 'SkovState pv'
getSkovData :: (MonadIO m, MonadReader r m, r ~ SkovState pv) => TreeStateWrapper pv m (SkovData pv)
getSkovData = do
    (SkovState ioref) <- ask
    liftIO $ readIORef ioref

-- |Helper function for updating the 'SkovData' behind the 'IORef' in the 'SkovState pv'.
-- This should only be used when it is required to update the 'SkovData' otherwise use 'getSkovData'.
-- Note that this requires IO since the actual 'SkovData' is behind an 'IORef' via the 'SkovState pv'.
-- withSkovData :: (MonadIO m, MonadReader r m, r ~ SkovState pv) => (SkovData pv -> SkovData pv) -> m ()
withSkovData :: (MonadIO m, MonadReader r m, HasSkovState r pv) => StateT (SkovData pv) m a -> m a
withSkovData f = do
    (SkovState ioref) <- view skovState
    sd <- liftIO $ readIORef ioref
    (res, sd') <- runStateT f sd
    liftIO $ writeIORef ioref $! sd'
    return res

withSkovDataPure :: (MonadIO m, MonadReader r m, HasSkovState r pv) => State (SkovData pv) a -> m a
withSkovDataPure f = do
    (SkovState ioref) <- view skovState
    sd <- liftIO $ readIORef ioref
    let (res, sd') = runState f sd
    liftIO $ writeIORef ioref $! sd'
    return res

-- * Operations on pending blocks

-- |Add a block to the pending block table and queue.
doAddPendingBlock :: (MonadState (SkovData pv) m) => PendingBlock -> m ()
{-# SPECIALIZE doAddPendingBlock :: PendingBlock -> State (SkovData pv) () #-}
doAddPendingBlock sb = do
    pendingBlocksQueue %= MPQ.insert theRound (blockHash, parentHash)
    pendingBlocksTable %= HM.adjust (sb :) parentHash
  where
    blockHash = getHash sb
    theRound = blockRound sb
    parentHash = blockParent sb

-- |Take the set of blocks that are pending a particular parent from the pending block table.
-- Note: this does not remove them from the pending blocks queue; blocks should be removed from
-- the queue as the finalized round progresses.
doTakePendingChildren :: (MonadState (SkovData pv) m) => BlockHash -> m [PendingBlock]
doTakePendingChildren parent = pendingBlocksTable . at' parent . non [] <<.= []

-- |Return the next block that is pending its parent with round number
-- less than or equal to the given value, removing it from the pending
-- table.  Returns 'Nothing' if there is no such pending block.
doTakeNextPendingUntil :: (MonadState (SkovData pv) m) => Round -> m (Maybe PendingBlock)
doTakeNextPendingUntil targetRound = tnpu =<< use pendingBlocksQueue
  where
    tnpu ppq = case MPQ.minViewWithKey ppq of
        Just ((r, (pending, parent)), ppq')
            | r <= targetRound -> do
                (myPB, otherPBs) <-
                    List.partition ((== pending) . getHash)
                        <$> use (pendingBlocksTable . at' parent . non [])
                case myPB of
                    [] -> tnpu ppq' -- Block is no longer actually pending
                    (realPB : _) -> do
                        pendingBlocksTable . at' parent . non [] .= otherPBs
                        pendingBlocksQueue .= ppq'
                        return (Just realPB)
        _ -> do
            pendingBlocksQueue .= ppq
            return Nothing

-- |Lookup a transaction in the transaction table if it is live.
-- This will not give results for finalized transactions.
doLookupLiveTransaction :: TransactionHash -> SkovData pv -> Maybe LiveTransactionStatus
doLookupLiveTransaction tHash sd =
    sd ^? transactionTable . ttHashMap . at tHash . traversed . _2

-- |Lookup a transaction in the transaction table, including finalized transactions.
doLookupTransaction :: (LowLevel.MonadTreeStateStore m) => TransactionHash -> SkovData pv -> m (Maybe TransactionStatus)
doLookupTransaction tHash sd = case doLookupLiveTransaction tHash sd of
    Just liveRes -> return $ Just $ Live liveRes
    Nothing -> fmap Finalized <$> LowLevel.lookupTransaction tHash

-- |Mark a live transaction as committed in a particular block.
-- This does nothing if the transaction is not live.
doCommitTransaction ::
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
doCommitTransaction rnd bh ti transaction =
    transactionTable
        . ttHashMap
        . at' (getHash transaction)
        . traversed
        . _2
        %= addResult bh rnd ti

-- |Add a transaction to the transaction table if its nonce/sequence number is at least the next
-- non-finalized nonce/sequence number. The return value is 'True' if and only if the transaction
-- was added.
doAddTransaction :: (MonadState (SkovData pv) m) => Round -> BlockItem -> VerificationResult -> m Bool
doAddTransaction rnd transaction verRes = do
    added <- transactionTable %%=! addTransaction transaction (commitPoint rnd) verRes
    when added $ transactionTablePurgeCounter += 1
    return added

-- |Get the 'PendingTransactionTable'.
doGetPendingTransactions :: (MonadState (SkovData pv) m) => m PendingTransactionTable
doGetPendingTransactions = use pendingTransactions

-- |Put the 'PendingTransactionTable'.
doPutPendingTransactions :: (MonadState (SkovData pv) m) => PendingTransactionTable -> m ()
doPutPendingTransactions pts = pendingTransactions .= pts

-- |Turn a 'PendingBlock' into a live block.
-- This marks the block as 'MemBlockAlive' in the block table
-- and updates the arrive time of the block.
-- and returns the resulting 'BlockPointer'.
doMakeLiveBlock :: (MonadState (SkovData pv) m) => PendingBlock -> PBS.HashedPersistentBlockState pv -> BlockHeight -> UTCTime -> m (BlockPointer pv)
doMakeLiveBlock pb st height arriveTime = do
    let bp =
            BlockPointer
                { _bpInfo = BlockMetadata{bmReceiveTime = pbReceiveTime pb, bmArriveTime = arriveTime, bmHeight = height},
                  _bpBlock = NormalBlock (pbBlock pb),
                  _bpState = st
                }
    blockTable . liveMap . at' (getHash pb) ?=! MemBlockAlive bp
    return bp

-- |Marks a block as dead.
-- This expunges the block from memory
-- and registers the block in the dead cache.
doMarkBlockDead :: (MonadState (SkovData pv) m) => BlockHash -> m ()
doMarkBlockDead blockHash = do
    blockTable . liveMap . at' blockHash .=! Nothing
    blockTable . deadBlocks %=! insertDeadCache blockHash
    return ()

-- |Mark the provided transaction as dead for the provided 'BlockHash'.
doMarkTransactionDead ::
    (MonadState (SkovData pv) m) =>
    -- |The 'BlockHash' where the transaction was committed.
    BlockHash ->
    -- |The 'BlockItem' to mark as dead.
    BlockItem ->
    m ()
doMarkTransactionDead blockHash transaction = transactionTable . ttHashMap . at' (getHash transaction) . mapped . _2 %= markDeadResult blockHash

-- |Get a 'BlockPointer' to the last finalized block.
doGetLastFinalized :: (MonadState (SkovData pv) m) => m (BlockPointer pv)
doGetLastFinalized = use lastFinalized

-- |Get the 'BlockStatus' of a non finalized block based on the 'BlockHash'
-- If the block could not be found in memory then this will return 'Nothing'
-- otherwise 'Just BlockStatus'.
-- This function should not be called directly, instead use either
-- 'doGetBlockStatus' or 'doGetRecentBlockStatus'.
doGetNonFinalizedBlockStatus :: (MonadState (SkovData (MPV m)) m) => BlockHash -> m (Maybe (BlockStatus (MPV m)))
doGetNonFinalizedBlockStatus blockHash = do
    SkovData{..} <- get
    -- We check whether the block is the last finalized block.
    if getHash _lastFinalized == blockHash
        then return $! Just $! BlockFinalized _lastFinalized
        else -- We check if the block is the focus block

            if getHash _focusBlock == blockHash
                then return $! Just $! BlockAlive _focusBlock
                else -- Now we lookup in the livemap of the block table,
                -- this will return if the block is pending or if it's alive on some other
                -- branch.
                -- If it is not present in the live store then we check whether the block
                -- has been marked as dead and finally we try to look in the persistent block store.
                case HM.lookup blockHash (_liveMap _blockTable) of
                    Just status ->
                        case status of
                            MemBlockPending sb -> return $! Just $! BlockPending sb
                            MemBlockAlive bp -> return $! Just $! BlockAlive bp
                    Nothing ->
                        -- Check if the block marked as dead before we look into the
                        -- persistent block store.
                        if memberDeadCache blockHash (_deadBlocks _blockTable)
                            then return $ Just BlockDead
                            else return Nothing

-- |Get the 'BlockStatus' of a block based on the provided 'BlockHash'.
-- Note. if one does not care about old finalized blocks then
-- use 'doGetRecentBlockStatus' instead as it circumvents a full lookup from disk.
doGetBlockStatus :: (LowLevel.MonadTreeStateStore m, MonadIO m) => (MonadState (SkovData (MPV m)) m) => BlockHash -> m (BlockStatus (MPV m))
doGetBlockStatus blockHash =
    doGetNonFinalizedBlockStatus blockHash >>= \case
        Just bs -> return bs
        Nothing ->
            LowLevel.lookupBlock blockHash >>= \case
                Nothing -> return BlockUnknown
                Just storedBlock -> do
                    blockPointer <- liftIO $ mkBlockPointer storedBlock
                    return $! BlockFinalized blockPointer
  where
    -- Create a block pointer from a stored block.
    mkBlockPointer sb@LowLevel.StoredBlock{..} = do
        _bpState <- mkHashedPersistentBlockState sb
        return BlockPointer{_bpInfo = stbInfo, _bpBlock = stbBlock, ..}
    mkHashedPersistentBlockState sb@LowLevel.StoredBlock{..} = do
        hpbsPointers <- newIORef $ BlobStore.blobRefToBufferedRef stbStatePointer
        let hpbsHash = blockStateHash sb
        return $! PBS.HashedPersistentBlockState{..}

-- |Get the 'RecentBlockStatus' of a block based on the provided 'BlockHash'.
-- One should use this instead of 'getBlockStatus' if
-- one does not require the actual contents and resulting state related
-- to the block in case the block is a predecessor of the last finalized block.
doGetRecentBlockStatus :: (LowLevel.MonadTreeStateStore m) => (MonadState (SkovData (MPV m)) m) => BlockHash -> m (RecentBlockStatus (MPV m))
doGetRecentBlockStatus blockHash =
    doGetNonFinalizedBlockStatus blockHash >>= \case
        Just bs -> return $! RecentBlock bs
        Nothing -> do
            LowLevel.memberBlock blockHash >>= \case
                True -> return OldFinalized
                False -> return Unknown

-- |Get the focus block.
-- This is probably the best block, but if
-- we're pruning a branch this will become the parent block
doGetFocusBlock :: (MonadState (SkovData pv) m) => m (BlockPointer pv)
doGetFocusBlock = use focusBlock

-- |Update the focus block
doPutFocusBlock :: (MonadState (SkovData pv) m) => BlockPointer pv -> m ()
doPutFocusBlock fb = focusBlock .= fb

-- |Get the current 'RoundStatus'
doGetRoundStatus :: (MonadState (SkovData pv) m) => m RoundStatus
doGetRoundStatus = use roundStatus

-- |Put the current 'RoundStatus'
doPutRoundStatus :: (MonadState (SkovData pv) m) => RoundStatus -> m ()
doPutRoundStatus rs = roundStatus .= rs

-- |Get the 'RuntimeParameters'
doGetRuntimeParameters :: (MonadState (SkovData pv) m) => m RuntimeParameters
doGetRuntimeParameters = use runtimeParameters

-- |Get consensus statistics.
doGetConsensusStatistics :: (MonadState (SkovData pv) m) => m ConsensusStatistics
doGetConsensusStatistics = use statistics

-- |Get the non finalized chain updates.
-- This returns a map from update sequence numbers to the
-- the corresponding chain updates groups.
-- The chain update groups are ordered by increasing
-- sequence number.
doGetNonfinalizedChainUpdates ::
    (MonadState (SkovData pv) m) =>
    -- |The 'UpdateType' to retrieve.
    UpdateType ->
    -- |The starting sequence number.
    UpdateSequenceNumber ->
    -- |The resulting list of
    m [(UpdateSequenceNumber, Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)]
doGetNonfinalizedChainUpdates uType updateSequenceNumber = do
    use (transactionTable . ttNonFinalizedChainUpdates . at' uType) >>= \case
        Nothing -> return []
        Just nfcus ->
            let (_, atsn, beyond) = Map.splitLookup updateSequenceNumber (nfcus ^. nfcuMap)
            in  return $ case atsn of
                    Nothing -> Map.toAscList beyond
                    Just s ->
                        let first = (updateSequenceNumber, s)
                            rest = Map.toAscList beyond
                        in  first : rest

-- |Get a non finalized credential by its 'TransactionHash'
-- This returns 'Nothing' in the case that the credential has already been finalized.
doGetNonFinalizedCredential ::
    (MonadState (SkovData pv) m) =>
    -- |'TransactionHash' for the transaction that contained the
    -- 'CredentialDeployment'.
    TransactionHash ->
    m (Maybe (CredentialDeploymentWithMeta, TVer.VerificationResult))
doGetNonFinalizedCredential txhash = do
    preuse (transactionTable . ttHashMap . ix txhash) >>= \case
        Just (WithMetadata{wmdData = CredentialDeployment{..}, ..}, status) ->
            case status of
                Received _ verRes -> return $ Just (WithMetadata{wmdData = biCred, ..}, verRes)
                Committed _ verRes _ -> return $ Just (WithMetadata{wmdData = biCred, ..}, verRes)
        _ -> return Nothing

-- |Purge transactions from the transaction table and pending transactions.
-- Transactions are purged only if they are not included in a live block, and
-- have either expired or arrived longer ago than the transaction keep alive time.
--
-- If the first argument is @False@, the transaction table is only purged if
-- 'rpInsertionsBeforeTransactionPurged' transactions have been inserted since
-- the last purge.  If it is true, the table is purged regardless.
--
-- WARNING: This function violates the independence of the tree state components.
-- In particular, the following invariants are assumed and maintained:
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
--   * The finalization list must reflect the current last finalized block.
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
doPurgeTransactionTable ::
    (MonadState (SkovData pv) m) =>
    -- |Whether to force the purging.
    Bool ->
    -- |The current time.
    UTCTime ->
    m ()
doPurgeTransactionTable force currentTime = do
    purgeCount <- use transactionTablePurgeCounter
    RuntimeParameters{..} <- use runtimeParameters
    when (force || purgeCount > rpInsertionsBeforeTransactionPurge) $ do
        transactionTablePurgeCounter .= 0
        lfb <- use lastFinalized
        let lastFinalizedRound = blockRound $! _bpBlock lfb
        transactionTable' <- use transactionTable
        pendingTransactions' <- use pendingTransactions
        let
            currentTransactionTime = utcTimeToTransactionTime currentTime
            oldestArrivalTime =
                if currentTransactionTime > rpTransactionsKeepAliveTime
                    then currentTransactionTime - rpTransactionsKeepAliveTime
                    else 0
            currentTimestamp = utcTimeToTimestamp currentTime
            (newTT, newPT) = Purge.purgeTables (commitPoint lastFinalizedRound) oldestArrivalTime currentTimestamp transactionTable' pendingTransactions'
        transactionTable .= newTT
        pendingTransactions .= newPT

-- |Get the next account nonce for an account.
-- Returns a tuple consisting of the successor of the
-- current account nonce and a boolean value indicating
-- that there are no pending or committed (but only finalized) transactions
-- tied to this account.
doGetNextAccountNonce ::
    (MonadState (SkovData pv) m) =>
    -- |The 'AccountAddressEq' to get the next available nonce for.
    -- This will work for account aliases as this is an 'AccountAddressEq'
    -- and not just a 'AccountAddress'.
    AccountAddressEq ->
    -- |The resulting account nonce and whether it is finalized or not.
    m (Nonce, Bool)
doGetNextAccountNonce addr =
    use (transactionTable . ttNonFinalizedTransactions . at' addr) >>= \case
        Nothing -> return (minNonce, True)
        Just anfts ->
            case Map.lookupMax (anfts ^. anftMap) of
                Nothing -> return (anfts ^. anftNextNonce, True)
                Just (nonce, _) -> return (nonce + 1, False)

-- |Clear pending and non-finalized blocks from the tree state.
-- Transactions that were committed (to any non-finalized block) have their status changed to
-- received.
doClearOnProtocolUpdate :: (MonadState (SkovData pv) m) => m ()
doClearOnProtocolUpdate = do
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
doClearAfterProtocolUpdate :: (MonadState (SkovData pv) m, BlockStateStorage m, GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv) => m ()
doClearAfterProtocolUpdate = do
    -- Clear the transaction table and pending transactions.
    transactionTable .=! emptyTransactionTable
    pendingTransactions .=! emptyPendingTransactionTable
    -- Set the focus block to the last finalized block.
    lastFinBlock <- use lastFinalized
    focusBlock .=! lastFinBlock
    -- Archive the last finalized block state.
    archiveBlockState $ _bpState lastFinBlock
    collapseCaches
