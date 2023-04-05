{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Blocks where

import Control.Monad.Reader
import Control.Monad.State
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Types.Transactions

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus (HasBakerContext, MonadMulticast (sendQuorumMessage), MonadTimeout, advanceRound)
import qualified Concordium.KonsensusV1.Consensus as Consensus
import Concordium.KonsensusV1.Consensus.Finality
import Concordium.KonsensusV1.Consensus.Quorum
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.Scheduler
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimerMonad
import Concordium.Types.BakerIdentity
import Control.Monad.Catch
import Data.Function
import Data.Ord

data BlockTraceLogEvent
    = LogBlockOld BlockHash
    | LogBlockDuplicate BlockHash
    | LogBlockInvalidParent BlockHash BlockHash
    | LogBlockInvalidEpoch BlockHash Epoch
    | LogBlockInvalidSignature BlockHash
    | LogBlockIncorrectBaker BlockHash BakerId BakerId
    | LogBlockQCRoundInconsistent BlockHash
    | LogBlockQCEpochInconsistent BlockHash
    | LogBlockInvalidQC BlockHash
    | LogBlockRoundInconsistent BlockHash
    | LogBlockEpochInconsistent BlockHash
    | LogBlockNonceIncorrect BlockHash
    | LogBlockTooFast BlockHash Timestamp Duration Timestamp
    | LogBlockTCMissing BlockHash
    | LogBlockTCRoundInconsistent BlockHash
    | LogBlockQCInconsistentWithTC BlockHash
    | LogBlockInvalidTC BlockHash
    | LogBlockUnexpectedTC BlockHash
    | LogBlockEpochFinalizationMissing BlockHash
    | LogBlockUnrelatedEpochFinalization BlockHash BlockHash
    | LogBlockInvalidEpochFinalizationTarget BlockHash BlockHash
    | LogBlockInvalidEpochFinalization BlockHash
    | LogBlockUnexpectedEpochFinalization BlockHash
    | LogBlockExecutionFailure BlockHash FailureReason
    | LogBlockInvalidTransactionOutcomesHash BlockHash TransactionOutcomesHash TransactionOutcomesHash
    | LogBlockInvalidStateHash BlockHash StateHash StateHash

instance Loggable BlockTraceLogEvent where
    loggableSource _ = Konsensus
    loggableLevel _ = LLTrace
    loggableMessage (LogBlockOld bh) = "Block " ++ show bh ++ " is from an old round or epoch."
    loggableMessage (LogBlockDuplicate bh) = "Block " ++ show bh ++ " is a duplicate."
    loggableMessage (LogBlockInvalidParent bh parent) =
        "Block "
            ++ show bh
            ++ " has an invalid (dead or old finalized) parent ("
            ++ show parent
            ++ ")."
    loggableMessage (LogBlockInvalidEpoch bh ep) =
        "Block "
            ++ show bh
            ++ " is for an invalid epoch ("
            ++ show ep
            ++ ")."
    loggableMessage (LogBlockInvalidSignature bh) =
        "Block " ++ show bh ++ " has an invalid signature."
    loggableMessage (LogBlockIncorrectBaker bh actual expected) =
        "Block "
            ++ show bh
            ++ " is from baker "
            ++ show actual
            ++ " but the round winner is baker "
            ++ show expected
            ++ "."
    loggableMessage (LogBlockQCRoundInconsistent bh) =
        "Block " ++ show bh ++ " QC round does not match parent block's round."
    loggableMessage (LogBlockQCEpochInconsistent bh) =
        "Block " ++ show bh ++ " QC epoch does not match parent block's epoch."
    loggableMessage (LogBlockInvalidQC bh) =
        "Block " ++ show bh ++ " contains an invalid QC."
    loggableMessage (LogBlockRoundInconsistent bh) =
        "Block " ++ show bh ++ " round is not higher than parent block's round."
    loggableMessage (LogBlockEpochInconsistent bh) =
        "Block " ++ show bh ++ " epoch is not the same or next epoch relative to parent."
    loggableMessage (LogBlockNonceIncorrect bh) =
        "Block " ++ show bh ++ " nonce is incorrect."
    loggableMessage (LogBlockTooFast bh bTimestamp minBlockTime pTimestamp) =
        "Block "
            ++ show bh
            ++ " timestamp ("
            ++ show bTimestamp
            ++ ") is less than minBlockTime ("
            ++ show minBlockTime
            ++ ") after parent timestamp ("
            ++ show pTimestamp
            ++ ")."
    loggableMessage (LogBlockTCMissing bh) =
        "Block "
            ++ show bh
            ++ " is missing a timeout certificate, but is not in the next round from its parent."
    loggableMessage (LogBlockTCRoundInconsistent bh) =
        "Block " ++ show bh ++ " timeout certificate is not for the correct round."
    loggableMessage (LogBlockQCInconsistentWithTC bh) =
        "Block " ++ show bh ++ " timeout certificate is inconsistent with the quorum certificate."
    loggableMessage (LogBlockInvalidTC bh) =
        "Block " ++ show bh ++ " timeout certificate is invalid."
    loggableMessage (LogBlockUnexpectedTC bh) =
        "Block " ++ show bh ++ " has unexpected timeout certificate."
    loggableMessage (LogBlockEpochFinalizationMissing bh) =
        "Block " ++ show bh ++ " missing an epoch finalization entry."
    loggableMessage (LogBlockUnrelatedEpochFinalization bh target) =
        "Block "
            ++ show bh
            ++ " contains epoch finalization entry for a block ("
            ++ show target
            ++ ") it does not descend from."
    loggableMessage (LogBlockInvalidEpochFinalizationTarget bh target) =
        "Block "
            ++ show bh
            ++ " contains epoch finalization entry for a block ("
            ++ show target
            ++ ") that is not part of the chain."
    loggableMessage (LogBlockInvalidEpochFinalization bh) =
        "Block "
            ++ show bh
            ++ " contains an invalid epoch finalization entry."
    loggableMessage (LogBlockUnexpectedEpochFinalization bh) =
        "Block "
            ++ show bh
            ++ " contains an epoch finalization entry when it should not."
    loggableMessage (LogBlockExecutionFailure bh failureReason) =
        "Block "
            ++ show bh
            ++ " failed execution: "
            ++ show failureReason
    loggableMessage (LogBlockInvalidTransactionOutcomesHash bh stated computed) =
        "Block "
            ++ show bh
            ++ " stated transaction outcome hash ("
            ++ show stated
            ++ ") does not match computed value ("
            ++ show computed
            ++ ")."
    loggableMessage (LogBlockInvalidStateHash bh stated computed) =
        "Block "
            ++ show bh
            ++ " stated state hash ("
            ++ show stated
            ++ ") does not match computed value ("
            ++ show computed
            ++ ")."

data BlockDebugLogEvent
    = LogBlockMultipleSigned BakerId Round

instance Loggable BlockDebugLogEvent where
    loggableSource _ = Konsensus
    loggableLevel _ = LLDebug
    loggableMessage (LogBlockMultipleSigned baker rnd) =
        "Baker " ++ show baker ++ " signed multiple blocks in round " ++ show rnd ++ "."

data BlockInfoLogEvent
    = LogBlockPending BlockHash BlockHash
    | LogBlockArrive BlockHash

instance Loggable BlockInfoLogEvent where
    loggableSource _ = Konsensus
    loggableLevel _ = LLInfo
    loggableMessage (LogBlockPending block parent) =
        "Block " ++ show block ++ " is pending its parent " ++ show parent ++ "."
    loggableMessage (LogBlockArrive block) =
        "Block " ++ show block ++ " arrived."

-- |A block that has passed initial verification, but must still be executed, added to the state,
-- and (potentially) signed as a finalizer.
data VerifiedBlock = VerifiedBlock
    { -- |The block that has passed initial verification.
      vbBlock :: !PendingBlock,
      -- |The bakers and finalizers for the epoch of this block.
      vbBakersAndFinalizers :: !BakersAndFinalizers,
      -- |The baker info for the block's own baker.
      vbBakerInfo :: !FullBakerInfo,
      -- |The leadership election nonce for the block's epoch.
      vbLeadershipElectionNonce :: !LeadershipElectionNonce
    }
    deriving (Eq, Show)

instance BlockData VerifiedBlock where
    type BakedBlockDataType VerifiedBlock = SignedBlock
    blockRound = blockRound . vbBlock
    blockEpoch = blockEpoch . vbBlock
    blockTimestamp = blockTimestamp . vbBlock
    blockBakedData = blockBakedData . vbBlock
    blockTransactions = blockTransactions . vbBlock
    blockTransactionCount = blockTransactionCount . vbBlock
    blockStateHash = blockStateHash . vbBlock

data BlockResult
    = -- |The block was successfully received, but not yet executed.
      BlockResultSuccess !VerifiedBlock
    | -- |The baker also signed another block in the same slot, but the block was otherwise
      -- successfully received, but not yet executed.
      BlockResultDoubleSign !VerifiedBlock
    | -- |The block contains data that is not valid with respect to the chain.
      BlockResultInvalid
    | -- |The block is too old to be added to the chain.
      BlockResultStale
    | -- |The block is pending its parent.
      BlockResultPending
    | -- |The timestamp of this block is too early.
      BlockResultEarly
    | -- |We have already seen this block.
      BlockResultDuplicate
    deriving (Eq, Show)

uponReceivingBlock ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadLogger m
    ) =>
    PendingBlock ->
    m BlockResult
uponReceivingBlock pendingBlock = do
    -- TODO: Check for consensus shutdown
    lfb <- use lastFinalized
    if blockEpoch pendingBlock < blockEpoch lfb || blockRound pendingBlock <= blockRound lfb
        then do
            -- The block is from an old epoch, or already finalized round
            logLoggable $ LogBlockOld pbHash
            return BlockResultStale
        else do
            sd <- get
            -- Check that the block is not already live or pending. The network-layer deduplication
            -- should generally prevent such blocks from getting here, but having this check means
            -- we can rely on the fact.
            case getMemoryBlockStatus pbHash sd of
                Just _ -> do
                    logLoggable $ LogBlockDuplicate pbHash
                    return BlockResultDuplicate
                Nothing -> do
                    getRecentBlockStatus (blockParent pendingBlock) sd >>= \case
                        RecentBlock (BlockAlive parent) -> receiveBlockKnownParent parent pendingBlock
                        RecentBlock (BlockFinalized parent) -> receiveBlockKnownParent parent pendingBlock
                        RecentBlock BlockPending{} -> receiveBlockUnknownParent pendingBlock
                        RecentBlock BlockDead -> rejectBadParent
                        RecentBlock BlockUnknown -> receiveBlockUnknownParent pendingBlock
                        OldFinalized -> rejectBadParent
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock
    rejectBadParent = do
        logLoggable $ LogBlockInvalidParent pbHash (blockParent pendingBlock)
        return BlockResultStale

-- |Process receiving a block where the parent is live (i.e. descended from the last finalized
-- block).  If this function returns 'BlockResultSuccess' then the caller is obliged to call
-- 'executeBlock' on the returned 'VerifiedBlock' in order to complete the processing of the block,
-- after relaying the block to peers.
--
-- The result can be one of the following:
--
-- * @'BlockResultSuccess' vb@: the block has a valid signature and is baked by the correct leader
--   in the block's round. (Note, this implies that the block is in the same or the next epoch from
--   its parent, since we can only determine the leadership election nonce if one of these is the
--   case.) The block is not yet added to the state, except that its existence is recorded in the
--   'roundExistingBlocks' index for checking double signing. The block must be subsequently
--   processed by calling @'executeBlock' vb@
--
-- * @'BlockResultDoubleSign' vb@: the block is valid, but the result of double signing.
--   As with 'BlockResultSuccess', the block is not yet added to the state and must be subsequently
--   processed by calling @'executeBlock' vb@.
--
-- * 'BlockResultInvalid': the epoch is invalid, the signature is invalid, or the baker is not the
--   round leader.
receiveBlockKnownParent ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadLogger m
    ) =>
    BlockPointer (MPV m) ->
    PendingBlock ->
    m BlockResult
receiveBlockKnownParent parent pendingBlock = do
    genesisHash <- use currentGenesisHash
    -- Since the parent block is live, its epoch is either currentEpoch or (currentEpoch - 1).
    -- [This follows from the invariants on what the current epoch can be.]
    -- For the new block's epoch to be valid, it must either be in the same epoch as the parent
    -- block or in the next epoch. Thus, the block's epoch is one of (currentEpoch - 1),
    -- currentEpoch, or (currentEpoch + 1). 'getBakersForLiveEpoch' will return the bakers for the
    -- epoch in all of these cases.
    gets (getBakersForLiveEpoch (blockEpoch pendingBlock)) >>= \case
        Nothing -> do
            logLoggable $ LogBlockInvalidEpoch pbHash (blockEpoch pendingBlock)
            return BlockResultInvalid
        Just bakersAndFinalizers
            -- We know the bakers
            | Just baker <- fullBaker (bakersAndFinalizers ^. bfBakers) (blockBaker pendingBlock),
              verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                -- The signature is valid
                receiveSigned bakersAndFinalizers
            | otherwise -> do
                -- The signature is invalid
                logLoggable $ LogBlockInvalidSignature pbHash
                return BlockResultInvalid
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock
    receiveSigned bakersAndFinalizers
        | blockEpoch pendingBlock == blockEpoch parent = do
            -- Block is in the current epoch, so use the leadership election nonce
            -- from the parent block.
            seedState <- getSeedState (bpState parent)
            checkLeader bakersAndFinalizers (seedState ^. currentLeadershipElectionNonce)
        | blockEpoch pendingBlock == blockEpoch parent + 1 = do
            -- Block is in the next epoch, so we update the leadership election nonce.
            seedState <- getSeedState (bpState parent)
            -- We check that the epoch transition has been triggered in the parent block.
            if seedState ^. epochTransitionTriggered
                then
                    checkLeader
                        bakersAndFinalizers
                        (nonceForNewEpoch (bakersAndFinalizers ^. bfBakers) seedState)
                else do
                    -- If the transition is not triggered, then the child block should not be
                    -- in the new epoch.
                    logLoggable $ LogBlockInvalidEpoch pbHash (blockEpoch pendingBlock)
                    return BlockResultInvalid
        | otherwise = do
            -- The block's epoch is not valid.
            logLoggable $ LogBlockInvalidEpoch pbHash (blockEpoch pendingBlock)
            return BlockResultInvalid
    checkLeader bakersAndFinalizers leNonce
        | blockBaker pendingBlock == roundBaker ^. bakerIdentity = do
            -- The baker is the leader.
            let verifiedBlock =
                    VerifiedBlock
                        { vbBlock = pendingBlock,
                          vbBakersAndFinalizers = bakersAndFinalizers,
                          vbBakerInfo = roundBaker,
                          vbLeadershipElectionNonce = leNonce
                        }
            let blockWitness = toBlockSignatureWitness (pbBlock pendingBlock)
            -- Check if we've already seen a signed block from this baker in this round.
            use (roundBakerExistingBlock (blockRound pendingBlock) (blockBaker pendingBlock)) >>= \case
                Just w -> do
                    -- If the baker has already signed a block in this round then we flag it.
                    logLoggable $
                        LogBlockMultipleSigned (blockBaker pendingBlock) (blockRound pendingBlock)
                    flag (DuplicateBlock w blockWitness)
                    return $ BlockResultDoubleSign verifiedBlock
                Nothing -> do
                    -- If the baker has not already signed a block in this round, we record that
                    -- it has now and return control to the caller for retransmitting the block.
                    -- The caller will subsequently invoke 'executeBlock' to complete the process.
                    roundBakerExistingBlock (blockRound pendingBlock) (blockBaker pendingBlock)
                        ?= blockWitness
                    return $ BlockResultSuccess verifiedBlock
        | otherwise = do
            -- The baker is not the leader.
            logLoggable $
                LogBlockIncorrectBaker
                    pbHash
                    (blockBaker pendingBlock)
                    (roundBaker ^. bakerIdentity)
            flag (NotLeader (toBlockSignatureWitness (pbBlock pendingBlock)))
            return BlockResultInvalid
      where
        roundBaker =
            getLeaderFullBakers (bakersAndFinalizers ^. bfBakers) leNonce (blockRound pendingBlock)

-- |Process receiving a block when the parent is not live.
-- Precondition: the block is for a round and epoch that have not already been finalized.
--
-- A block with an unknown parent is added to the pending block table and marked pending, unless
-- one of the following conditions holds, in which case the block is rejected as invalid:
--
-- * The timestamp is no less than the receive time of the block + the early block threshold.
--   (Returns 'BlockResultEarly'.)
--
-- * The bakers for the block's epoch are known and either:
--
--     - the baker is not a valid baker for the epoch; or
--     - the baker is valid but the signature on the block is not valid.
--
-- * TODO: if any of the transactions in the block is invalid.
receiveBlockUnknownParent ::
    ( LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadLogger m
    ) =>
    PendingBlock ->
    m BlockResult
receiveBlockUnknownParent pendingBlock = do
    earlyThreshold <- rpEarlyBlockThreshold <$> use runtimeParameters
    if blockTimestamp pendingBlock
        < addDuration (utcTimeToTimestamp $ pbReceiveTime pendingBlock) earlyThreshold
        then do
            genesisHash <- use currentGenesisHash
            gets (getBakersForLiveEpoch (blockEpoch pendingBlock)) >>= \case
                Nothing -> do
                    -- We do not know the bakers, so we treat this like an early block.
                    return BlockResultEarly
                Just bf -- We know the bakers
                    | Just baker <- fullBaker (bf ^. bfBakers) (blockBaker pendingBlock),
                      verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                        -- The signature is valid
                        continuePending
                    | otherwise -> do
                        -- The signature is invalid
                        logLoggable $ LogBlockInvalidSignature pbHash
                        return BlockResultInvalid
        else return BlockResultEarly
  where
    pbHash = getHash pendingBlock
    continuePending = do
        -- TODO: Check the transactions in the block
        addPendingBlock pendingBlock
        markPending pendingBlock
        logLoggable $ LogBlockPending pbHash (blockParent pendingBlock)
        return BlockResultPending

-- |Get the minimum time between consecutive blocks, as of the specified block.
getMinBlockTime ::
    ( IsConsensusV1 (MPV m),
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m)
    ) =>
    BlockPointer (MPV m) ->
    m Duration
getMinBlockTime b = do
    cp <- getChainParameters (bpState b)
    return $ cp ^. cpConsensusParameters . cpMinBlockTime

-- |Add a newly-arrived block, returning the new block pointer. This does the following:
--
--  * Adds the block to the block table as alive.
--
--  * Adds the block to the live branches.
--
--  * Updates the statistics to reflect the arrival time of the new block.
--
-- Preconditions:
--
--  * The block's parent must be the last finalized block or another live block.
--
--  * The block must not already be a live block.
addBlock ::
    (TimeMonad m, MonadState (SkovData (MPV m)) m, MonadLogger m) =>
    -- |Block to add
    PendingBlock ->
    -- |Block state
    HashedPersistentBlockState (MPV m) ->
    -- |Parent pointer
    BlockPointer (MPV m) ->
    m (BlockPointer (MPV m))
addBlock pendingBlock blockState parent = do
    let height = blockHeight parent + 1
    now <- currentTime
    newBlock <- makeLiveBlock pendingBlock blockState height now
    addToBranches newBlock
    logLoggable $ LogBlockArrive (getHash pendingBlock)
    let nominalTime = timestampToUTCTime $ blockTimestamp newBlock
    let numTransactions = blockTransactionCount newBlock
    !stats <- updateStatsOnArrive nominalTime now numTransactions <$> use statistics
    statistics .= stats
    return newBlock

-- |A 'BlockPointer', ordered lexicographically on:
--
-- * Round number
-- * Epoch number
-- * Descending timestamp
-- * Descending receive time
-- * Descending arrival time
-- * Block hash
newtype OrderedBlock pv = OrderedBlock {theOrderedBlock :: BlockPointer pv}

instance Ord (OrderedBlock pv) where
    compare =
        compare `on` toTuple
      where
        toTuple (OrderedBlock blk) =
            ( blockRound blk,
              blockEpoch blk,
              Down (blockTimestamp blk, blockReceiveTime blk, blockArriveTime blk),
              getHash @BlockHash blk
            )

instance Eq (OrderedBlock pv) where
    a == b = compare a b == EQ

processPendingChildren ::
    ( IsConsensusV1 (MPV m),
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m,
      MonadLogger m
    ) =>
    BlockPointer (MPV m) ->
    m (OrderedBlock (MPV m))
processPendingChildren parent = do
    children <- takePendingChildren (getHash parent)
    foldM process (OrderedBlock parent) children
  where
    process best child = do
        res <- processPendingChild child
        return $! maybe best (max best) res

-- |Process a pending child block given that its parent has become live.
--
-- PRECONDITIONS:
--   - The block's signature has been verified. (This is the case for )
--
-- TODO: Notify pending live
processPendingChild ::
    ( IsConsensusV1 (MPV m),
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m,
      MonadLogger m
    ) =>
    PendingBlock ->
    m (Maybe (OrderedBlock (MPV m)))
processPendingChild block = do
    sd <- get
    if isPending blockHash sd
        then case getLiveOrLastFinalizedBlock (blockParent block) sd of
            Just parent -> do
                receiveBlockKnownParent parent block >>= \case
                    BlockResultSuccess vb -> do
                        processReceiveOK parent vb
                    BlockResultDoubleSign vb -> do
                        processReceiveOK parent vb
                    _ -> do
                        processAsDead
            Nothing -> do
                -- The block can never be part of the tree since the parent is not live/last
                -- finalized, and yet the parent has already been processed.
                processAsDead
        else return Nothing
  where
    blockHash = getHash block
    processAsDead = do
        blockArriveDead blockHash
        return Nothing
    processReceiveOK parent vb = do
        processBlock parent vb >>= \case
            Just newBlock -> Just <$> processPendingChildren newBlock
            Nothing -> processAsDead

-- |Process a received block. This handles the processing after the initial checks and after the
-- block has been relayed (or not). This DOES NOT include processing pending children of the block
-- or signing the block as a finalizer.
--
-- Precondition:
--
-- * The parent block is correct: @getHash parent == blockParent pendingBlock@.
-- * The block is signed by a valid baker for its epoch.
-- * The baker is the leader for the round according to the parent block.
--
-- TODO: Transaction processing.
processBlock ::
    ( IsConsensusV1 (MPV m),
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m,
      MonadLogger m
    ) =>
    -- |Parent block (@parent@)
    BlockPointer (MPV m) ->
    -- |Block being processed (@pendingBlock@)
    VerifiedBlock ->
    m (Maybe (BlockPointer (MPV m)))
processBlock parent VerifiedBlock{vbBlock = pendingBlock, ..}
    -- Check that the QC is consistent with the parent block round.
    | qcRound (blockQuorumCertificate pendingBlock) /= blockRound parent = do
        logLoggable $ LogBlockQCRoundInconsistent pbHash
        flag $ BlockQCRoundInconsistent sBlock
        rejectBlock
    -- Check that the QC is consistent with the parent block epoch.
    | qcEpoch (blockQuorumCertificate pendingBlock) /= blockEpoch parent = do
        logLoggable $ LogBlockQCEpochInconsistent pbHash
        flag $ BlockQCEpochInconsistent sBlock
        rejectBlock
    -- Check that the block round is greater than the round of the parent block.
    | blockRound parent >= blockRound pendingBlock = do
        logLoggable $ LogBlockRoundInconsistent pbHash
        flag $ BlockRoundInconsistent sBlock
        rejectBlock
    -- Check that the block epoch is either the same as the epoch of the parent block or the next
    -- epoch.
    -- This is equivalent to the condition from the blue paper:
    --
    -- blockEpoch parent > blockEpoch pendingBlock || blockEpoch parent < blockEpoch pendingBlock - 1
    --
    -- If the block has been successfully verified by 'receiveBlockKnownParent', this test cannot
    -- fail, because it is already checked there.
    | blockEpoch parent /= blockEpoch pendingBlock,
      blockEpoch parent + 1 /= blockEpoch pendingBlock = do
        logLoggable $ LogBlockEpochInconsistent pbHash
        flag $ BlockEpochInconsistent sBlock
        rejectBlock
    -- [Note: the timestamp check is deferred in the implementation compared to the bluepaper.]
    -- Check that the block nonce is correctly formed
    | not $
        verifyBlockNonce
            vbLeadershipElectionNonce
            (blockRound pendingBlock)
            (vbBakerInfo ^. bakerElectionVerifyKey)
            (blockNonce pendingBlock) =
        do
            logLoggable $ LogBlockNonceIncorrect pbHash
            flag $ BlockNonceIncorrect sBlock
            rejectBlock
    | otherwise = do
        genMeta <- use genesisMetadata
        checkTimestamp $
            getParentBakersAndFinalizers $ \parentBF ->
                checkQC genMeta parentBF $
                    checkTC genMeta parentBF $
                        checkEpochFinalizationEntry genMeta parentBF $
                            checkBlockExecution genMeta $ \blockState -> do
                                -- When adding a block, we guarantee that the new block is at most
                                -- one epoch after the last finalized block. If the block is in the
                                -- same epoch as its parent, then the guarantee is upheld by the
                                -- fact that the parent block was checked to be live (and the
                                -- last finalized block is unchanged). If the block is in the next
                                -- epoch from the parent, then 'checkEpochFinalizationEntry' checks:
                                --  * The block contains a valid epoch finalization entry for the
                                --    epoch of the parent block. In particular, it finalizes a
                                --    block in that epoch which triggers the epoch transition.
                                --  * The block finalized by the finalization entry is now
                                --    considered finalized.
                                --  * The parent block is descended from the (new) last finalized
                                --    block.
                                -- This implies that the block is in the epoch after the last
                                -- finalized block.
                                newBlock <- addBlock pendingBlock blockState parent
                                -- Note that the parent block could potentially no longer be live
                                -- as a result of finalization processing in
                                -- 'checkEpochFinalizationEntry'.
                                checkFinality (blockQuorumCertificate pendingBlock)
                                checkedUpdateHighestQC (blockQuorumCertificate pendingBlock)
                                curRound <- use $ roundStatus . rsCurrentRound
                                when (curRound < blockRound pendingBlock) $
                                    advanceRound (blockRound pendingBlock) $
                                        case blockTimeoutCertificate pendingBlock of
                                            Present tc -> Left (tc, blockQuorumCertificate pendingBlock)
                                            Absent -> Right (blockQuorumCertificate pendingBlock)
                                return (Just newBlock)
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock
    sBlock = pbBlock pendingBlock
    rejectBlock = return Nothing
    -- Check the timestamp and invoke the continue if successful.
    checkTimestamp continue = do
        -- Check the timestamp is sufficiently far after the parent block.
        minBlockTime <- getMinBlockTime parent
        if blockTimestamp pendingBlock < blockTimestamp parent `addDuration` minBlockTime
            then do
                logLoggable $
                    LogBlockTooFast
                        pbHash
                        (blockTimestamp pendingBlock)
                        minBlockTime
                        (blockTimestamp parent)
                flag $ BlockTooFast sBlock (bpBlock parent)
                rejectBlock
            else continue
    -- Check the timeout certificate for presence and well-formedness when required.
    -- If successful, invoke the continuation with the timeout certificate, if it is required.
    checkTC GenesisMetadata{..} parentBF continue
        | blockRound pendingBlock > blockRound parent + 1 =
            case blockTimeoutCertificate pendingBlock of
                -- Check that a timeout certificate is present if the block round is not the
                -- sequentially next round.
                Absent -> do
                    logLoggable $ LogBlockTCMissing pbHash
                    flag $ BlockTCMissing sBlock
                    rejectBlock
                Present tc
                    -- Check that the TC round is correct.
                    | tcRound tc /= blockRound pendingBlock - 1 -> do
                        logLoggable $ LogBlockTCRoundInconsistent pbHash
                        flag $ BlockTCRoundInconsistent sBlock
                        rejectBlock
                    | blockRound parent < tcMaxRound tc
                        || blockEpoch parent < tcMaxEpoch tc
                        || blockEpoch parent - tcMinEpoch tc > 2 -> do
                        logLoggable $ LogBlockQCInconsistentWithTC pbHash
                        logEvent Konsensus LLTrace $ "blockRound parent: " ++ show (blockRound parent)
                        logEvent Konsensus LLTrace $ "tcMaxRound tc: " ++ show (tcMaxRound tc)
                        logEvent Konsensus LLTrace $ "blockEpoch parent: " ++ show (blockEpoch parent)
                        logEvent Konsensus LLTrace $ "tcMaxEpoch tc: " ++ show (tcMaxEpoch tc)
                        logEvent Konsensus LLTrace $ "tcMinEpoch tc: " ++ show (tcMinEpoch tc)
                        flag $ BlockQCInconsistentWithTC sBlock
                        rejectBlock
                    | otherwise -> do
                        -- Check correctness of the timeout certificate.
                        -- The timeout certificate should only contain timeouts for epochs that
                        -- are at most one epoch from the epoch of the last finalized block (LFB).
                        -- In particular, there cannot be a timeout more than one epoch earlier
                        -- than the LFB: The round under consideration is after the round of the
                        -- LFB. The baker of the present block considers that round to be in at
                        -- least the epoch of the LFB. Honest parties cannot be more than one
                        -- epoch apart on the same round, so if the baker is honest then they would
                        -- not include timeouts from any epoch earlier than one before the epoch of
                        -- the LFB.
                        -- Also, there cannot be a timeout more than one epoch later than the LFB.
                        -- This is because we already reject above if
                        --    blockEpoch parent < tcMaxEpoch tc
                        -- Given that the parent is live and descended from the LFB, it can be
                        -- at most one epoch later than the LFB, and so also the tcMaxEpoch.
                        eBkrs <- use epochBakers
                        let checkTCValid eb1 eb2 =
                                checkTimeoutCertificate
                                    gmCurrentGenesisHash
                                    (toRational $ genesisSignatureThreshold gmParameters)
                                    (eb1 ^. bfFinalizers)
                                    (eb2 ^. bfFinalizers)
                                    (parentBF ^. bfFinalizers)
                                    tc
                        -- FIXME: can have tcMinEpoch tc == eBkrs ^. currentEpoch - 2
                        let tcOK
                                | tcMinEpoch tc == eBkrs ^. currentEpoch - 1 =
                                    checkTCValid
                                        (eBkrs ^. previousEpochBakers)
                                        (eBkrs ^. currentEpochBakers)
                                | tcMinEpoch tc == eBkrs ^. currentEpoch =
                                    checkTCValid
                                        (eBkrs ^. currentEpochBakers)
                                        (eBkrs ^. nextEpochBakers)
                                | tcMinEpoch tc == eBkrs ^. currentEpoch + 1,
                                  tcIsSingleEpoch tc =
                                    checkTCValid
                                        (eBkrs ^. nextEpochBakers)
                                        -- Because the TC is for a single epoch, the second set of
                                        -- bakers will not be used in the validity check.
                                        (eBkrs ^. nextEpochBakers)
                                | otherwise = False
                        if tcOK
                            then continue
                            else do
                                logLoggable $ LogBlockInvalidTC pbHash
                                flag $ BlockInvalidTC sBlock
                                rejectBlock
        -- If the previous round didn't timeout, check we have no timeout certificate
        | Present _ <- blockTimeoutCertificate pendingBlock = do
            logLoggable $ LogBlockUnexpectedTC pbHash
            flag $ BlockUnexpectedTC sBlock
            rejectBlock
        | otherwise = continue
    -- Check the finalization entry, and process it if it updates our perspective on the last
    -- finalized block.
    checkEpochFinalizationEntry GenesisMetadata{..} BakersAndFinalizers{..} continue
        -- If the block is in a new epoch, the epoch finalization entry should be present and
        -- correct.
        | blockEpoch pendingBlock == blockEpoch parent + 1 =
            case blockEpochFinalizationEntry pendingBlock of
                Absent -> do
                    logLoggable $ LogBlockEpochFinalizationMissing pbHash
                    flag $ BlockEpochFinalizationMissing sBlock
                    rejectBlock
                Present finEntry
                    -- Check that the finalization entry is for the correct epoch and
                    -- contains valid QCs.
                    | qcEpoch (feFinalizedQuorumCertificate finEntry) == blockEpoch parent,
                      checkFinalizationEntry
                        gmCurrentGenesisHash
                        (toRational $ genesisSignatureThreshold gmParameters)
                        _bfFinalizers
                        finEntry -> do
                        -- Check that the finalized block has timestamp past the trigger time for
                        -- the epoch. We use the seed state for the parent block to get the trigger
                        -- time, because that is in the same epoch.
                        parentSeedState <- getSeedState (bpState parent)
                        -- Get the status of the block finalized by the finalization entry for the
                        -- check.
                        let finBlockHash = qcBlock (feFinalizedQuorumCertificate finEntry)
                        get >>= getBlockStatus finBlockHash >>= \case
                            BlockFinalized finBlock
                                | blockTimestamp finBlock >= parentSeedState ^. triggerBlockTime ->
                                    -- We already know that the block is finalized.
                                    continue
                            BlockAlive finBlock
                                | blockTimestamp finBlock >= parentSeedState ^. triggerBlockTime -> do
                                    -- We do not currently consider the block finalized, so we
                                    -- process the finalization now. The block we are finalizing is
                                    -- a live (non-finalized) block, and therefore it is at most
                                    -- one epoch after the last finalized block.
                                    processFinalization finBlock finEntry
                                    shrinkTimeout finBlock
                                    -- Update the highest QC.
                                    checkedUpdateHighestQC $ feSuccessorQuorumCertificate finEntry
                                    -- We check if the block is descended from the NEW last
                                    -- finalized block, since that should have changed.
                                    gets (getLiveOrLastFinalizedBlock (getHash parent)) >>= \case
                                        Just _ -> continue
                                        _ -> do
                                            logLoggable $
                                                LogBlockUnrelatedEpochFinalization
                                                    pbHash
                                                    finBlockHash
                                            -- Possibly we should flag in this case.
                                            rejectBlock
                            _ -> do
                                logLoggable $
                                    LogBlockInvalidEpochFinalizationTarget
                                        pbHash
                                        finBlockHash
                                flag $ BlockInvalidEpochFinalization sBlock
                                rejectBlock
                    | otherwise -> do
                        logLoggable $ LogBlockInvalidEpochFinalization pbHash
                        flag $ BlockInvalidEpochFinalization sBlock
                        rejectBlock
        -- Here, the epoch must be the same as the parent epoch by the earlier epoch consistency
        -- check, and so we require the epoch finalization entry to be absent.
        | Present _ <- blockEpochFinalizationEntry pendingBlock = do
            logLoggable $ LogBlockUnexpectedEpochFinalization pbHash
            flag $ BlockUnexpectedEpochFinalization sBlock
            rejectBlock
        | otherwise = continue
    checkBlockExecution GenesisMetadata{..} continue = do
        let execData =
                BlockExecutionData
                    { bedIsNewEpoch = blockEpoch pendingBlock == blockEpoch parent + 1,
                      bedEpochDuration = genesisEpochDuration gmParameters,
                      bedTimestamp = blockTimestamp pendingBlock,
                      bedBlockNonce = blockNonce pendingBlock,
                      bedParentState = bpState parent
                    }
        executeBlockStateUpdate execData >>= \case
            Left failureReason -> do
                logLoggable $ LogBlockExecutionFailure pbHash failureReason
                flag (BlockExecutionFailure sBlock)
                rejectBlock
            Right newState -> do
                outcomesHash <- getTransactionOutcomesHash newState
                if
                        | outcomesHash /= blockTransactionOutcomesHash pendingBlock -> do
                            -- Incorrect transaction outcomes
                            logLoggable $
                                LogBlockInvalidTransactionOutcomesHash
                                    pbHash
                                    (blockTransactionOutcomesHash pendingBlock)
                                    outcomesHash
                            flag $ BlockInvalidTransactionOutcomesHash sBlock (bpBlock parent)
                            rejectBlock
                        | getHash newState /= blockStateHash pendingBlock -> do
                            -- Incorrect state hash
                            logLoggable $
                                LogBlockInvalidStateHash
                                    pbHash
                                    (blockStateHash pendingBlock)
                                    (getHash newState)
                            flag $ BlockInvalidStateHash sBlock (bpBlock parent)
                            rejectBlock
                        | otherwise ->
                            continue newState
    getParentBakersAndFinalizers continue
        | blockEpoch parent == blockEpoch pendingBlock = continue vbBakersAndFinalizers
        | otherwise =
            gets (getBakersForLiveEpoch (blockEpoch parent)) >>= \case
                -- If this case happens, the parent block must now precede the last finalized block,
                -- so we can no longer add the block.
                Nothing -> rejectBlock
                Just bf -> continue bf
    checkQC GenesisMetadata{..} BakersAndFinalizers{..} continue = do
        let qcOK =
                checkQuorumCertificate
                    gmCurrentGenesisHash
                    (toRational $ genesisSignatureThreshold gmParameters)
                    _bfFinalizers
                    (blockQuorumCertificate pendingBlock)
        if qcOK
            then continue
            else do
                logLoggable $ LogBlockInvalidQC pbHash
                flag $ BlockInvalidQC sBlock
                rejectBlock

-- |Update the highest QC if the supplied QC is for a later round than the previous QC.
checkedUpdateHighestQC ::
    ( MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    QuorumCertificate ->
    m ()
checkedUpdateHighestQC newQC = do
    rs <- use roundStatus
    let isBetterQC = qcRound (rs ^. rsHighestQC) < qcRound newQC
    when isBetterQC $ do
        roundExistingQuorumCertificate (qcRound newQC) ?= toQuorumCertificateWitness newQC
        setRoundStatus $!
            rs{_rsHighestQC = newQC}

executeBlock ::
    ( IsConsensusV1 (MPV m),
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m,
      MonadReader r m,
      HasBakerContext r,
      TimerMonad m,
      MonadMulticast m,
      MonadLogger m
    ) =>
    VerifiedBlock ->
    m ()
executeBlock verifiedBlock = do
    -- TODO: check for consensus shutdown
    gets (getLiveOrLastFinalizedBlock (blockParent (vbBlock verifiedBlock))) >>= \case
        Just parent -> do
            res <- processBlock parent verifiedBlock
            forM_ res $ \newBlock -> do
                OrderedBlock best <- processPendingChildren newBlock
                checkedValidateBlock best
        Nothing -> return ()

checkedValidateBlock ::
    ( MonadReader r m,
      HasBakerContext r,
      BlockData b,
      HashableTo BlockHash b,
      TimerMonad m,
      MonadState (SkovData (MPV m)) m,
      MonadMulticast m,
      LowLevel.MonadTreeStateStore m,
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadTimeout m,
      MonadLogger m,
      IsConsensusV1 (MPV m)
    ) =>
    b ->
    m ()
checkedValidateBlock validBlock = do
    mBakerIdentity <- view Consensus.bakerIdentity
    forM_ mBakerIdentity $ \bakerIdent@BakerIdentity{..} -> do
        mBakers <- gets $ getBakersForLiveEpoch (blockEpoch validBlock)
        forM_ mBakers $ \BakersAndFinalizers{..} -> do
            forM_ (finalizerByBakerId _bfFinalizers bakerId) $ \ !finInfo ->
                if finalizerSignKey finInfo /= Sig.verifyKey bakerSignKey
                    || finalizerBlsKey finInfo /= bakerAggregationPublicKey
                    then do
                        -- TODO: Log a warning that the key does not match.
                        return ()
                    else do
                        let !blockHash = getHash validBlock
                        _ <-
                            onTimeout (DelayUntil (timestampToUTCTime $ blockTimestamp validBlock)) $!
                                validateBlock blockHash bakerIdent finInfo
                        return ()

-- |Produce a quorum signature on a block.
-- This checks that the block is still valid, is in the current round and epoch, and the
-- round is signable (i.e. we haven't already signed a quorum message or timeout message).
--
-- It is assumed that the supplied 'FinalizerInfo' accurately represents the finalizer in the
-- epoch of the block, and that the baker identity matches the finalizer info (i.e. the keys
-- are correct).
validateBlock ::
    ( MonadState (SkovData (MPV m)) m,
      MonadMulticast m,
      LowLevel.MonadTreeStateStore m,
      IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadTimeout m,
      MonadLogger m
    ) =>
    -- |The block to sign.
    BlockHash ->
    -- |The baker keys.
    BakerIdentity ->
    -- |The details of the finalizer in the finalization committee for the block's epoch.
    FinalizerInfo ->
    m ()
validateBlock blockHash BakerIdentity{..} finInfo = do
    maybeBlock <- gets $ getLiveBlock blockHash
    forM_ maybeBlock $ \block -> do
        rs@RoundStatus{..} <- use roundStatus
        curEpoch <- use currentEpoch
        when
            ( blockRound block == _rsCurrentRound
                && rsNextSignableRound rs <= blockRound block
                && blockEpoch block == curEpoch
            )
            $ do
                genesisHash <- use currentGenesisHash
                let qsm =
                        QuorumSignatureMessage
                            { qsmGenesis = genesisHash,
                              qsmBlock = blockHash,
                              qsmRound = blockRound block,
                              qsmEpoch = blockEpoch block
                            }
                let quorumSignature = signQuorumSignatureMessage qsm bakerAggregationKey
                let finIndex = finalizerIndex finInfo

                let quorumMessage = buildQuorumMessage qsm quorumSignature finIndex
                setRoundStatus $! rs{_rsLastSignedQuorumMessage = Present quorumMessage}
                sendQuorumMessage quorumMessage
                processQuorumMessage $
                    VerifiedQuorumMessage
                        { vqmMessage = quorumMessage,
                          vqmFinalizerWeight = finalizerWeight finInfo
                        }
