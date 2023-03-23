{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Blocks where

import Control.Monad.Reader
import Control.Monad.State
import Lens.Micro.Platform

import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus (HasBakerContext, MonadTimeout, advanceRound, computeFinalizationCommittee)
import qualified Concordium.KonsensusV1.Consensus as Consensus
import Concordium.KonsensusV1.Consensus.Finality
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

uponReceivingBlock ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m
    ) =>
    PendingBlock ->
    m BlockResult
uponReceivingBlock pendingBlock = do
    -- TODO: Check for consensus shutdown
    lfb <- use lastFinalized
    if blockEpoch pendingBlock < blockEpoch lfb || blockRound pendingBlock <= blockRound lfb
        then -- The block is from an old epoch, or already finalized round
            return BlockResultStale
        else do
            sd <- get
            -- Check that the block is not already live or pending. The network-layer deduplication
            -- should generally prevent such blocks from getting here, but having this check means
            -- we can rely on the fact.
            case getMemoryBlockStatus (getHash pendingBlock) sd of
                Just _ -> return BlockResultDuplicate
                Nothing -> do
                    getRecentBlockStatus (blockParent pendingBlock) sd >>= \case
                        RecentBlock (BlockAlive parent) -> receiveBlockKnownParent parent pendingBlock
                        RecentBlock (BlockFinalized parent) -> receiveBlockKnownParent parent pendingBlock
                        RecentBlock BlockPending{} -> receiveBlockUnknownParent pendingBlock
                        RecentBlock BlockDead -> return BlockResultStale
                        RecentBlock BlockUnknown -> receiveBlockUnknownParent pendingBlock
                        OldFinalized -> return BlockResultStale

-- |Get the bakers for a particular epoch, given that we have a live block that is nominally the
-- parent block. This will typically get the bakers from the cache that is with respect to the
-- current epoch. However, in the (unlikely) case that the block is from an epoch further
-- than can be deduced from the cache, we derive the bakers from the supplied parent
-- block.
--
-- Preconditions:
--  - The target epoch is either the same as the epoch of the parent block, or the next epoch.
--  - The parent block is live, and so either in the same epoch or the next epoch as the last
--    finalized block.
getBakersForEpochKnownParent ::
    ( IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m)
    ) =>
    BlockPointer (MPV m) ->
    Epoch ->
    m (Maybe BakersAndFinalizers)
getBakersForEpochKnownParent parent targetEpoch =
    gets (getBakersForLiveEpoch targetEpoch) >>= \case
        Nothing | targetEpoch == blockEpoch parent + 1 -> do
            _bfBakers <- getNextEpochBakers (bpState parent)
            finParams <- getNextEpochFinalizationCommitteeParameters (bpState parent)
            let _bfFinalizers = computeFinalizationCommittee _bfBakers finParams
            return $ Just BakersAndFinalizers{..}
        mbakers -> return mbakers

-- |Process receiving a block where the parent is live (i.e. descended from the last finalized
-- block).  If this function returns 'BlockResultSuccess' then the caller is obliged to call
-- 'executeBlock' on the returned 'VerifiedBlock' in order to complete the processing of the block,
-- after relaying the block to peers.
receiveBlockKnownParent ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m
    ) =>
    BlockPointer (MPV m) ->
    PendingBlock ->
    m BlockResult
receiveBlockKnownParent parent pendingBlock = do
    genesisHash <- use $ genesisMetadata . to gmCurrentGenesisHash
    getBakersForEpochKnownParent parent (blockEpoch pendingBlock) >>= \case
        Nothing ->
            -- The block's epoch is not valid. 'getBakersForEpochKnownParent' will return the
            -- bakers and finalizers if the block is in the same epoch as its parent (which is
            -- at most one epoch after the last finalized block) or in the next epoch (which
            -- can be computed from the parent block).
            return BlockResultInvalid
        Just bakersAndFinalizers
            -- We know the bakers
            | Just baker <- fullBaker (bakersAndFinalizers ^. bfBakers) (blockBaker pendingBlock),
              verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                -- The signature is valid
                receiveSigned bakersAndFinalizers
            | otherwise -> do
                -- The signature is invalid
                return BlockResultInvalid
  where
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
                else -- If the transition is not triggered, then the child block should not be
                -- in the new epoch.
                    return BlockResultInvalid
        | otherwise =
            -- The block's epoch is not valid.
            return BlockResultInvalid
    checkLeader bakersAndFinalizers leNonce
        | let roundBaker = getLeaderFullBakers (bakersAndFinalizers ^. bfBakers) leNonce (blockRound pendingBlock),
          blockBaker pendingBlock == roundBaker ^. bakerIdentity = do
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
                    -- If the baker has already signed a block in this round then we flag it and
                    -- execute it immediately (without rebroadcasting).
                    flag (DuplicateBlock w blockWitness)
                    _ <- processBlock parent verifiedBlock
                    return BlockResultDuplicate
                Nothing -> do
                    -- If the baker has not already signed a block in this round, we record that
                    -- it has now and return control to the caller for retransmitting the block.
                    -- The caller will subsequently invoke 'executeBlock' to complete the process.
                    roundBakerExistingBlock (blockRound pendingBlock) (blockBaker pendingBlock)
                        ?= blockWitness
                    return $ BlockResultSuccess verifiedBlock
        | otherwise = do
            flag (NotLeader (toBlockSignatureWitness (pbBlock pendingBlock)))
            return BlockResultInvalid

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
      MonadState (SkovData (MPV m)) m
    ) =>
    PendingBlock ->
    m BlockResult
receiveBlockUnknownParent pendingBlock = do
    earlyThreshold <- rpEarlyBlockThreshold <$> use runtimeParameters
    if blockTimestamp pendingBlock
        < addDuration (utcTimeToTimestamp $ pbReceiveTime pendingBlock) earlyThreshold
        then do
            genesisHash <- use $ genesisMetadata . to gmCurrentGenesisHash
            gets (getBakersForLiveEpoch (blockEpoch pendingBlock)) >>= \case
                Nothing -> do
                    -- We do not know the bakers
                    continuePending
                Just bf -- We know the bakers
                    | Just baker <- fullBaker (bf ^. bfBakers) (blockBaker pendingBlock),
                      verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                        -- The signature is valid
                        continuePending
                    | otherwise ->
                        -- The signature is invalid
                        return BlockResultInvalid
        else return BlockResultEarly
  where
    continuePending = do
        -- TODO: Check the transactions in the block
        addPendingBlock pendingBlock
        markPending pendingBlock
        return BlockResultPending

-- |Get the minimum time between consecutive blocks.
-- TODO: should account for pending changes?
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
    (TimeMonad m, MonadState (SkovData (MPV m)) m) =>
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
    let nominalTime = timestampToUTCTime $ blockTimestamp newBlock
    let numTransactions = blockTransactionCount newBlock
    !stats <- updateStatsOnArrive nominalTime now numTransactions <$> use statistics
    statistics .= stats
    return newBlock

-- |Process a received block. This handles the processing after the initial checks and after the
-- block has been relayed (or not). This DOES NOT include signing the block as a finalizer.
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
      MonadTimeout m
    ) =>
    -- |Parent block (@parent@)
    BlockPointer (MPV m) ->
    -- |Block being processed (@pendingBlock@)
    VerifiedBlock ->
    m Bool
processBlock parent VerifiedBlock{vbBlock = pendingBlock, ..}
    -- Check that the QC is consistent with the parent block round.
    | qcRound (blockQuorumCertificate pendingBlock) /= blockRound parent = do
        flag $ BlockQCRoundInconsistent sBlock
        return False
    -- Check that the QC is consistent with the parent block epoch.
    | qcEpoch (blockQuorumCertificate pendingBlock) /= blockEpoch parent = do
        flag $ BlockQCEpochInconsistent sBlock
        return False
    -- Check that the block round is greater than the round of the parent block.
    | blockRound parent >= blockRound pendingBlock = do
        flag $ BlockRoundInconsistent sBlock
        return False
    -- Check that the block epoch is either the same as the epoch of the parent block or the next
    -- epoch.
    | blockEpoch parent > blockEpoch pendingBlock || blockEpoch parent < blockEpoch pendingBlock - 1 = do
        flag $ BlockEpochInconsistent sBlock
        return False
    -- [Note: the timestamp check is deferred in the implementation compared to the bluepaper.]
    -- Check that the block nonce is correctly formed
    | not $
        verifyBlockNonce
            vbLeadershipElectionNonce
            (blockRound pendingBlock)
            (vbBakerInfo ^. bakerElectionVerifyKey)
            (blockNonce pendingBlock) =
        do
            flag $ BlockNonceIncorrect sBlock
            return False
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
                                _ <- addBlock pendingBlock blockState parent
                                checkFinalityWithBlock (blockQuorumCertificate pendingBlock) parent
                                checkedUpdateHighestQC (blockQuorumCertificate pendingBlock)
                                curRound <- use $ roundStatus . rsCurrentRound
                                when (curRound < blockRound pendingBlock) $
                                    advanceRound (blockRound pendingBlock) $
                                        case blockTimeoutCertificate pendingBlock of
                                            Present tc -> Left (tc, blockQuorumCertificate pendingBlock)
                                            Absent -> Right (blockQuorumCertificate pendingBlock)
                                return True
  where
    sBlock = pbBlock pendingBlock
    -- Check the timestamp and invoke the continue if successful.
    checkTimestamp continue = do
        -- Check the timestamp is sufficiently far after the parent block.
        minBlockTime <- getMinBlockTime parent
        if blockTimestamp pendingBlock < blockTimestamp parent `addDuration` minBlockTime
            then do
                flag $ BlockTooFast sBlock (bpBlock parent)
                return False
            else continue
    -- Check the timeout certificate for presence and well-formedness when required.
    -- If successful, invoke the continuation with the timeout certificate, if it is required.
    checkTC GenesisMetadata{..} parentBF continue
        | blockRound pendingBlock > blockRound parent + 1 =
            case blockTimeoutCertificate pendingBlock of
                -- Check that a timeout certificate is present if the block round is not the
                -- sequentially next round.
                Absent -> do
                    flag $ BlockTCMissing sBlock
                    return False
                Present tc
                    -- Check that the TC round is correct.
                    | tcRound tc /= blockRound pendingBlock - 1 -> do
                        flag $ BlockTCRoundInconsistent sBlock
                        return False
                    | blockRound parent < tcMaxRound tc
                        || blockEpoch parent < tcMaxEpoch tc
                        || blockEpoch parent - tcMinEpoch tc > 2 -> do
                        flag $ BlockQCInconsistentWithTC sBlock
                        return False
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
                                flag $ BlockInvalidTC sBlock
                                return False
        -- If the previous round didn't timeout, check we have no timeout certificate
        | Present _ <- blockTimeoutCertificate pendingBlock = do
            flag $ BlockUnexpectedTC sBlock
            return False
        | otherwise = continue
    -- Check the finalization entry, and process it if it updates our perspective on the last
    -- finalized block.
    checkEpochFinalizationEntry GenesisMetadata{..} BakersAndFinalizers{..} continue
        -- If the block is in a new epoch, the epoch finalization entry should be present and
        -- correct.
        | blockEpoch pendingBlock == blockEpoch parent + 1 =
            case blockEpochFinalizationEntry pendingBlock of
                Absent -> do
                    flag $ BlockEpochFinalizationMissing sBlock
                    return False
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
                        get >>= getBlockStatus (qcBlock (feFinalizedQuorumCertificate finEntry)) >>= \case
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
                                    gets (getMemoryBlockStatus (getHash parent)) >>= \case
                                        Just (BlockAliveOrFinalized _) -> continue
                                        _ -> do
                                            -- Possibly we should flag in this case.
                                            return False
                            _ -> do
                                flag $ BlockInvalidEpochFinalization sBlock
                                return False
                    | otherwise -> do
                        flag $ BlockInvalidEpochFinalization sBlock
                        return False
        -- Here, the epoch must be the same as the parent epoch by the earlier epoch consistency
        -- check, and so we require the epoch finalization entry to be absent.
        | Present _ <- blockEpochFinalizationEntry pendingBlock = do
            flag $ BlockUnexpectedEpochFinalization sBlock
            return False
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
            Left _ -> do
                flag (BlockExecutionFailure sBlock)
                return False
            Right newState -> do
                outcomesHash <- getTransactionOutcomesHash newState
                if
                        | outcomesHash /= blockTransactionOutcomesHash pendingBlock -> do
                            flag $ BlockInvalidTransactionOutcomesHash sBlock (bpBlock parent)
                            return False
                        | getHash newState /= blockStateHash pendingBlock -> do
                            flag $ BlockInvalidStateHash sBlock (bpBlock parent)
                            return False
                        | otherwise ->
                            continue newState
    getParentBakersAndFinalizers continue
        | blockEpoch parent == blockEpoch pendingBlock = continue vbBakersAndFinalizers
        | otherwise =
            gets (getBakersForLiveEpoch (blockEpoch parent)) >>= \case
                -- If this case happens, the parent block must now precede the last finalized block,
                -- so we can no longer add the block.
                Nothing -> return False
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
                flag $ BlockInvalidQC sBlock
                return False

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
    when isBetterQC $
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
      TimerMonad m
    ) =>
    VerifiedBlock ->
    m ()
executeBlock verifiedBlock = do
    -- TODO: check for consensus shutdown
    get >>= getRecentBlockStatus (blockParent (vbBlock verifiedBlock)) >>= \case
        RecentBlock (BlockAliveOrFinalized parent) -> ebWithParent parent
        _ -> return ()
  where
    ebWithParent parent = do
        success <- processBlock parent verifiedBlock
        when success $ do
            checkedValidateBlock (vbBlock verifiedBlock)

checkedValidateBlock ::
    ( MonadReader r m,
      HasBakerContext r,
      BlockData b,
      HashableTo BlockHash b,
      TimerMonad m,
      MonadState (SkovData (MPV m)) m
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

validateBlock :: (MonadState (SkovData (MPV m)) m) => BlockHash -> BakerIdentity -> FinalizerInfo -> m ()
validateBlock blockHash bakerIdent finInfo = do
    block <- gets $ getLiveBlock blockHash
    -- TODO: implementation
    return ()
