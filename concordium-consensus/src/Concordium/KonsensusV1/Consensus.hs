{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import Lens.Micro.Platform

import Concordium.Types

import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Types.SeedState

-- |A block that has passed initial verification, but must still be executed, added to the state,
-- and (potentially) signed as a finalizer.
data VerifiedBlock = VerifiedBlock
    { -- |The block that has passed initial verification.
      vbBlock :: !PendingBlock,
      -- |The bakers for the epoch of this block.
      vbBakers :: !FullBakers,
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
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadIO m
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
                        Unknown -> receiveBlockUnknownParent pendingBlock

-- |Process receiving a block where the parent is live (i.e. descended from the last finalized
-- block).  If this function returns 'BlockResultSuccess' then the caller is obliged to call
-- 'executeBlock' on the returned 'VerifiedBlock' in order to complete the processing of the block,
-- after relaying the block to peers.
receiveBlockKnownParent ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadIO m
    ) =>
    BlockPointer (MPV m) ->
    PendingBlock ->
    m BlockResult
receiveBlockKnownParent parent pendingBlock = do
    genesisHash <- use $ genesisMetadata . to gmCurrentGenesisHash
    gets (doGetBakersForEpoch (blockEpoch pendingBlock)) >>= \case
        Nothing ->
            -- The block's epoch is not valid. A live block must either be in the same epoch as
            -- the last finalized block or in the next epoch. If 'doGetBakersForEpoch' returns
            -- 'Nothing', then it is not in either of those.
            return BlockResultInvalid
        Just bf
            -- We know the bakers
            | Just baker <- fullBaker (bf ^. bfBakers) (blockBaker pendingBlock),
              verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                -- The signature is valid
                receiveSigned (bf ^. bfBakers)
            | otherwise -> do
                -- The signature is invalid
                return BlockResultInvalid
  where
    receiveSigned bakers
        | blockEpoch pendingBlock == blockEpoch parent = do
            -- Block is in the current epoch, so use the leadership election nonce
            -- from the parent block.
            seedState <- getSeedState (bpState parent)
            checkLeader bakers (currentLeadershipElectionNonce seedState)
        | blockEpoch pendingBlock == blockEpoch parent + 1 = do
            -- Block is in the next epoch, so we update the leadership election nonce.
            seedState <- getSeedState (bpState parent)
            -- We check that the epoch transition has been triggered in the parent block.
            if epochTransitionTriggered seedState
                then checkLeader bakers (nonceForNewEpoch bakers seedState)
                else -- If the transition is not triggered, then the child block should not be
                -- in the new epoch.
                    return BlockResultInvalid
        | otherwise =
            -- The block's epoch is not valid.
            return BlockResultInvalid
    checkLeader bakers leNonce
        | let roundBaker = getLeaderFullBakers bakers leNonce (blockRound pendingBlock),
          blockBaker pendingBlock == roundBaker ^. bakerIdentity = do
            let verifiedBlock =
                    VerifiedBlock
                        { vbBlock = pendingBlock,
                          vbBakers = bakers,
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
-- * TODO: if any of the transactions in the block are invalid.
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
            gets (doGetBakersForEpoch (blockEpoch pendingBlock)) >>= \case
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

-- |Process a received block. This handles the processing after the initial checks and after the
-- block has been relayed (or not). This DOES NOT include signing the block as a finalizer.
--
-- Precondition:
--
-- * The parent block is correct: @getHash parent == blockParent pendingBlock@.
-- * The block is signed by a valid baker for its epoch.
-- * The baker is the leader for the round according to the parent block.
processBlock ::
    ( IsConsensusV1 (MPV m),
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadIO m
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
        genConfig <- use genesisMetadata
        checkTimestamp $
            checkTimeoutCertificatePresentAndCorrectRound $ \mTimeoutCert ->
                getParentBakersAndFinalizers $ \parentBF ->
                    checkEpochFinalizationEntry genConfig parentBF $
                        checkQC parentBF $
                            undefined
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
    checkTimeoutCertificatePresentAndCorrectRound continue
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
                    | otherwise -> continue (Just tc)
        | otherwise = continue Nothing
    -- Check the finalization entry
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
                        -- the epoch. We use the seed state for the parent block, because that is in
                        -- the same epoch.
                        parentSeedState <- getSeedState (bpState parent)
                        -- Get the status of the block finalized by the finalization entry for the
                        -- check.
                        get >>= getBlockStatus (qcBlock (feFinalizedQuorumCertificate finEntry)) >>= \case
                            BlockAliveOrFinalized finBlock
                                | blockTimestamp finBlock >= triggerBlockTime parentSeedState ->
                                    continue
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
    getParentBakersAndFinalizers continue =
        gets (doGetBakersForEpoch (blockEpoch parent)) >>= \case
            -- If this case happens, the parent block must now precede the last finalized block,
            -- so we can no longer add the block.
            Nothing -> return False
            Just bf -> continue bf
    checkQC BakersAndFinalizers{..} continue = do
        GenesisMetadata{..} <- use genesisMetadata
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

executeBlock ::
    ( IsConsensusV1 (MPV m),
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadIO m
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
            -- TODO: If we're a finalizer, wait for the timestamp and then validate the block.
            return ()
