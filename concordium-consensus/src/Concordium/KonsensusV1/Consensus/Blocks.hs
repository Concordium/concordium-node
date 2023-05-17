{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines functionality for receiving, validating and producing blocks.
module Concordium.KonsensusV1.Consensus.Blocks where

import Control.Applicative
import Control.Exception
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Function
import Data.Ord
import Data.Time
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Utils

import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus hiding (bakerIdentity)
import qualified Concordium.KonsensusV1.Consensus as Consensus
import Concordium.KonsensusV1.Consensus.Finality
import Concordium.KonsensusV1.Consensus.Quorum
import Concordium.KonsensusV1.Consensus.Timeout.Internal
import Concordium.KonsensusV1.Flag
import Concordium.KonsensusV1.LeaderElection
import Concordium.KonsensusV1.Scheduler
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimerMonad
import Concordium.Types.BakerIdentity

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

-- * Receiving blocks

-- |The result type for 'uponReceivingBlock'.
data BlockResult
    = -- |The block was successfully received, but not yet executed.
      BlockResultSuccess !VerifiedBlock
    | -- |The baker also signed another block in the same round, but the block was otherwise
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

-- |Handle initial verification of a block before it is relayed to peers.
-- If the result is @'BlockResultSuccess' vb@, the block should be relayed on the network and
-- @'executeBlock' vb@ should be called. If the result is @'BlockResultDoubleSign' vb@, the block
-- should not be relayed on the network, but @'executeBlock' vb@ should still be called.
-- For any other result, the block should not be relayed, nor 'executeBlock' be called.
--
-- With reference to the bluepaper, this implements **uponReceivingBlock** up to the point at which
-- the block is relayed. The caller is responsible for relaying the block and invoking
-- 'executeBlock' to complete the procedure (as necessary).
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
    -- TODO: Check for consensus shutdown. Issue #825
    lfb <- use lastFinalized
    if blockEpoch pendingBlock < blockEpoch lfb || blockRound pendingBlock <= blockRound lfb
        then do
            -- The block is from an old epoch, or already finalized round
            logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " is from an old round or epoch."
            return BlockResultStale
        else do
            sd <- get
            -- Check that the block is not already live or pending. The network-layer deduplication
            -- should generally prevent such blocks from getting here, but having this check means
            -- we can rely on the fact.
            case getMemoryBlockStatus pbHash sd of
                Just _ -> do
                    logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " is a duplicate."
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
        logEvent Konsensus LLTrace $
            "Block "
                <> show pbHash
                <> " has an invalid (dead or old finalized) parent ("
                <> show (blockParent pendingBlock)
                <> ")."
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
    logEvent Konsensus LLInfo $ "Block " <> show pbHash <> " received."
    let nominalTime = timestampToUTCTime $ blockTimestamp pendingBlock
    statistics %=! updateStatsOnReceive nominalTime (pbReceiveTime pendingBlock)
    genesisHash <- use currentGenesisHash
    getBakers >>= \case
        Nothing -> do
            logEvent Konsensus LLTrace $
                "Block "
                    <> show pbHash
                    <> " is for an invalid epoch ("
                    <> show (blockEpoch pendingBlock)
                    <> ")."
            return BlockResultInvalid
        Just bakersAndFinalizers
            -- We know the bakers
            | Just baker <- fullBaker (bakersAndFinalizers ^. bfBakers) (blockBaker pendingBlock),
              verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                -- The signature is valid
                receiveSigned bakersAndFinalizers
            | otherwise -> do
                -- The signature is invalid
                logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " has an invalid signature."
                return BlockResultInvalid
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock
    -- Since the parent block is live, its epoch is either currentEpoch or (currentEpoch - 1).
    -- [This follows from the invariants on what the current epoch can be.]
    -- For the new block's epoch to be valid, it must either be in the same epoch as the parent
    -- block or in the next epoch. Thus, the block's epoch is one of (currentEpoch - 1),
    -- currentEpoch, or (currentEpoch + 1). Either currentEpoch == lastFinalizedEpoch, or
    -- currentEpoch == (lastFinalizedEpoch + 1). 'getBakersForEpoch' will return the bakers in all
    -- cases except where the block's epoch is (lastFinalizedEpoch + 2).
    getBakers =
        gets (getBakersForEpoch (blockEpoch pendingBlock)) >>= \case
            Nothing
                | blockEpoch pendingBlock == blockEpoch parent + 1 ->
                    Just <$> getNextEpochBakersAndFinalizers (bpState parent)
            res -> return res
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
                    logEvent Konsensus LLTrace $
                        "Block "
                            <> show pbHash
                            <> " contains epoch finalization entry for a block ("
                            <> show (blockEpoch pendingBlock)
                            <> ") that is not part of the chain."
                    return BlockResultInvalid
        | otherwise = do
            -- The block's epoch is not valid.
            logEvent Konsensus LLTrace $
                "Block "
                    <> show pbHash
                    <> " is for an invalid epoch ("
                    <> show (blockEpoch pendingBlock)
                    <> ")."
            flag $ BlockEpochInconsistent $ pbBlock pendingBlock
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
                    logEvent Konsensus LLDebug $
                        "Baker "
                            <> show (blockBaker pendingBlock)
                            <> " signed multiple blocks in round "
                            <> show (blockRound pendingBlock)
                            <> "."
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
            logEvent Konsensus LLTrace $
                "Block "
                    <> show pbHash
                    <> " is from baker "
                    <> show (blockBaker pendingBlock)
                    <> " but the round winner is baker "
                    <> show (roundBaker ^. bakerIdentity)
                    <> "."
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
-- * The timestamp is no less than the receive time of the block + the early block threshold, or
--   we do not know the set of bakers for the block's epoch.
--   (Returns 'BlockResultEarly'.)
--
-- * The bakers for the block's epoch are known and either:
--
--     - the baker is not a valid baker for the epoch; or
--     - the baker is valid but the signature on the block is not valid.
--
-- TODO: if any of the transactions in the block are invalid. Issue #699
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
            gets (getBakersForEpoch (blockEpoch pendingBlock)) >>= \case
                Nothing -> do
                    -- We do not know the bakers, so we treat this like an early block.
                    return BlockResultEarly
                Just bf -- We know the bakers
                    | Just baker <- fullBaker (bf ^. bfBakers) (blockBaker pendingBlock),
                      verifyBlockSignature (baker ^. bakerSignatureVerifyKey) genesisHash pendingBlock ->
                        -- The signature is valid
                        continuePending
                    | otherwise -> do
                        -- The signature is invalid.
                        -- Note: we do not mark the block dead, because potentially a valid block
                        -- with the same hash exists.
                        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " has an invalid signature."
                        return BlockResultInvalid
        else return BlockResultEarly
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock
    continuePending = do
        -- TODO: Check the transactions in the block. Issue #699
        addPendingBlock pendingBlock
        markPending pendingBlock
        logEvent Konsensus LLInfo $ "Block " <> show pbHash <> " is pending its parent " <> show (blockParent pendingBlock) <> "."
        return BlockResultPending

-- |Get the minimum time between consecutive blocks, as of the specified block.
-- This is the value of the minimum block time chain parameter, and determines the minimum interval
-- between the block and a child block.
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
--  * Invokes the 'onBlock' event handler provided by 'MonadConsensusEvent'.
--
-- Preconditions:
--
--  * The block's parent must be the last finalized block or another live block.
--
--  * The block must not already be a live block.
addBlock ::
    (TimeMonad m, MonadState (SkovData (MPV m)) m, MonadConsensusEvent m, MonadLogger m) =>
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
    logEvent Konsensus LLInfo $
        "Block "
            <> show pbHash
            <> " arrived at "
            <> show now
            <> ". Processed in "
            <> show (diffUTCTime now (pbReceiveTime pendingBlock))
            <> "."
    let nominalTime = timestampToUTCTime $ blockTimestamp newBlock
        numTransactions = blockTransactionCount newBlock
    statistics %=! updateStatsOnArrive nominalTime now numTransactions
    onBlock newBlock
    return newBlock
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock

-- |Process a received block. This handles the processing after the initial checks and after the
-- block has been relayed (or not). This DOES NOT include processing pending children of the block
-- or signing the block as a finalizer.
--
-- If the block is executed successfully, it is added to the tree and returned.
-- Otherwise, 'Nothing' is returned.
--
-- The last finalized block may be updated as a result of the processing. In particular, if the
-- block is the first in a new epoch and contains a valid epoch finalization entry, the block
-- finalized by that entry will be finalized (if it was not already). Furthermore, if the block's QC
-- allows for the grandparent block to be finalized, then that will be finalized. (Note: both of
-- these do not happen for the same block, since the former requires the block to be in a different
-- epoch from its parent, and the latter requires it to be in the same epoch as its parent.)
--
-- Precondition:
--
-- * The parent block is correct: @getHash parent == blockParent pendingBlock@.
-- * The block is signed by a valid baker for its epoch.
-- * The baker is the leader for the round according to the parent block.
--
-- TODO: Transaction processing. Issue #699.
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
      MonadConsensusEvent m,
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
        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " QC round does not match parent block's round."
        flag $ BlockQCRoundInconsistent sBlock
        rejectBlock
    -- Check that the QC is consistent with the parent block epoch.
    | qcEpoch (blockQuorumCertificate pendingBlock) /= blockEpoch parent = do
        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " QC epoch does not match parent block's epoch."
        flag $ BlockQCEpochInconsistent sBlock
        rejectBlock
    -- Check that the block round is greater than the round of the parent block.
    | blockRound parent >= blockRound pendingBlock = do
        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " round is not higher than parent block's round."
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
        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " epoch is not the same or next epoch relative to parent."
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
            logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " nonce is incorrect."
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
                                checkFinality (blockQuorumCertificate pendingBlock)
                                curRound <- use $ roundStatus . rsCurrentRound
                                let certifiedParent =
                                        CertifiedBlock
                                            { cbQuorumCertificate = blockQuorumCertificate pendingBlock,
                                              cbQuorumBlock = parent
                                            }
                                if curRound < blockRound pendingBlock
                                    then case blockTimeoutCertificate pendingBlock of
                                        Present tc ->
                                            advanceRoundWithTimeout
                                                RoundTimeout
                                                    { rtTimeoutCertificate = tc,
                                                      rtCertifiedBlock = certifiedParent
                                                    }
                                        Absent ->
                                            advanceRoundWithQuorum
                                                certifiedParent
                                    else checkedUpdateHighestCertifiedBlock certifiedParent
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
                logEvent Konsensus LLTrace $
                    "Block "
                        ++ show pbHash
                        ++ " timestamp ("
                        ++ show (blockTimestamp pendingBlock)
                        ++ ") is less than minBlockTime ("
                        ++ show minBlockTime
                        ++ ") after parent timestamp ("
                        ++ show (blockTimestamp parent)
                        ++ ")."
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
                    logEvent Konsensus LLTrace $
                        "Block "
                            <> show pbHash
                            <> " is missing a timeout certificate, but is not in the next round from its parent."
                    flag $ BlockTCMissing sBlock
                    rejectBlock
                Present tc
                    -- Check that the TC round is correct.
                    | tcRound tc /= blockRound pendingBlock - 1 -> do
                        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " timeout certificate is not for the correct round."
                        flag $ BlockTCRoundInconsistent sBlock
                        rejectBlock
                    | blockRound parent < tcMaxRound tc
                        || blockEpoch parent < tcMaxEpoch tc -> do
                        logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " timeout certificate is inconsistent with the quorum certificate."
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
                        -- Given that the parent is live and descended from the LFB, the parent is
                        -- at most one epoch later than the LFB, and so the tcMaxEpoch is also.
                        eBkrs <- use epochBakers
                        let checkTCValid eb1 eb2 =
                                checkTimeoutCertificate
                                    gmCurrentGenesisHash
                                    (toRational $ genesisSignatureThreshold gmParameters)
                                    (eb1 ^. bfFinalizers)
                                    (eb2 ^. bfFinalizers)
                                    (parentBF ^. bfFinalizers)
                                    tc
                        lastFinEpoch <- use $ lastFinalized . to blockEpoch
                        let tcOK
                                | tcMinEpoch tc == lastFinEpoch - 1 =
                                    checkTCValid
                                        (eBkrs ^. previousEpochBakers)
                                        (eBkrs ^. currentEpochBakers)
                                | tcMinEpoch tc == lastFinEpoch =
                                    checkTCValid
                                        (eBkrs ^. currentEpochBakers)
                                        (eBkrs ^. nextEpochBakers)
                                -- Note that we do not check that @tcIsSingleEpoch tc@ here,
                                -- since @tcMinEpoch tc == lastFinEpoch + 1@ implies that
                                -- @tcMaxEpoch tc == lastFinEpoch + 2@ which would be
                                -- rejected by the test above @blockEpoch parent <= tcMaxEpoch tc@.
                                | tcMinEpoch tc == lastFinEpoch + 1 =
                                    checkTCValid
                                        (eBkrs ^. nextEpochBakers)
                                        -- Because the TC is for a single epoch, the second set of
                                        -- bakers will not be used in the validity check.
                                        (eBkrs ^. nextEpochBakers)
                                | otherwise = False
                        if tcOK
                            then continue
                            else do
                                logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " timeout certificate is invalid."
                                flag $ BlockInvalidTC sBlock
                                rejectBlock
        -- If the previous round didn't timeout, check we have no timeout certificate
        | Present _ <- blockTimeoutCertificate pendingBlock = do
            logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " has unexpected timeout certificate."
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
                    logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " missing an epoch finalization entry."
                    flag $ BlockEpochFinalizationMissing sBlock
                    rejectBlock
                Present finEntry
                    -- Check that the finalization entry is for the correct epoch,
                    -- that the higher QC round is at most the round of the parent block, and
                    -- contains valid QCs.
                    | qcEpoch (feFinalizedQuorumCertificate finEntry) == blockEpoch parent,
                      qcRound (feSuccessorQuorumCertificate finEntry) <= blockRound parent,
                      checkFinalizationEntry
                        gmCurrentGenesisHash
                        (toRational $ genesisSignatureThreshold gmParameters)
                        _bfFinalizers
                        finEntry -> do
                        -- We record that we have checked a valid QC for the successor round.
                        -- We do not record the finalized round, because we expect it to be
                        -- finalized, and we only keep entries for non-finalized rounds.
                        recordCheckedQuorumCertificate (feSuccessorQuorumCertificate finEntry)
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
                                    -- Note: we do not update the highest certified block here
                                    -- because the parent block will be at least as high as that
                                    -- in the successor QC. (Also, we may not have the block
                                    -- pointed to by the successor QC.)
                                    --
                                    -- Note that we have checked that @qcRound (feSuccessorQuorumCertificate finEntry) <= blockRound parent@
                                    -- so either the @parent@ is the block that finalized @finBlock@ or @finBlock@ was already finalized
                                    -- when @parent@ was baked.
                                    -- Hence, at this point we MUST know the @parent@ so there is no need to look it up,
                                    -- so we just @continue@.
                                    -- If this is not the case, then there must've been more than 1/3 dishonest finalizers
                                    -- and as a result conflicting blocks were finalized.
                                    continue
                            _ -> do
                                logEvent Konsensus LLTrace $
                                    "Block "
                                        <> show pbHash
                                        <> " contains epoch finalization entry for a block ("
                                        <> show finBlockHash
                                        <> ") that is not part of the chain."
                                flag $ BlockInvalidEpochFinalization sBlock
                                rejectBlock
                    | otherwise -> do
                        logEvent Konsensus LLTrace $
                            "Block "
                                <> show pbHash
                                <> " contains an invalid epoch finalization entry."
                        flag $ BlockInvalidEpochFinalization sBlock
                        rejectBlock
        -- Here, the epoch must be the same as the parent epoch by the earlier epoch consistency
        -- check, and so we require the epoch finalization entry to be absent.
        | Present _ <- blockEpochFinalizationEntry pendingBlock = do
            logEvent Konsensus LLTrace $
                "Block "
                    <> show pbHash
                    <> " contains an epoch finalization entry when it should not."
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
                logEvent Konsensus LLTrace $
                    "Block "
                        <> show pbHash
                        <> " failed execution: "
                        <> show failureReason
                flag (BlockExecutionFailure sBlock)
                rejectBlock
            Right newState -> do
                outcomesHash <- getTransactionOutcomesHash newState
                if
                        | outcomesHash /= blockTransactionOutcomesHash pendingBlock -> do
                            -- Incorrect transaction outcomes
                            logEvent Konsensus LLTrace $
                                "Block "
                                    <> show pbHash
                                    <> " stated transaction outcome hash ("
                                    <> show (blockTransactionOutcomesHash pendingBlock)
                                    <> ") does not match computed value ("
                                    <> show outcomesHash
                                    <> ")."
                            flag $ BlockInvalidTransactionOutcomesHash sBlock (bpBlock parent)
                            rejectBlock
                        | getHash newState /= blockStateHash pendingBlock -> do
                            -- Incorrect state hash
                            logEvent Konsensus LLTrace $
                                "Block "
                                    <> show pbHash
                                    <> " stated state hash ("
                                    <> show (blockStateHash pendingBlock)
                                    <> ") does not match computed value ("
                                    <> show (getHash newState :: StateHash)
                                    <> ")."
                            flag $ BlockInvalidStateHash sBlock (bpBlock parent)
                            rejectBlock
                        | otherwise ->
                            continue newState
    getParentBakersAndFinalizers continue
        | blockEpoch parent == blockEpoch pendingBlock = continue vbBakersAndFinalizers
        | otherwise =
            gets (getBakersForEpoch (blockEpoch parent)) >>= \case
                -- If this case happens, the parent block must now precede the last finalized block,
                -- so we can no longer add the block.
                Nothing -> rejectBlock
                Just bf -> continue bf
    checkQC GenesisMetadata{..} BakersAndFinalizers{..} continue = do
        let qc = blockQuorumCertificate pendingBlock
        let qcOK =
                checkQuorumCertificate
                    gmCurrentGenesisHash
                    (toRational $ genesisSignatureThreshold gmParameters)
                    _bfFinalizers
                    qc
        if qcOK
            then do
                recordCheckedQuorumCertificate qc
                continue
            else do
                logEvent Konsensus LLTrace $ "Block " <> show pbHash <> " contains an invalid QC."
                flag $ BlockInvalidQC sBlock
                rejectBlock

-- |A 'BlockPointer', ordered lexicographically on:
--
-- * Round number
-- * Epoch number
-- * Descending timestamp
-- * Block hash
--
-- This ordering is used to determine the "best block" to sign when a block arrives.
newtype OrderedBlock pv = OrderedBlock {theOrderedBlock :: BlockPointer pv}

instance Ord (OrderedBlock pv) where
    compare =
        compare `on` toTuple
      where
        toTuple (OrderedBlock blk) =
            ( blockRound blk,
              blockEpoch blk,
              Down (blockTimestamp blk),
              getHash @BlockHash blk
            )

instance Eq (OrderedBlock pv) where
    a == b = compare a b == EQ

-- |Process the pending children of a block that has just become live.
-- A pending child block either becomes alive or dead after it is processed.
-- Pending children are processed recursively (i.e. pending children of pending children are also
-- processed, etc.).
-- The returned block is the best resulting live block among the supplied block and any pending
-- children that become live. ("Best" here means the greatest with respect to the order on
-- 'OrderedBlock'.)
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
      MonadLogger m,
      MonadConsensusEvent m
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
--   - The block's signature has been verified. (This is the case for blocks that are marked as
--     pending.)
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
      MonadLogger m,
      MonadConsensusEvent m
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
                        processReceiveOK True parent vb
                    BlockResultDoubleSign vb -> do
                        processReceiveOK False parent vb
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
    processReceiveOK advertise parent vb = do
        processBlock parent vb >>= \case
            Just newBlock -> do
                -- We only advertise the block to our peers
                -- if it was received succesfully. Hence if peers are catching up with us
                -- they are getting the blocks in the same order as we are.
                when advertise onPendingLive
                Just <$> processPendingChildren newBlock
            Nothing -> processAsDead

-- |Produce a quorum signature on a block.
-- This checks that the block is still valid, is in the current round and epoch, and the
-- round is signable (i.e. we haven't already signed a quorum message or timeout message).
--
-- It is assumed that the supplied 'FinalizerInfo' accurately represents the finalizer in the
-- epoch of the block, and that the baker identity matches the finalizer info (i.e. the keys
-- are correct).
validateBlock ::
    ( MonadState (SkovData (MPV m)) m,
      MonadBroadcast m,
      LowLevel.MonadTreeStateStore m,
      IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadTimeout m,
      MonadConsensusEvent m,
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
        persistentRS <- use persistentRoundStatus
        curRound <- use $ roundStatus . rsCurrentRound
        curEpoch <- use $ roundStatus . rsCurrentEpoch
        when
            ( blockRound block == curRound
                && prsNextSignableRound persistentRS <= blockRound block
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
                setPersistentRoundStatus $!
                    persistentRS
                        { _prsLastSignedQuorumMessage = Present quorumMessage
                        }
                sendQuorumMessage quorumMessage
                processQuorumMessage $
                    VerifiedQuorumMessage
                        { vqmMessage = quorumMessage,
                          vqmFinalizerWeight = finalizerWeight finInfo,
                          vqmBlock = block
                        }

-- |If the given time has elapsed, perform the supplied action. Otherwise, start a timer to
-- asynchronously perform the action at the given time.
doAfter :: (TimeMonad m, TimerMonad m) => UTCTime -> m () -> m ()
doAfter time action = do
    now <- currentTime
    if time <= now
        then action
        else void $ onTimeout (DelayUntil time) action

-- |Produce a quorum signature on a block if the block is eligible and we are a finalizer for the
-- block's epoch. This will delay until the timestamp of the block has elapsed so that we do not
-- sign blocks prematurely.
checkedValidateBlock ::
    ( MonadReader r m,
      HasBakerContext r,
      BlockData b,
      HashableTo BlockHash b,
      TimerMonad m,
      MonadState (SkovData (MPV m)) m,
      MonadBroadcast m,
      LowLevel.MonadTreeStateStore m,
      MonadThrow m,
      MonadIO m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m,
      IsConsensusV1 (MPV m)
    ) =>
    -- |Block to (potentially) sign.
    b ->
    m ()
checkedValidateBlock validBlock = do
    withFinalizerForEpoch (blockEpoch validBlock) $ \bakerIdent finInfo -> do
        let !blockHash = getHash validBlock
        doAfter (timestampToUTCTime $ blockTimestamp validBlock) $!
            validateBlock blockHash bakerIdent finInfo

-- |Execute a block that has previously been verified by 'uponReceivingBlock'.
--
-- This should be robust against other consensus operations occurring in between
-- 'uponReceivingBlock' and 'executeBlock'. However, in practise it is recommended that
-- 'executeBlock' should be called immediately, without releasing the lock on the consensus
-- state.
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
      MonadBroadcast m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    VerifiedBlock ->
    m ()
executeBlock verifiedBlock = do
    -- TODO: check for consensus shutdown. Issue #825
    gets (getLiveOrLastFinalizedBlock (blockParent (vbBlock verifiedBlock))) >>= \case
        Just parent -> do
            res <- processBlock parent verifiedBlock
            forM_ res $ \newBlock -> do
                OrderedBlock best <- processPendingChildren newBlock
                checkedValidateBlock best
        Nothing -> return ()

-- * Block production

-- |Inputs used for baking a new block.
data BakeBlockInputs (pv :: ProtocolVersion) = BakeBlockInputs
    { -- |Secret keys for the baker.
      bbiBakerIdentity :: BakerIdentity,
      -- |Round in which the block is to be produced.
      bbiRound :: Round,
      -- |Epoch in which the block is to be produced.
      -- Should always be either @blockEpoch bbiParent@ or @1 + blockEpoch bbiParent@.
      bbiEpoch :: Epoch,
      -- |Parent block.
      bbiParent :: BlockPointer pv,
      -- |A valid quorum certificate for the parent block.
      bbiQuorumCertificate :: QuorumCertificate,
      -- |If the parent block belongs to a round earlier than @bbiRound - 1@, this is a valid
      -- timeout certificate for round @bbiRound - 1@ in the same epoch as the parent block.
      -- Otherwise, it is 'Absent'.
      bbiTimeoutCertificate :: Option TimeoutCertificate,
      -- |If the block is to be the first in a new epoch (i.e. @bbiEpoch@ is
      -- @blockEpoch bbiParent + 1@) then this is a valid finalization entry for a block after the
      -- trigger time in epoch @blockEpoch bbiParent@.
      bbiEpochFinalizationEntry :: Option FinalizationEntry,
      -- |The set of bakers for the epoch 'bbiEpoch'.
      bbiEpochBakers :: FullBakers,
      -- |The leadership election nonce used in the election.
      bbiLeadershipElectionNonce :: LeadershipElectionNonce
    }

-- |Determine if the node is a baker and eligible to bake in the current round, and produce
-- 'BakeBlockInputs' if so. This checks:
--
--   * We have not already attempted to bake for this round.
--
--   * We have not baked for a more recent round.
--
--   * We have baker credentials.
--
--   * We are the winner of the round in the current epoch.
--
--   * The baker's public keys match those in the baking committee.
--
-- Note, if any of the tests fails, it is expected to fail on subsequent calls in the same round.
-- (In particular, we do not expect the baker keys to change.) Thus, when 'prepareBakeBlockInputs'
-- is called, it marks that we have attempted to bake for the round, so subsequent calls will fail
-- until the round is advanced (and so the flag is reset).
prepareBakeBlockInputs ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      MonadLogger m
    ) =>
    m (Maybe (BakeBlockInputs (MPV m)))
prepareBakeBlockInputs = runMaybeT $ do
    -- We directly set the @rsRoundEligibleToBake@ to 'False' here as
    -- even if the function returns early without producing the inputs required
    -- for baking a block then such a reason is not recoverable.
    -- This function can fail in the following ways:
    -- - The baker is configured with wrong keys and hence the
    --   node has to be restarted and configured with the correct keys,
    --   but it would not really be possible anyhow to bake in this round for the baker
    --   as the chain would most likely have advanced the round in the time it took to reboot the node.
    -- - The baker is not leader for the current round or the baker is trying to bake for an old rould.
    --   The only way to progress from such a failure is to advance the round anyhow
    --   (which makes it eligible for baking again).
    canBake <- roundStatus . rsRoundEligibleToBake <<.= False
    -- Terminate if we've already tried to bake for this round.
    guard canBake
    sd <- get
    let rs = sd ^. roundStatus
    -- Note: we rely on the current round being always in sync with the previous round TC/
    -- highest QC.
    let bbiRound = rs ^. rsCurrentRound
    -- Check that we haven't baked for this or a more recent round.
    guard (bbiRound > sd ^. persistentRoundStatus . prsLastBakedRound)
    -- Terminate if we do not have baker credentials.
    bbiBakerIdentity@BakerIdentity{..} <- MaybeT (view Consensus.bakerIdentity)
    -- Determine the parent block and its quorum certificate, as well as the timeout certificate if
    -- applicable.
    let ( bbiTimeoutCertificate,
          CertifiedBlock
            { cbQuorumBlock = bbiParent,
              cbQuorumCertificate = bbiQuorumCertificate
            }
            )
                | Present RoundTimeout{..} <- rs ^. rsPreviousRoundTimeout =
                    ( Present rtTimeoutCertificate,
                      if cbEpoch highestCB > cbEpoch rtCertifiedBlock
                        then rtCertifiedBlock
                        else highestCB
                    )
                | otherwise =
                    ( Absent,
                      highestCB
                    )
              where
                highestCB = rs ^. rsHighestCertifiedBlock
    let bbiEpoch = sd ^. roundStatus . rsCurrentEpoch
    let bbiEpochFinalizationEntry
            | bbiEpoch > qcEpoch bbiQuorumCertificate =
                -- This assertion should not fail because the invariant on
                -- '_lastEpochFinalizationEntry' requires the entry to be present whenever
                -- @_currentEpoch > blockEpoch _lastFinalized@, and @bbiQuorumCertificate@ is for
                -- the parent block which is in at least the epoch of the last finalized block.
                assert (isPresent finEntry) finEntry
            | otherwise = Absent
          where
            finEntry = sd ^. roundStatus . rsLastEpochFinalizationEntry
    bbiEpochBakers <-
        if isAbsent bbiEpochFinalizationEntry
            then getCurrentEpochBakers (bpState bbiParent)
            else getNextEpochBakers (bpState bbiParent)
    parentSeedState <- getSeedState (bpState bbiParent)
    let bbiLeadershipElectionNonce =
            if isAbsent bbiEpochFinalizationEntry
                then parentSeedState ^. currentLeadershipElectionNonce
                else nonceForNewEpoch bbiEpochBakers parentSeedState
    let leader = getLeaderFullBakers bbiEpochBakers bbiLeadershipElectionNonce bbiRound
    guard (leader ^. bakerIdentity == bakerId)
    unless (bakerSignPublicKey bbiBakerIdentity == leader ^. bakerSignatureVerifyKey) $ do
        logEvent Konsensus LLWarning "Baker signing key does not match the key in the current committee."
        empty
    unless (bakerElectionPublicKey bbiBakerIdentity == leader ^. bakerElectionVerifyKey) $ do
        logEvent Konsensus LLWarning "Baker election key does not match the key in the current committee."
        empty
    return BakeBlockInputs{..}

-- |Construct a block given 'BakeBlockInputs'.
bakeBlock ::
    ( MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    BakeBlockInputs (MPV m) ->
    m SignedBlock
bakeBlock BakeBlockInputs{..} = do
    curTimestamp <- utcTimeToTimestamp <$> currentTime
    minBlockTime <- getMinBlockTime bbiParent
    let bbTimestamp = max curTimestamp (addDuration (blockTimestamp bbiParent) minBlockTime)
    epochDuration <- use $ genesisMetadata . to (genesisEpochDuration . gmParameters)
    let bbNonce =
            computeBlockNonce
                bbiLeadershipElectionNonce
                bbiRound
                (bakerElectionKey bbiBakerIdentity)
    let executionData =
            BlockExecutionData
                { bedIsNewEpoch = isPresent bbiEpochFinalizationEntry,
                  bedEpochDuration = epochDuration,
                  bedTimestamp = bbTimestamp,
                  bedBlockNonce = bbNonce,
                  bedParentState = bpState bbiParent
                }
    -- TODO: When the scheduler is integrated, this will be changed. Issue #699
    executeBlockStateUpdate executionData >>= \case
        Left err -> case err of {}
        Right newState -> do
            bbTransactionOutcomesHash <- getTransactionOutcomesHash newState
            bbStateHash <- getStateHash newState
            let bakedBlock =
                    BakedBlock
                        { bbRound = bbiRound,
                          bbEpoch = bbiEpoch,
                          bbBaker = bakerId bbiBakerIdentity,
                          bbQuorumCertificate = bbiQuorumCertificate,
                          bbTimeoutCertificate = bbiTimeoutCertificate,
                          bbEpochFinalizationEntry = bbiEpochFinalizationEntry,
                          bbTransactions = mempty,
                          ..
                        }
            genesisHash <- use currentGenesisHash
            let signedBlock = signBlock (bakerSignKey bbiBakerIdentity) genesisHash bakedBlock
            updatePersistentRoundStatus $ prsLastBakedRound .~ bbiRound
            now <- currentTime
            let nominalTime = timestampToUTCTime bbTimestamp
            statistics %=! updateStatsOnReceive nominalTime now
            _ <-
                addBlock
                    PendingBlock{pbBlock = signedBlock, pbReceiveTime = now}
                    newState
                    bbiParent
            return signedBlock

-- |Try to make a block, distribute it on the network and sign it as a finalizer.
-- This function should be called after any operation that can advance the current round to
-- attempt block production. A block will only be produced if we have credentials, are the
-- winner of the round in the current epoch, and have not already tried to produce a block in the
-- round.
--
-- Note: We will only attempt to bake a block once for a round. It is possible (if unlikely) that
-- we might enter a round and subsequently advance epoch, while remaining in the same round.
-- It could be that we did not win the round in the old epoch, but do in the new one. In such a
-- case, it would be reasonable to produce a block for the round in the new epoch, but we do not.
-- The circumstances creating this scenario are sufficiently rare that it shouldn't be a problem to
-- allow the round to time out.
makeBlock ::
    ( MonadReader r m,
      HasBakerContext r,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      TimeMonad m,
      TimerMonad m,
      MonadBroadcast m,
      MonadThrow m,
      MonadIO m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    m ()
makeBlock = do
    mInputs <- prepareBakeBlockInputs
    forM_ mInputs $ \inputs -> do
        block <- bakeBlock inputs
        doAfter (timestampToUTCTime $ blockTimestamp block) $ do
            sendBlock block
            checkedValidateBlock block
