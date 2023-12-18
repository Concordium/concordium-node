{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Vector as Vector
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.BakerIdentity
import Concordium.Types.Block (localToAbsoluteBlockHeight)
import Concordium.Types.HashableTo
import Concordium.Types.Option
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Utils

import Concordium.Genesis.Data.BaseV1
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.PurgeTransactions
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
import Concordium.KonsensusV1.Transactions
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Scheduler (FilteredTransactions (..))
import Concordium.TimerMonad

-- | A block that has passed initial verification, but must still be executed, added to the state,
--  and (potentially) signed as a finalizer.
data VerifiedBlock (pv :: ProtocolVersion) = VerifiedBlock
    { -- | The block that has passed initial verification.
      vbBlock :: !(PendingBlock pv),
      -- | The bakers and finalizers for the epoch of this block.
      vbBakersAndFinalizers :: !BakersAndFinalizers,
      -- | The baker info for the block's own baker.
      vbBakerInfo :: !FullBakerInfo,
      -- | The leadership election nonce for the block's epoch.
      vbLeadershipElectionNonce :: !LeadershipElectionNonce
    }
    deriving (Eq, Show)

instance BlockData (VerifiedBlock pv) where
    type BakedBlockDataType (VerifiedBlock pv) = SignedBlock pv
    blockRound = blockRound . vbBlock
    blockEpoch = blockEpoch . vbBlock
    blockTimestamp = blockTimestamp . vbBlock
    blockBakedData = blockBakedData . vbBlock
    blockTransactions = blockTransactions . vbBlock
    blockTransactionCount = blockTransactionCount . vbBlock

-- * Receiving blocks

-- | The result type for 'uponReceivingBlock'.
data BlockResult pv
    = -- | The block was successfully received, but not yet executed.
      BlockResultSuccess !(VerifiedBlock pv)
    | -- | The baker also signed another block in the same round, but the block was otherwise
      --  successfully received, but not yet executed.
      BlockResultDoubleSign !(VerifiedBlock pv)
    | -- | The block contains data that is not valid with respect to the chain.
      BlockResultInvalid
    | -- | The block is too old to be added to the chain.
      BlockResultStale
    | -- | The block is pending its parent.
      BlockResultPending
    | -- | The timestamp of this block is too early.
      BlockResultEarly
    | -- | We have already seen this block.
      BlockResultDuplicate
    | -- | Consensus has been shutdown.
      BlockResultConsensusShutdown
    deriving (Eq, Show)

-- | Handle initial verification of a block before it is relayed to peers.
--  If the result is @'BlockResultSuccess' vb@, the block should be relayed on the network and
--  @'executeBlock' vb@ should be called. If the result is @'BlockResultDoubleSign' vb@, the block
--  should not be relayed on the network, but @'executeBlock' vb@ should still be called.
--  For any other result, the block should not be relayed, nor 'executeBlock' be called.
--
--  With reference to the bluepaper, this implements **uponReceivingBlock** up to the point at which
--  the block is relayed. The caller is responsible for relaying the block and invoking
--  'executeBlock' to complete the procedure (as necessary).
uponReceivingBlock ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadLogger m
    ) =>
    PendingBlock (MPV m) ->
    m (BlockResult (MPV m))
uponReceivingBlock pendingBlock = do
    isShutdown <- use isConsensusShutdown
    if isShutdown
        then return BlockResultConsensusShutdown
        else do
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

-- | Process receiving a block where the parent is live (i.e. descended from the last finalized
--  block).  If this function returns 'BlockResultSuccess' then the caller is obliged to call
--  'executeBlock' on the returned 'VerifiedBlock' in order to complete the processing of the block,
--  after relaying the block to peers.
--
--  The result can be one of the following:
--
--  * @'BlockResultSuccess' vb@: the block has a valid signature and is baked by the correct leader
--    in the block's round. (Note, this implies that the block is in the same or the next epoch from
--    its parent, since we can only determine the leadership election nonce if one of these is the
--    case.) The block is not yet added to the state, except that its existence is recorded in the
--    'roundExistingBlocks' index for checking double signing. The block must be subsequently
--    processed by calling @'executeBlock' vb@
--
--  * @'BlockResultDoubleSign' vb@: the block is valid, but the result of double signing.
--    As with 'BlockResultSuccess', the block is not yet added to the state and must be subsequently
--    processed by calling @'executeBlock' vb@.
--
--  * 'BlockResultInvalid': the epoch is invalid, the signature is invalid, or the baker is not the
--    round leader.
receiveBlockKnownParent ::
    ( IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadLogger m
    ) =>
    BlockPointer (MPV m) ->
    PendingBlock (MPV m) ->
    m (BlockResult (MPV m))
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
                Just w
                    | bswBlockHash w == pbHash -> do
                        -- This case is uncommon, as duplicates are typically detected earlier, but
                        -- it can happen if:
                        --   1. the block bypasses de-duplication (i.e. as a direct message), and
                        --   2. the block is invalid, but has a valid baker signature.
                        -- We thus check if the block is actually distinct so that we do not
                        -- spuriously report it as a double signing.
                        return BlockResultDuplicate
                    | otherwise -> do
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

-- | Process receiving a block when the parent is not live.
--  Precondition: the block is for a round and epoch that have not already been finalized.
--
--  If the timestamp is no less than the receive time of the block + the early block threshold, the
--  function returns 'BlockResultEarly'. Otherwise, it returns 'BlockResultPending'.
receiveBlockUnknownParent ::
    ( LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadLogger m
    ) =>
    PendingBlock (MPV m) ->
    m (BlockResult (MPV m))
receiveBlockUnknownParent pendingBlock = do
    earlyThreshold <- rpEarlyBlockThreshold <$> use runtimeParameters
    if blockTimestamp pendingBlock
        < addDuration (utcTimeToTimestamp $ pbReceiveTime pendingBlock) earlyThreshold
        then do
            logEvent Konsensus LLInfo $ "Block " <> show pbHash <> " is pending its parent " <> show (blockParent pendingBlock) <> "."
            return BlockResultPending
        else return BlockResultEarly
  where
    pbHash :: BlockHash
    pbHash = getHash pendingBlock

-- | Get the minimum time between consecutive blocks, as of the specified block.
--  This is the value of the minimum block time chain parameter, and determines the minimum interval
--  between the block and a child block.
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

-- | Add a newly-arrived block, returning the new block pointer. This does the following:
--
--   * Adds the block to the block table as alive.
--
--   * Adds the block to the live branches.
--
--   * Updates the statistics to reflect the arrival time of the new block.
--
--   * Invokes the 'onBlock' event handler provided by 'MonadConsensusEvent'.
--
--  Preconditions:
--
--   * The block's parent must be the last finalized block or another live block.
--
--   * The block must not already be a live block.
addBlock ::
    forall m.
    (TimeMonad m, MonadState (SkovData (MPV m)) m, MonadConsensusEvent m, MonadLogger m, IsProtocolVersion (MPV m)) =>
    -- | Block to add
    PendingBlock (MPV m) ->
    -- | Block state
    HashedPersistentBlockState (MPV m) ->
    -- | Parent pointer
    BlockPointer (MPV m) ->
    -- | Energy used in executing the block
    Energy ->
    m (BlockPointer (MPV m))
addBlock pendingBlock blockState parent energyUsed = do
    let height = blockHeight parent + 1
    now <- currentTime
    newBlock <- makeLiveBlock @m pendingBlock blockState height now energyUsed
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

-- | Process a received block. This handles the processing after the initial checks and after the
--  block has been relayed (or not). This DOES NOT include processing pending children of the block
--  or signing the block as a finalizer.
--
--  If the block is executed successfully, it is added to the tree and returned.
--  Otherwise, 'Nothing' is returned.
--
--  The last finalized block may be updated as a result of the processing. In particular, if the
--  block is the first in a new epoch and contains a valid epoch finalization entry, the block
--  finalized by that entry will be finalized (if it was not already). Furthermore, if the block's QC
--  allows for the grandparent block to be finalized, then that will be finalized. (Note: both of
--  these do not happen for the same block, since the former requires the block to be in a different
--  epoch from its parent, and the latter requires it to be in the same epoch as its parent.)
--
--  Precondition:
--
--  * The parent block is correct: @getHash parent == blockParent pendingBlock@.
--  * The block is signed by a valid baker for its epoch.
--  * The baker is the leader for the round according to the parent block.
processBlock ::
    forall m.
    ( IsConsensusV1 (MPV m),
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadProtocolVersion m,
      MonadIO m,
      TimeMonad m,
      MonadThrow m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    -- | Parent block (@parent@)
    BlockPointer (MPV m) ->
    -- | Block being processed (@pendingBlock@)
    VerifiedBlock (MPV m) ->
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
                            checkBlockExecution genMeta parentBF $ \blockState energyUsed -> do
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
                                newBlock <- addBlock @m pendingBlock blockState parent energyUsed
                                let certifiedParent =
                                        CertifiedBlock
                                            { cbQuorumCertificate = blockQuorumCertificate pendingBlock,
                                              cbQuorumBlock = parent
                                            }
                                -- Write out the parent if it is newly certified, and check if
                                -- it finalizes other blocks also.
                                processCertifiedBlock certifiedParent
                                forM_ (blockEpochFinalizationEntry pendingBlock) $ \finEntry -> do
                                    -- If the epoch finalization entry is present, then it will
                                    -- already be checked to be valid.
                                    let finBlockHash = qcBlock (feFinalizedQuorumCertificate finEntry)
                                    -- Check if the block finalized by the entry is still live.
                                    mNewFinBlock <- gets (getLiveBlock finBlockHash)
                                    forM_ mNewFinBlock $ \finBlock -> do
                                        -- If so, we finalize it now.
                                        processFinalizationEntry finBlock finEntry
                                        shrinkTimeout finBlock
                                curRound <- use $ roundStatus . rsCurrentRound
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
                    -- Note that these conditions ensure that the parent block is descended from
                    -- the block finalized by the finalization entry (assuming sufficient honesty
                    -- among the finalization committee).
                    | qcEpoch (feFinalizedQuorumCertificate finEntry) == blockEpoch parent,
                      qcRound (feSuccessorQuorumCertificate finEntry) <= blockRound parent,
                      checkFinalizationEntry
                        gmCurrentGenesisHash
                        (toRational $ genesisSignatureThreshold gmParameters)
                        _bfFinalizers
                        finEntry -> do
                        -- We record that we have checked a valid QC for both rounds, since we
                        -- retain these for each certified block from the last finalized block
                        -- onwards.
                        recordCheckedQuorumCertificate $ feFinalizedQuorumCertificate finEntry
                        recordCheckedQuorumCertificate $ feSuccessorQuorumCertificate finEntry
                        -- Check that the finalized block has timestamp past the trigger time for
                        -- the epoch. We use the seed state for the parent block to get the trigger
                        -- time, because that is in the same epoch.
                        parentSeedState <- getSeedState (bpState parent)
                        -- Get the status of the block finalized by the finalization entry for the
                        -- check.
                        let finBlockHash = qcBlock (feFinalizedQuorumCertificate finEntry)
                        get >>= getBlockStatus finBlockHash >>= \case
                            BlockAliveOrFinalized finBlock
                                | blockTimestamp finBlock >= parentSeedState ^. triggerBlockTime ->
                                    -- The block is alive or already finalized, and justifies the
                                    -- epoch transition.
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
    checkBlockExecution GenesisMetadata{..} parentBF continue = do
        let execData =
                BlockExecutionData
                    { bedIsNewEpoch = blockEpoch pendingBlock == blockEpoch parent + 1,
                      bedEpochDuration = genesisEpochDuration gmParameters,
                      bedTimestamp = blockTimestamp pendingBlock,
                      bedBlockNonce = blockNonce pendingBlock,
                      bedParentState = bpState parent,
                      bedParticipatingBakers =
                        ParticipatingBakers
                            { pbBlockBaker = blockBaker pendingBlock,
                              pbQCSignatories =
                                quorumCertificateSigningBakers
                                    (_bfFinalizers parentBF)
                                    (blockQuorumCertificate pendingBlock)
                            }
                    }
        processBlockItems (sbBlock (pbBlock pendingBlock)) parent >>= \case
            Nothing -> do
                logEvent Konsensus LLTrace $
                    "Block "
                        <> show pbHash
                        <> " failed transaction verification."
                flag (BlockExecutionFailure sBlock)
                rejectBlock
            Just transactions -> do
                executeBlockState execData transactions >>= \case
                    Left failureReason -> do
                        logEvent Konsensus LLTrace $
                            "Block "
                                <> show pbHash
                                <> " failed execution: "
                                <> show failureReason
                        flag (BlockExecutionFailure sBlock)
                        rejectBlock
                    Right (newState, energyUsed) ->
                        case blockDerivableHashes pendingBlock of
                            DerivableBlockHashesV0
                                { dbhv0TransactionOutcomesHash = pendingBlockTxOutcomesHash,
                                  dbhv0BlockStateHash = pendingBlockStateHash
                                } -> do
                                    -- Prior to PV7 the transaction outcome was tracked separate from
                                    -- the state hash, meaning both have to be checked here.
                                    outcomesHash <- getTransactionOutcomesHash newState
                                    if
                                        | outcomesHash /= pendingBlockTxOutcomesHash -> do
                                            -- Incorrect transaction outcomes
                                            logEvent Konsensus LLTrace $
                                                "Block "
                                                    <> show pbHash
                                                    <> " stated transaction outcome hash ("
                                                    <> show pendingBlockTxOutcomesHash
                                                    <> ") does not match computed value ("
                                                    <> show outcomesHash
                                                    <> ")."
                                            flag $ BlockInvalidTransactionOutcomesHash sBlock (bpBlock parent)
                                            rejectBlock
                                        | getHash newState /= pendingBlockStateHash -> do
                                            -- Incorrect state hash
                                            logEvent Konsensus LLTrace $
                                                "Block "
                                                    <> show pbHash
                                                    <> " stated state hash ("
                                                    <> show pendingBlockStateHash
                                                    <> ") does not match computed value ("
                                                    <> show (getHash newState :: StateHash)
                                                    <> ")."
                                            flag $ BlockInvalidStateHash sBlock (bpBlock parent)
                                            rejectBlock
                                        | otherwise ->
                                            continue newState energyUsed
                            DerivableBlockHashesV1{dbhv1BlockResultHash = pendingBlockResultHash} -> do
                                -- Starting from P7 the baked block only contains a block result hash
                                -- which is computed from transaction outcomes, the block state hash
                                -- and more.
                                let relativeBlockHeight = 1 + blockHeight parent
                                computedResultHash <- computeBlockResultHash newState relativeBlockHeight
                                if computedResultHash /= pendingBlockResultHash
                                    then do
                                        -- Incorrect block result hash
                                        logEvent Konsensus LLTrace $
                                            "Block "
                                                <> show pbHash
                                                <> " stated result hash ("
                                                <> show pendingBlockResultHash
                                                <> ") does not match computed value ("
                                                <> show computedResultHash
                                                <> ")."
                                        flag $ BlockInvalidStateHash sBlock (bpBlock parent)
                                        rejectBlock
                                    else continue newState energyUsed
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
                logEvent Konsensus LLTrace $
                    "Block "
                        <> show pbHash
                        <> " contains an invalid QC."
                flag $ BlockInvalidQC sBlock
                rejectBlock

-- | A 'BlockPointer', ordered lexicographically on:
--
--  * Round number
--  * Epoch number
--  * Descending timestamp
--  * Block hash
--
--  This ordering is used to determine the "best block" to sign when a block arrives.
--
--  Note that the ordering above could also first have been on 'Epoch' and then 'Round'
--  since if we had two blocks: one with a higher 'Round' and one with
--  a higher 'Epoch' then neither of those will be signed.
--  In the lower 'Round' case then the 'Round' would've timed out (hence the higher 'Epoch')
--  and in the lower 'Epoch' case the consensus runner will already be an 'Epoch' ahead.
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

-- | Produce a quorum signature on a block.
--  This checks that the block is still valid, is in the current round and epoch, and the
--  round is signable (i.e. we haven't already signed a quorum message or timeout message).
--
--  It is assumed that the supplied 'FinalizerInfo' accurately represents the finalizer in the
--  epoch of the block, and that the baker identity matches the finalizer info (i.e. the keys
--  are correct).
validateBlock ::
    ( MonadState (SkovData (MPV m)) m,
      MonadBroadcast m,
      LowLevel.MonadTreeStateStore m,
      IsConsensusV1 (MPV m),
      MonadThrow m,
      MonadIO m,
      MonadProtocolVersion m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      MonadReader r m,
      HasBakerContext r,
      TimeMonad m,
      TimerMonad m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    -- | The block to sign.
    BlockHash ->
    -- | The baker keys.
    BakerIdentity ->
    -- | The details of the finalizer in the finalization committee for the block's epoch.
    FinalizerInfo ->
    m ()
validateBlock blockHash BakerIdentity{..} finInfo = do
    logEvent Baker LLTrace "validateBlock called"
    maybeBlock <- gets $ getLiveBlock blockHash
    forM_ maybeBlock $ \block -> do
        logEvent Baker LLTrace $
            "validateBlock: block "
                ++ show (getHash @BlockHash block)
                ++ " is live"
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
                processQuorumMessage
                    VerifiedQuorumMessage
                        { vqmMessage = quorumMessage,
                          vqmFinalizerWeight = finalizerWeight finInfo,
                          vqmFinalizerBakerId = finalizerBakerId finInfo,
                          vqmBlock = block
                        }
                    makeBlock

-- | If the given time has elapsed, perform the supplied action. Otherwise, start a timer to
--  asynchronously perform the action at the given time.
doAfter :: (TimeMonad m, TimerMonad m) => UTCTime -> m () -> m ()
doAfter time action = do
    now <- currentTime
    if time <= now
        then action
        else void $ onTimeout (DelayUntil time) action

-- | Produce a quorum signature on a block if the block is eligible and we are a finalizer for the
--  block's epoch. This will delay until the timestamp of the block has elapsed so that we do not
--  sign blocks prematurely.
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
      MonadProtocolVersion m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m,
      IsConsensusV1 (MPV m)
    ) =>
    -- | Block to (potentially) sign.
    b ->
    m ()
checkedValidateBlock validBlock = do
    logEvent Baker LLTrace "checkedValidateBlock called"
    withFinalizerForEpoch (blockEpoch validBlock) $ \bakerIdent finInfo -> do
        let !blockHash = getHash validBlock
        logEvent Baker LLTrace $
            "checkedValidateBlock delaying until "
                ++ show (timestampToUTCTime $ blockTimestamp validBlock)
        doAfter (timestampToUTCTime $ blockTimestamp validBlock) $!
            validateBlock blockHash bakerIdent finInfo

-- | Execute a block that has previously been verified by 'uponReceivingBlock'.
--
--  This should be robust against other consensus operations occurring in between
--  'uponReceivingBlock' and 'executeBlock'. However, in practise it is recommended that
--  'executeBlock' should be called immediately, without releasing the lock on the consensus
--  state.
executeBlock ::
    ( IsConsensusV1 (MPV m),
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadState (SkovData (MPV m)) m,
      MonadProtocolVersion m,
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
    VerifiedBlock (MPV m) ->
    m ()
executeBlock verifiedBlock = do
    isShutdown <- use isConsensusShutdown
    unless isShutdown $ do
        gets (getLiveOrLastFinalizedBlock (blockParent (vbBlock verifiedBlock))) >>= \case
            Just parent -> do
                res <- processBlock parent verifiedBlock
                forM_ res checkedValidateBlock
            Nothing -> return ()

-- * Block production

-- | Inputs used for baking a new block.
data BakeBlockInputs (pv :: ProtocolVersion) = BakeBlockInputs
    { -- | Secret keys for the baker.
      bbiBakerIdentity :: BakerIdentity,
      -- | Round in which the block is to be produced.
      bbiRound :: Round,
      -- | Epoch in which the block is to be produced.
      --  Should always be either @blockEpoch bbiParent@ or @1 + blockEpoch bbiParent@.
      bbiEpoch :: Epoch,
      -- | Parent block.
      bbiParent :: BlockPointer pv,
      -- | A valid quorum certificate for the parent block.
      bbiQuorumCertificate :: QuorumCertificate,
      -- | If the parent block belongs to a round earlier than @bbiRound - 1@, this is a valid
      --  timeout certificate for round @bbiRound - 1@ in the same epoch as the parent block.
      --  Otherwise, it is 'Absent'.
      bbiTimeoutCertificate :: Option TimeoutCertificate,
      -- | If the block is to be the first in a new epoch (i.e. @bbiEpoch@ is
      --  @blockEpoch bbiParent + 1@) then this is a valid finalization entry for a block after the
      --  trigger time in epoch @blockEpoch bbiParent@.
      bbiEpochFinalizationEntry :: Option FinalizationEntry,
      -- | The set of bakers for the epoch 'bbiEpoch'.
      bbiEpochBakers :: FullBakers,
      -- | The leadership election nonce used in the election.
      bbiLeadershipElectionNonce :: LeadershipElectionNonce
    }

-- | Determine if the node is a baker and eligible to bake in the current round, and produce
--  'BakeBlockInputs' if so. This checks:
--
--    * We have not already attempted to bake for this round.
--
--    * We have not baked for a more recent round.
--
--    * We have baker credentials.
--
--    * We are the winner of the round in the current epoch.
--
--    * The baker's public keys match those in the baking committee.
--
--  Note, if any of the tests fails, it is expected to fail on subsequent calls in the same round.
--  (In particular, we do not expect the baker keys to change.) Thus, when 'prepareBakeBlockInputs'
--  is called, it marks that we have attempted to bake for the round, so subsequent calls will fail
--  until the round is advanced (and so the flag is reset).
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
    -- We check that the finalization entry is consistent with the QC, since otherwise the block
    -- would be rejected. Normally, this shouldn't be an issue because the highest QC will be
    -- at least the successor QC in the finalization entry.
    forM_ bbiEpochFinalizationEntry $ \finEntry -> do
        unless (qcRound (feSuccessorQuorumCertificate finEntry) <= qcRound bbiQuorumCertificate) $
            do
                logEvent Konsensus LLDebug $
                    "Unable to bake block in round "
                        ++ show bbiRound
                        ++ ", epoch "
                        ++ show bbiEpoch
                        ++ " since the parent block ("
                        ++ show (getHash @BlockHash bbiParent)
                        ++ ") is in round "
                        ++ show (blockRound bbiParent)
                        ++ " but the epoch finalization entry includes a QC for round "
                        ++ show (qcRound (feSuccessorQuorumCertificate finEntry))
                        ++ "."
                empty
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

-- | Construct a block given 'BakeBlockInputs'.
bakeBlock ::
    forall m.
    ( MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      TimeMonad m,
      MonadProtocolVersion m,
      IsConsensusV1 (MPV m),
      LowLevel.MonadTreeStateStore m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    BakeBlockInputs (MPV m) ->
    m (SignedBlock (MPV m))
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
    !signatories <-
        gets (getBakersForEpoch (qcEpoch bbiQuorumCertificate)) <&> \case
            Nothing -> error "Invariant violation: could not determine bakers for QC epoch."
            Just bakers -> quorumCertificateSigningBakers (bakers ^. bfFinalizers) bbiQuorumCertificate
    let executionData =
            BlockExecutionData
                { bedIsNewEpoch = isPresent bbiEpochFinalizationEntry,
                  bedEpochDuration = epochDuration,
                  bedTimestamp = bbTimestamp,
                  bedBlockNonce = bbNonce,
                  bedParentState = bpState bbiParent,
                  bedParticipatingBakers =
                    ParticipatingBakers
                        { pbBlockBaker = bakerId bbiBakerIdentity,
                          pbQCSignatories = signatories
                        }
                }
    runtime <- use runtimeParameters
    tt <- use transactionTable
    updateFocusBlockTo bbiParent
    ptt <- use pendingTransactionTable
    (filteredTransactions, newState, energyUsed) <- constructBlockState runtime tt ptt executionData
    bbDerivableHashes <- case blockHashVersion @(BlockHashVersionFor (MPV m)) of
        SBlockHashVersion0 -> do
            dbhv0TransactionOutcomesHash <- getTransactionOutcomesHash newState
            dbhv0BlockStateHash <- getStateHash newState
            return $ DerivableBlockHashesV0{..}
        SBlockHashVersion1 -> do
            let relativeBlockHeight = 1 + blockHeight bbiParent
            dbhv1BlockResultHash <- computeBlockResultHash newState relativeBlockHeight
            return $ DerivableBlockHashesV1{..}
    let bakedBlock =
            BakedBlock
                { bbRound = bbiRound,
                  bbEpoch = bbiEpoch,
                  bbBaker = bakerId bbiBakerIdentity,
                  bbQuorumCertificate = bbiQuorumCertificate,
                  bbTimeoutCertificate = bbiTimeoutCertificate,
                  bbEpochFinalizationEntry = bbiEpochFinalizationEntry,
                  bbTransactions = Vector.fromList (fst . fst <$> ftAdded filteredTransactions),
                  ..
                }
    genesisHash <- use currentGenesisHash
    let signedBlock = signBlock (bakerSignKey bbiBakerIdentity) genesisHash bakedBlock
    updatePersistentRoundStatus $ prsLastBakedRound .~ bbiRound
    now <- currentTime
    let nominalTime = timestampToUTCTime bbTimestamp
    statistics %=! updateStatsOnReceive nominalTime now
    newBlockPointer <-
        addBlock
            PendingBlock{pbBlock = signedBlock, pbReceiveTime = now}
            newState
            bbiParent
            energyUsed
    focusBlock .=! newBlockPointer
    -- Update the transaction table and pending transaction table.
    lfb <- use lastFinalized
    let (newTT, newPTT) =
            filterTables
                (blockRound lfb)
                (blockRound signedBlock)
                (getHash signedBlock)
                filteredTransactions
                tt
                ptt
    transactionTable .=! newTT
    pendingTransactionTable .=! newPTT
    return signedBlock

-- | Extract information from SkovData and the block state to compute the result block hash.
computeBlockResultHash ::
    forall m.
    ( MonadState (SkovData (MPV m)) m,
      BlockStateStorage m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m)
    ) =>
    -- | The block state right after executing the block.
    HashedPersistentBlockState (MPV m) ->
    -- | The relative block height for the block.
    BlockHeight ->
    m BlockResultHash
computeBlockResultHash newState relativeBlockHeight = do
    theBlockStateHash <- getStateHash newState
    transactionOutcomesHash <- getTransactionOutcomesHash newState
    currentFinalizationCommitteeHash <- use $ to bakersForCurrentEpoch . bfFinalizerHash
    nextFinalizationCommitteeHash <- do
        -- Attempt to get the finalization committee from SkovData otherwise compute it from the
        -- information in the block state.
        currentEpoch <- use (roundStatus . rsCurrentEpoch)
        nextSkovBakersAndFinalizers <- gets (getBakersForEpoch (currentEpoch + 1))
        case nextSkovBakersAndFinalizers of
            Just bakersAndFinalizers -> return $ bakersAndFinalizers ^. bfFinalizerHash
            Nothing -> do
                nextFullBakers <- getNextEpochBakers newState
                nextFinalizationParameters <- getNextEpochFinalizationCommitteeParameters newState
                let nextFinalizationCommittee = computeFinalizationCommittee nextFullBakers nextFinalizationParameters
                return $ computeFinalizationCommitteeHash nextFinalizationCommittee
    blockHeightInfo <- do
        genesisBlockHeightInfo <- use genesisBlockHeight
        return
            BlockHeightInfo
                { bhiAbsoluteBlockHeight = localToAbsoluteBlockHeight (gbhiAbsoluteHeight genesisBlockHeightInfo) relativeBlockHeight,
                  bhiGenesisIndex = gbhiGenesisIndex genesisBlockHeightInfo,
                  bhiRelativeBlockHeight = relativeBlockHeight
                }
    return $
        makeBlockResultHash
            BlockResultHashInput
                { shiBlockStateHash = theBlockStateHash,
                  shiTransationOutcomesHash = transactionOutcomesHash,
                  shiCurrentFinalizationCommitteeHash = currentFinalizationCommitteeHash,
                  shiNextFinalizationCommitteeHash = nextFinalizationCommitteeHash,
                  shiBlockHeightInfo = blockHeightInfo
                }

-- | Try to make a block, distribute it on the network and sign it as a finalizer.
--  This function should be called after any operation that can advance the current round to
--  attempt block production. A block will only be produced if we have credentials, are the
--  winner of the round in the current epoch, and have not already tried to produce a block in the
--  round.
--
--  Note: We will only attempt to bake a block once for a round. It is possible (if unlikely) that
--  we might enter a round and subsequently advance epoch, while remaining in the same round.
--  It could be that we did not win the round in the old epoch, but do in the new one. In such a
--  case, it would be reasonable to produce a block for the round in the new epoch, but we do not.
--  The circumstances creating this scenario are sufficiently rare that it shouldn't be a problem to
--  allow the round to time out.
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
      MonadProtocolVersion m,
      MonadBroadcast m,
      MonadThrow m,
      MonadIO m,
      MonadTimeout m,
      MonadConsensusEvent m,
      MonadLogger m
    ) =>
    m ()
makeBlock = do
    isShutdown <- use isConsensusShutdown
    unless isShutdown $ do
        mInputs <- prepareBakeBlockInputs
        forM_ mInputs $ \inputs -> do
            block <- bakeBlock inputs
            logEvent Baker LLDebug $
                "Baking block at "
                    ++ show (timestampToUTCTime $ blockTimestamp block)
            void . onTimeout (DelayUntil . timestampToUTCTime $ blockTimestamp block) $ do
                sendBlock block
                checkedValidateBlock block
