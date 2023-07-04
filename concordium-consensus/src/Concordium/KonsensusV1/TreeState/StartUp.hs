{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.StartUp where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Statistics as Stats
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.Transactions
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimeMonad
import Concordium.TransactionVerification as TVer

-- |Generate the 'EpochBakers' for a genesis block.
genesisEpochBakers ::
    ( BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      MPV m ~ pv
    ) =>
    PBS.HashedPersistentBlockState pv ->
    m EpochBakers
genesisEpochBakers genState = do
    curFullBakers <- getCurrentEpochBakers genState
    curFinParams <- getCurrentEpochFinalizationCommitteeParameters genState
    let _currentEpochBakers = computeBakersAndFinalizers curFullBakers curFinParams
    let _previousEpochBakers = _currentEpochBakers
    nextFullBakers <- getNextEpochBakers genState
    nextFinParams <- getNextEpochFinalizationCommitteeParameters genState
    let _nextEpochBakers = computeBakersAndFinalizers nextFullBakers nextFinParams
    _nextPayday <- getPaydayEpoch genState
    return $! EpochBakers{..}

-- |Construct the epoch bakers for a given last finalized block based on the low level tree state
-- store.
makeEpochBakers ::
    ( BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      MPV m ~ pv,
      MonadThrow m,
      LowLevel.MonadTreeStateStore m,
      MonadIO m
    ) =>
    BlockPointer pv ->
    m EpochBakers
makeEpochBakers lastFinBlock = do
    let lfbState = bpState lastFinBlock
    curFullBakers <- getCurrentEpochBakers lfbState
    curFinParams <- getCurrentEpochFinalizationCommitteeParameters lfbState
    let _currentEpochBakers = computeBakersAndFinalizers curFullBakers curFinParams
    nextFullBakers <- getNextEpochBakers lfbState
    nextFinParams <- getNextEpochFinalizationCommitteeParameters lfbState
    let _nextEpochBakers = computeBakersAndFinalizers nextFullBakers nextFinParams
    _nextPayday <- getPaydayEpoch lfbState
    _previousEpochBakers <-
        if blockEpoch lastFinBlock == 0
            then return _currentEpochBakers
            else backTo (blockEpoch lastFinBlock - 1) (0, 0) (blockEpoch lastFinBlock, blockHeight lastFinBlock)
    return $! EpochBakers{..}
  where
    -- INVARIANTS:
    --  * lowEpoch <= targetEpoch < highEpoch
    --  * lowHeight < highHeight
    --  * The block at lowHeight has epoch lowEpoch
    --  * The block at highEpoch has epoch highEpoch
    --  * If blockHeight x <= blockHeight y then blockEpoch x <= blockEpoch y.
    --  * There is at least one block of each epoch up to highEpoch.
    backTo targetEpoch (lowEpoch, lowHeight) (highEpoch, highHeight) = do
        -- We split the height interval in proportion to where the target epoch falls in the
        -- epoch interval. This guarantees:
        --  * curHeight < highHeight, since targetEpoch < highEpoch
        --  * lowHeight <= curHeight, with lowHeight == curHeight only if lowEpoch == targetEpoch,
        --    since (highHeight - lowHeight) >= (highEpoch - lowEpoch).
        let curHeight =
                lowHeight
                    + fromIntegral
                        ( toInteger (highHeight - lowHeight)
                            * toInteger (targetEpoch - lowEpoch)
                            `div` toInteger (highEpoch - lowEpoch)
                        )
        LowLevel.lookupBlockByHeight curHeight >>= \case
            Nothing ->
                throwM . TreeStateInvariantViolation $
                    "Missing block at height " ++ show curHeight
            Just stb -> case compare (blockEpoch stb) targetEpoch of
                EQ -> do
                    blockState <- bpState <$> mkBlockPointer stb
                    fullBakers <- getCurrentEpochBakers blockState
                    finParams <- getCurrentEpochFinalizationCommitteeParameters blockState
                    return $ computeBakersAndFinalizers fullBakers finParams
                LT -> do
                    backTo targetEpoch (blockEpoch stb, curHeight) (highEpoch, highHeight)
                GT -> do
                    backTo targetEpoch (lowEpoch, lowHeight) (blockEpoch stb, curHeight)

-- |Construct a 'SkovData' by initialising it with data loaded from disk.
--
-- Note: this does not fully initialise the transaction table, and does not load the certified
-- blocks into the block table.
loadSkovData ::
    ( MonadThrow m,
      LowLevel.MonadTreeStateStore m,
      MonadIO m,
      BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv,
      MPV m ~ pv,
      IsConsensusV1 pv
    ) =>
    RuntimeParameters ->
    m (SkovData pv)
loadSkovData _runtimeParameters = do
    _persistentRoundStatus <- LowLevel.lookupCurrentRoundStatus
    mLatestFinEntry <- LowLevel.lookupLatestFinalizationEntry
    genesisBlock <-
        LowLevel.lookupFirstBlock >>= \case
            Nothing -> throwM . TreeStateInvariantViolation $ "Missing genesis block in database"
            Just gb -> mkBlockPointer gb
    lastFinBlock <-
        LowLevel.lookupLastFinalizedBlock >>= \case
            Nothing -> throwM . TreeStateInvariantViolation $ "Missing last finalized block in database"
            Just b -> mkBlockPointer b
    _rsHighestCertifiedBlock <- do
        case mLatestFinEntry of
            Nothing ->
                return
                    CertifiedBlock
                        { cbQuorumCertificate = genesisQuorumCertificate (getHash genesisBlock),
                          cbQuorumBlock = genesisBlock
                        }
            Just finEntry
                | let qc = feFinalizedQuorumCertificate finEntry,
                  qcBlock qc == getHash lastFinBlock -> do
                    return
                        CertifiedBlock
                            { cbQuorumCertificate = qc,
                              cbQuorumBlock = lastFinBlock
                            }
                | otherwise ->
                    throwM . TreeStateInvariantViolation $
                        "Database last finalized entry does not match the last finalized block"
    let currentRound = 1 + cbRound _rsHighestCertifiedBlock
    lastFinSeedState <- getSeedState $ bpState lastFinBlock
    (currentEpoch, lastEpochFinEntry) <-
        if lastFinSeedState ^. epochTransitionTriggered -- FIXME: Also check that the consensus is not shut down
            then case mLatestFinEntry of
                Nothing ->
                    throwM . TreeStateInvariantViolation $
                        "Missing finalization entry for last finalized block"
                Just finEntry -> return (blockEpoch lastFinBlock + 1, Present finEntry)
            else return (blockEpoch lastFinBlock, Absent)
    chainParams <- getChainParameters $ bpState lastFinBlock
    let _roundStatus =
            RoundStatus
                { _rsCurrentRound = currentRound,
                  _rsHighestCertifiedBlock = _rsHighestCertifiedBlock,
                  _rsPreviousRoundTimeout = Absent,
                  _rsRoundEligibleToBake = True,
                  _rsCurrentEpoch = currentEpoch,
                  _rsLastEpochFinalizationEntry = lastEpochFinEntry,
                  _rsCurrentTimeout =
                    chainParams
                        ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
                }
    let _blockTable = emptyBlockTable
    let _branches = Seq.empty
    let _roundExistingBlocks = Map.empty
    -- We record that we have checked the QC for the last finalized block.
    let _roundExistingQCs = case mLatestFinEntry of
            Nothing -> Map.empty
            Just finEntry -> Map.singleton (qcRound qc) (toQuorumCertificateWitness qc)
              where
                qc = feFinalizedQuorumCertificate finEntry
    _genesisMetadata <- case bpBlock genesisBlock of
        GenesisBlock gm -> return gm
        _ -> throwM . TreeStateInvariantViolation $ "First block is not a genesis block"
    let _skovPendingBlocks = emptyPendingBlocks
    let _lastFinalized = lastFinBlock
    _latestFinalizationEntry <- maybe Absent Present <$> LowLevel.lookupLatestFinalizationEntry
    _skovEpochBakers <- makeEpochBakers lastFinBlock

    -- We will load our last timeout message if appropriate in 'loadCertifiedBlocks'.
    let _currentTimeoutMessages = Absent
    let _currentQuorumMessages = emptyQuorumMessages
    let _transactionTable = TT.emptyTransactionTable
    let _transactionTablePurgeCounter = 0
    let _skovPendingTransactions =
            PendingTransactions
                { _pendingTransactionTable = TT.emptyPendingTransactionTable,
                  _focusBlock = lastFinBlock
                }
    let _statistics = Stats.initialConsensusStatistics
    return SkovData{..}

-- |Load the certified blocks from the low-level database into the tree state.
-- This caches their block states, adds them to the block table and branches,
-- adds their transactions to the transaction table and pending transaction table,
-- updates the highest certified block, and records block signature witnesses and
-- checked quorum certificates for the blocks.
--
-- This also sets the previous round timeout if the low level state records that it timed out.
-- It also puts the latest timeout message in the set of timeout messages for the current round
-- if the current round matches the round of the timeout message.
--
-- This should be called on the result of 'loadSkovData' after the transaction table has
-- been initialised for the last finalized block.
loadCertifiedBlocks ::
    forall m.
    ( MonadThrow m,
      LowLevel.MonadTreeStateStore m,
      MonadIO m,
      BlockStateStorage m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      MonadState (SkovData (MPV m)) m,
      TimeMonad m
    ) =>
    m ()
loadCertifiedBlocks = do
    certBlocks <- LowLevel.lookupCertifiedBlocks
    mapM_ loadCertBlock certBlocks
    oLastTimeout <- use $ persistentRoundStatus . prsLatestTimeout
    forM_ oLastTimeout $ \lastTimeout -> do
        curRound <- use $ roundStatus . rsCurrentRound
        when (tcRound lastTimeout >= curRound) $ do
            highCB <- use $ roundStatus . rsHighestCertifiedBlock
            unless
                ( cbRound highCB < tcRound lastTimeout
                    && cbRound highCB >= tcMaxRound lastTimeout
                    && cbEpoch highCB >= tcMaxEpoch lastTimeout
                    && cbEpoch highCB <= 2 + tcMinEpoch lastTimeout
                )
                $ throwM . TreeStateInvariantViolation
                $ "Missing certified block consistent with last timeout certificate"
            roundStatus . rsPreviousRoundTimeout
                .= Present
                    RoundTimeout
                        { rtTimeoutCertificate = lastTimeout,
                          rtCertifiedBlock = highCB
                        }
            roundStatus . rsCurrentRound .= tcRound lastTimeout + 1
    rs <- use roundStatus
    let expectedCurrentRound
            | Present prevTO <- rs ^. rsPreviousRoundTimeout = 1 + tcRound (rtTimeoutCertificate prevTO)
            | otherwise = 1 + cbRound (rs ^. rsHighestCertifiedBlock)
    unless (expectedCurrentRound == rs ^. rsCurrentRound) $
        throwM . TreeStateInvariantViolation $
            "The current round does not match the expected round."
    -- Add the latest timeout message to the timeout messages if it makes sense in the current
    -- context.
    prs <- use persistentRoundStatus
    forM_ (_prsLastSignedTimeoutMessage prs) $ \tm -> do
        when (tmRound (tmBody tm) == rs ^. rsCurrentRound) $ do
            forM_ (updateTimeoutMessages Absent tm) $ \tms -> currentTimeoutMessages .= Present tms
  where
    loadCertBlock (storedBlock, qc) = do
        blockPointer <- mkBlockPointer storedBlock
        cacheBlockState (bpState blockPointer)
        blockTable . liveMap . at' (getHash blockPointer) ?=! MemBlockAlive blockPointer
        addToBranches blockPointer
        forM_ (blockTransactions blockPointer) $ \tr -> do
            -- Add transactions to the transaction table as 'TVer.TrustedSuccess', since they
            -- occur in blocks that have already been checked.
            let verRes = TVer.Ok TVer.TrustedSuccess
            added <-
                transactionTable
                    %%=! TT.addTransaction tr (TT.commitPoint (blockRound blockPointer)) verRes
            unless added $
                throwM . TreeStateInvariantViolation $
                    "Transaction in certified block cannot be added to transaction table"
            addPendingTransaction tr
        roundStatus . rsHighestCertifiedBlock
            .= CertifiedBlock
                { cbQuorumCertificate = qc,
                  cbQuorumBlock = blockPointer
                }

        curRound <- use $ roundStatus . rsCurrentRound
        when (blockRound blockPointer >= curRound) $ do
            roundStatus
                %=! (rsPreviousRoundTimeout .~ Absent)
                . (rsCurrentRound .~ blockRound blockPointer + 1)

        forM_ (blockBakedData blockPointer) $ \signedBlock ->
            roundBakerExistingBlock (blockRound signedBlock) (blockBaker signedBlock)
                ?= toBlockSignatureWitness signedBlock
        recordCheckedQuorumCertificate qc
