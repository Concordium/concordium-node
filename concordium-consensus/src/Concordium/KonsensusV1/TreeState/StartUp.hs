{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.StartUp where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Lens.Micro.Platform

import Concordium.Genesis.Data.BaseV1
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Types.SeedState
import Concordium.Types.UpdateQueues
import Concordium.Types.Updates
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
import Concordium.Logger
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

-- |Given a block with the 'shutdownTriggered' flag set in its seed state, trace back along the
-- chain to find the earliest such block.
findShutdownTriggerBlock ::
    ( LowLevel.MonadTreeStateStore m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState (MPV m),
      BlockStateQuery m,
      IsConsensusV1 (MPV m),
      MonadIO m,
      MonadThrow m
    ) =>
    BlockPointer (MPV m) ->
    m (BlockPointer (MPV m))
findShutdownTriggerBlock candidateTriggerBlock = do
    parentHash <- case blockBakedData candidateTriggerBlock of
        Absent ->
            throwM . TreeStateInvariantViolation $
                "Shutdown trigger flag is set in the genesis block."
        Present bbd -> return $ blockParent bbd
    LowLevel.lookupBlock parentHash >>= \case
        Just parentSB -> do
            parent <- mkBlockPointer parentSB
            parentSeedState <- getSeedState $ bpState parent
            if parentSeedState ^. shutdownTriggered
                then findShutdownTriggerBlock parent
                else return candidateTriggerBlock
        Nothing ->
            throwM . TreeStateInvariantViolation $
                "The block "
                    ++ show parentHash
                    ++ " is not present in the database, but is the parent of "
                    ++ show (getHash @BlockHash candidateTriggerBlock)
                    ++ "."

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
    -- |Runtime parameters to use
    RuntimeParameters ->
    -- |Set to 'True' if a rollback occurred before loading the skov
    Bool ->
    -- |The 'SkovData' and, if the consensus is shutdown, the effective protocol update.
    m (SkovData pv, Maybe ProtocolUpdate)
loadSkovData _runtimeParameters didRollback = do
    _persistentRoundStatus <- LowLevel.lookupCurrentRoundStatus
    mLatestFinEntry <- LowLevel.lookupLatestFinalizationEntry
    genesisBlock <-
        LowLevel.lookupFirstBlock >>= \case
            Nothing -> throwM . TreeStateInvariantViolation $ "Missing genesis block in database"
            Just gb -> mkBlockPointer gb
    _genesisMetadata <- case bpBlock genesisBlock of
        GenesisBlock gm -> return gm
        _ -> throwM . TreeStateInvariantViolation $ "First block is not a genesis block"
    let sigThreshold = toRational $ genesisSignatureThreshold $ gmParameters _genesisMetadata
    lastFinBlock <-
        LowLevel.lookupLastFinalizedBlock >>= \case
            Nothing -> throwM . TreeStateInvariantViolation $ "Missing last finalized block in database"
            Just b -> mkBlockPointer b
    _skovEpochBakers <- makeEpochBakers lastFinBlock
    _rsHighestCertifiedBlock <- do
        case mLatestFinEntry of
            Nothing
                | blockRound lastFinBlock == 0 ->
                    return
                        CertifiedBlock
                            { cbQuorumCertificate = genesisQuorumCertificate (getHash genesisBlock),
                              cbQuorumBlock = genesisBlock
                            }
                | otherwise ->
                    throwM . TreeStateInvariantViolation $
                        "Missing finalization entry for last finalized block"
            Just finEntry
                | let qc = feFinalizedQuorumCertificate finEntry,
                  qcBlock qc == getHash lastFinBlock -> do
                    -- Validate the finalization entry
                    unless
                        ( checkFinalizationEntry
                            (getHash genesisBlock)
                            sigThreshold
                            (_skovEpochBakers ^. currentEpochBakers . bfFinalizers)
                            finEntry
                        )
                        $ throwM . TreeStateInvariantViolation
                        $ "Latest finalization entry is not valid: " ++ show finEntry
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
    -- If the last finalized block has the shutdown trigger flag set in its
    -- seedstate, the consensus is shutdown, so we determine the shutdown trigger block, which
    -- will be the terminal block. The terminal block may differ from the last finalized block.
    let consensusIsShutdown = lastFinSeedState ^. shutdownTriggered
    _terminalBlock <-
        if consensusIsShutdown
            then Present <$> findShutdownTriggerBlock lastFinBlock
            else return Absent
    (currentEpoch, lastEpochFinEntry) <-
        if lastFinSeedState ^. epochTransitionTriggered
            && not consensusIsShutdown
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
                  -- If blocks were rolled back, we should not attempt to bake in the current round
                  -- since it will be older than it would have been before the rollback.
                  _rsRoundEligibleToBake = not didRollback,
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
    let _skovPendingBlocks = emptyPendingBlocks
    let _lastFinalized = lastFinBlock
    _latestFinalizationEntry <- maybe Absent Present <$> LowLevel.lookupLatestFinalizationEntry
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
    protocolUpdate <-
        if consensusIsShutdown
            then do
                getProtocolUpdateStatus (bpState lastFinBlock) >>= \case
                    ProtocolUpdated pu -> return $ Just pu
                    PendingProtocolUpdates{} ->
                        throwM . TreeStateInvariantViolation $
                            "Consensus is shut down, but no effective protocol update was present \
                            \in the last finalized block."
            else return Nothing
    return (SkovData{..}, protocolUpdate)

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
      TimeMonad m,
      MonadLogger m
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
            eBkrs <- use epochBakers
            -- If the finalization committee for the highest certified block is different
            -- from the committee for the last finalized block, then there is a question of
            -- which certified block should be paired with the timeout certificate.
            let requiresTCCheck = blockEpoch (cbQuorumBlock highCB) >= eBkrs ^. nextPayday
            GenesisMetadata{..} <- use genesisMetadata
            let checkTC' eb1 eb2 ebParent =
                    checkTimeoutCertificate
                        gmCurrentGenesisHash
                        (toRational $ genesisSignatureThreshold gmParameters)
                        (eb1 ^. bfFinalizers)
                        (eb2 ^. bfFinalizers)
                        (ebParent ^. bfFinalizers)
                        lastTimeout
            lastFinEpoch <- use $ lastFinalized . to blockEpoch
            -- This function checks the timeout certificate is valid with respect to the bakers
            -- for a given epoch.
            let checkTC
                    | tcMinEpoch lastTimeout == lastFinEpoch - 1 =
                        checkTC' (eBkrs ^. previousEpochBakers) (eBkrs ^. currentEpochBakers)
                    | tcMinEpoch lastTimeout == lastFinEpoch =
                        checkTC' (eBkrs ^. currentEpochBakers) (eBkrs ^. nextEpochBakers)
                    | tcIsSingleEpoch lastTimeout && tcMinEpoch lastTimeout == lastFinEpoch + 1 =
                        checkTC' (eBkrs ^. nextEpochBakers) (eBkrs ^. nextEpochBakers)
                    | otherwise = const False
            let tcOKForHCB =
                    blockEpoch (cbQuorumBlock highCB) == lastFinEpoch + 1
                        && checkTC (eBkrs ^. nextEpochBakers)
            let checkBlockCompatibleWithLastTimeout certBlock =
                    cbRound certBlock < tcRound lastTimeout
                        && cbRound certBlock >= tcMaxRound lastTimeout
                        && cbEpoch certBlock >= tcMaxEpoch lastTimeout
                        -- This last condition should always be true, otherwise the timeout certificate
                        -- itself is invalid. For it to fail, tcMaxEpoch would be below the epoch of the
                        -- last finalized block, and therefore cannot justify a timeout in a subsequent
                        -- round.
                        && cbEpoch certBlock <= 2 + tcMinEpoch lastTimeout
            if (not requiresTCCheck || tcOKForHCB) && checkBlockCompatibleWithLastTimeout highCB
                then setLastTimeout lastTimeout highCB
                else
                    if requiresTCCheck && not tcOKForHCB
                        then do
                            -- Find the certified block for the previous epoch with the highest
                            -- round number.
                            candidateCB <- case dropWhile ((lastFinEpoch /=) . qcEpoch . snd) . reverse $ certBlocks of
                                [] -> do
                                    -- There are no subsequent certified blocks in the same epoch as
                                    -- the last finalized block, so use the last finalized block.
                                    lfb <- use lastFinalized
                                    -- We get the quorum certificate from the latest finalization
                                    -- entry. If there is no entry, then the last finalized block
                                    -- is the genesis block, so we use a genesis quorum certificate.
                                    qc <-
                                        ofOption
                                            (genesisQuorumCertificate (getHash lfb))
                                            feFinalizedQuorumCertificate
                                            <$> use latestFinalizationEntry
                                    return $ CertifiedBlock qc lfb
                                ((_, qc) : _) -> do
                                    mblock <- gets (getLiveBlock (qcBlock qc))
                                    case mblock of
                                        Nothing ->
                                            -- This case cannot happen, since loadCertBlock
                                            -- is called on all of the certified blocks, which
                                            -- ensures they are in the live blocks.
                                            error "Missing certified block"
                                        Just block -> return $ CertifiedBlock qc block
                            -- Unless the database has been corrupted in an unexpected way, it
                            -- should not be possible for the timeout certificate to be invalid
                            -- with respect to the epoch of the last finalized block at this point.
                            unless (checkTC (eBkrs ^. currentEpochBakers)) $
                                throwM . TreeStateInvariantViolation $
                                    "The latest timeout certificate could not be validated."
                            if checkBlockCompatibleWithLastTimeout candidateCB
                                then setLastTimeout lastTimeout candidateCB
                                else failLoadLastTimeout
                        else failLoadLastTimeout

    rs <- use roundStatus
    let expectedCurrentRound
            | Present prevTO <- rs ^. rsPreviousRoundTimeout = 1 + tcRound (rtTimeoutCertificate prevTO)
            | otherwise = 1 + cbRound (rs ^. rsHighestCertifiedBlock)
    unless (expectedCurrentRound == rs ^. rsCurrentRound) $
        throwM . TreeStateInvariantViolation $
            "The current round does not match the expected round."
    -- Add the latest timeout message to the timeout messages if it makes sense in the current
    -- context.
    -- Note: we do not restore the latest quorum message because if it is for the current
    -- round, then we won't have the block that it is signing, since only certified blocks are
    -- stored in the database.
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

    -- Set the previous round timeout.
    setLastTimeout lastTimeout certBlock = do
        roundStatus . rsPreviousRoundTimeout
            .= Present
                RoundTimeout
                    { rtTimeoutCertificate = lastTimeout,
                      rtCertifiedBlock = certBlock
                    }
        roundStatus . rsCurrentRound .= tcRound lastTimeout + 1
    -- Attempting to load the last timeout failed because we could not find a consistent last
    -- certified block. This can occur if some blocks were rolled back.
    failLoadLastTimeout = do
        roundStatus . rsRoundEligibleToBake .= False
        logEvent Skov LLWarning "Missing certified block consistent with last timeout certificate"
