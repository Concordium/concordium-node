{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.StartUp where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Statistics as Stats
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.SeedState (shutdownTriggered)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

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
        LowLevel.lookupLastBlock >>= \case
            Nothing -> throwM . TreeStateInvariantViolation $ "Missing last block in database"
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
    let lastSignedQMRound =
            ofOption 0 qmRound $ _prsLastSignedQuorumMessage _persistentRoundStatus
    let lastSignedTMRound =
            ofOption 0 (tmRound . tmBody) $ _prsLastSignedTimeoutMessage _persistentRoundStatus
    let currentRound =
            maximum
                [ maybe 1 ((1 +) . qcRound . feSuccessorQuorumCertificate) mLatestFinEntry,
                  lastSignedQMRound,
                  lastSignedTMRound
                ]
    let currentEpoch = blockEpoch lastFinBlock
    chainParams <- getChainParameters $ bpState lastFinBlock
    let _roundStatus =
            RoundStatus
                { _rsCurrentRound = currentRound,
                  _rsHighestCertifiedBlock = _rsHighestCertifiedBlock,
                  _rsPreviousRoundTimeout = Absent,
                  _rsRoundEligibleToBake = True,
                  _rsCurrentEpoch = currentEpoch,
                  _rsLastEpochFinalizationEntry = Absent,
                  _rsCurrentTimeout =
                    chainParams
                        ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
                }
    let _blockTable = emptyBlockTable
    let _branches = Seq.empty
    let _roundExistingBlocks = Map.empty
    let _roundExistingQCs = Map.empty
    _genesisMetadata <- case bpBlock genesisBlock of
        GenesisBlock gm -> return gm
        _ -> throwM . TreeStateInvariantViolation $ "First block is not a genesis block"
    let _skovPendingBlocks = emptyPendingBlocks
    let _lastFinalized = lastFinBlock
    -- When loading, we currently do not have the finalizing certified block.
    -- TODO: When the database storage is modified to allow this, load the block. Issue #843
    let _finalizingCertifiedBlock = Absent
    _skovEpochBakers <- makeEpochBakers lastFinBlock
    finBlockSeedstate <- getSeedState $ bpState lastFinBlock
    let _currentTimeoutMessages = case _prsLastSignedTimeoutMessage _persistentRoundStatus of
            Absent -> Absent
            Present tm ->
                if tmRound (tmBody tm) == currentRound
                    then
                        Present $
                            TimeoutMessages
                                { tmFirstEpoch = currentEpoch,
                                  tmFirstEpochTimeouts = Map.singleton (tmFinalizerIndex $ tmBody tm) tm,
                                  tmSecondEpochTimeouts = Map.empty
                                }
                    else Absent
    let _currentQuorumMessages = emptyQuorumMessages
    let _transactionTable = TT.emptyTransactionTable
    let _transactionTablePurgeCounter = 0
    let _skovPendingTransactions =
            PendingTransactions
                { _pendingTransactionTable = TT.emptyPendingTransactionTable,
                  _focusBlock = lastFinBlock
                }
    let _statistics = Stats.initialConsensusStatistics
        _isConsensusShutdown = finBlockSeedstate ^. shutdownTriggered
    return SkovData{..}
