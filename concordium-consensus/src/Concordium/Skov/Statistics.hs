module Concordium.Skov.Statistics where

import Concordium.GlobalState.BlockPointer (bpArriveTime, bpTransactionCount)
import Concordium.GlobalState.Statistics
import Concordium.GlobalState.TreeState

import Concordium.Logger
import Concordium.Skov.Monad hiding (getConsensusStatistics)
import Concordium.TimeMonad

-- | Called when a block is fully validated (arrives) to update the statistics.
updateArriveStatistics :: (MonadLogger m, TreeStateMonad m, SkovQueryMonad m) => BlockPointerType m -> m ()
updateArriveStatistics bp = do
    nominalTime <- getSlotTime (blockSlot bp)
    s <-
        updateStatsOnArrive nominalTime (bpArriveTime bp) (bpTransactionCount bp)
            <$> getConsensusStatistics
    putConsensusStatistics s
    logEvent Skov LLInfo $ showArriveStatistics s

-- | Called when a block is received to update the statistics.
updateReceiveStatistics :: (TreeStateMonad m, MonadLogger m, SkovQueryMonad m) => PendingBlock -> m ()
updateReceiveStatistics pb = do
    nominalTime <- getSlotTime (blockSlot pb)
    s <- updateStatsOnReceive nominalTime (blockReceiveTime pb) <$> getConsensusStatistics
    putConsensusStatistics s
    logEvent Skov LLInfo $ showReceiveStatistics s

-- | Called when a block has been finalized to update the statistics.
updateFinalizationStatistics :: (TreeStateMonad m, MonadLogger m, TimeMonad m) => m ()
updateFinalizationStatistics = do
    curTime <- currentTime
    s <- updateStatsOnFinalize curTime <$> getConsensusStatistics
    putConsensusStatistics s
    logEvent Skov LLInfo $ showFinalizationStatistics s
