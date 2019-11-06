{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Skov.Statistics where

import Lens.Micro.Platform
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX

import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Statistics

import Concordium.Skov.Monad
import Concordium.Logger
import Concordium.TimeMonad

-- | Called when a block is fully validated (arrives) to update the statistics.
updateArriveStatistics :: forall m. (LoggerMonad m, TreeStateMonad m, SkovQueryMonad m) => BlockPointer m -> m ()
updateArriveStatistics bp = do
        s0 <- getConsensusStatistics
        let s1 = s0 & blocksVerifiedCount +~ 1
        s2 <- updateLatency s1
        let s3 = updatePeriod s2
        let s = updateTransactionsPerBlock s3
        putConsensusStatistics s
        logEvent Skov LLInfo $ "Arrive statistics:" ++
            " blocksVerifiedCount=" ++ show (s ^. blocksVerifiedCount) ++
            " blockLastArrive=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastArrive) ++
            " blockArriveLatencyEMA=" ++ show (s ^. blockArriveLatencyEMA) ++
            " blockArriveLatencyEMSD=" ++ show (sqrt $ s ^. blockArriveLatencyEMVar) ++
            " blockArrivePeriodEMA=" ++ show (s ^. blockArrivePeriodEMA) ++
            " blockArrivePeriodEMSD=" ++ show (sqrt <$> s ^. blockArrivePeriodEMVar) ++
            " transactionsPerBlockEMA=" ++ show (s ^. transactionsPerBlockEMA) ++
            " transactionsPerBlockEMSD=" ++ show (sqrt $ s ^. transactionsPerBlockEMVar)
    where
        curTime = bpArriveTime bp
        updateLatency s = do
            slotTime <- getSlotTime (blockSlot bp)
            let 
                oldEMA = s ^. blockArriveLatencyEMA
                delta = realToFrac (diffUTCTime curTime slotTime) - oldEMA
            return $
                s & (blockArriveLatencyEMA .~ oldEMA + emaWeight * delta)
                  & (blockArriveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updatePeriod s = 
            case s ^. blockLastArrive of
                Nothing -> s & blockLastArrive ?~ curTime
                Just lastBTime -> 
                    let 
                        blockTime = realToFrac (diffUTCTime curTime lastBTime)
                        oldEMA = fromMaybe blockTime (s ^. blockArrivePeriodEMA)
                        delta = blockTime - oldEMA
                        oldEMVar = fromMaybe 0 (s ^. blockArrivePeriodEMVar)
                    in
                        s & (blockLastArrive ?~ curTime)
                            & (blockArrivePeriodEMA ?~ oldEMA + emaWeight * delta)
                            & (blockArrivePeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updateTransactionsPerBlock s =
            let
                oldEMA = s ^. transactionsPerBlockEMA
                delta = fromIntegral (bpTransactionCount bp) - oldEMA
            in
                s & (transactionsPerBlockEMA .~ oldEMA + emaWeight * delta)
                  & (transactionsPerBlockEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))

-- | Called when a block is received to update the statistics.
updateReceiveStatistics :: forall m. (TreeStateMonad m, LoggerMonad m, SkovQueryMonad m) => PendingBlock m -> m ()
updateReceiveStatistics pb = do
        s0 <- getConsensusStatistics
        let s1 = s0 & blocksReceivedCount +~ 1
        s2 <- updateLatency s1
        let s = updatePeriod s2
        putConsensusStatistics s
        logEvent Skov LLInfo $ "Receive statistics:" ++
            " blocksReceivedCount=" ++ show (s ^. blocksReceivedCount) ++
            " blockLastReceived=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastReceived) ++
            " blockReceiveLatencyEMA=" ++ show (s ^. blockReceiveLatencyEMA) ++
            " blockReceiveLatencyEMSD=" ++ show (sqrt $ s ^. blockReceiveLatencyEMVar) ++
            " blockReceivePeriodEMA=" ++ show (s ^. blockReceivePeriodEMA) ++
            " blockReceivePeriodEMSD=" ++ show (sqrt <$> s ^. blockReceivePeriodEMVar)
    where
        updateLatency s = do
            slotTime <- getSlotTime (blockSlot pb)
            let
                oldEMA = s ^. blockReceiveLatencyEMA
                delta = realToFrac (diffUTCTime (blockReceiveTime pb) slotTime) - oldEMA
            return $
                s & (blockReceiveLatencyEMA .~ oldEMA + emaWeight * delta)
                  & (blockReceiveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        updatePeriod s = 
            case s ^. blockLastReceived of
                Nothing -> s & blockLastReceived ?~ blockReceiveTime pb
                Just lastBTime ->
                    let
                        blockTime = realToFrac (diffUTCTime (blockReceiveTime pb) lastBTime)
                        oldEMA = fromMaybe blockTime (s ^. blockReceivePeriodEMA)
                        delta = blockTime - oldEMA
                        oldEMVar = fromMaybe 0 (s ^. blockReceivePeriodEMVar)
                    in
                        s & (blockLastReceived ?~ blockReceiveTime pb)
                          & (blockReceivePeriodEMA ?~ oldEMA + emaWeight * delta)
                          & (blockReceivePeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))

-- | Called when a block has been finalized to update the statistics.        
updateFinalizationStatistics :: forall m. (TreeStateMonad m, LoggerMonad m, TimeMonad m) => m ()
updateFinalizationStatistics = do
        s0 <- getConsensusStatistics
        let s1 = s0 & finalizationCount +~ 1
        curTime <- currentTime
        let s = case (s1 ^. lastFinalizedTime) of
                Nothing -> s1 & lastFinalizedTime ?~ curTime
                Just lastFinTime ->
                    let
                        finTime = realToFrac (diffUTCTime curTime lastFinTime)
                        oldEMA = fromMaybe finTime (s1 ^. finalizationPeriodEMA)
                        delta = finTime - oldEMA
                        oldEMVar = fromMaybe 0 (s1 ^. finalizationPeriodEMVar)
                    in
                        s1 & (lastFinalizedTime ?~ curTime)
                           & (finalizationPeriodEMA ?~ oldEMA + emaWeight * delta)
                           & (finalizationPeriodEMVar ?~ (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        putConsensusStatistics s
        logEvent Skov LLInfo $ "Finalization statistics:" ++
            " finalizationCount=" ++ show (s ^. finalizationCount) ++
            " lastFinalizedTime=" ++ show (maybe (0::Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. lastFinalizedTime) ++
            " finalizationPeriodEMA=" ++ show (s ^. finalizationPeriodEMA) ++
            " finalizationPeriodEMSD=" ++ show (sqrt <$> s ^. finalizationPeriodEMVar)
