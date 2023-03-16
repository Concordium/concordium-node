{-# LANGUAGE TemplateHaskell #-}

module Concordium.GlobalState.Statistics where

import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Lens.Micro.Platform

import Concordium.Utils

-- |Weight factor to use in computing exponentially-weighted moving averages.
emaWeight :: Double
emaWeight = 0.1

-- |Statistics for the consensus layer
data ConsensusStatistics = ConsensusStatistics
    { -- |Total number of blocks received
      _blocksReceivedCount :: !Int,
      -- |Total number of blocks received and verified
      _blocksVerifiedCount :: !Int,
      -- |The last time a block was received
      _blockLastReceived :: !(Maybe UTCTime),
      -- |Moving average latency between a block's slot time and received time
      _blockReceiveLatencyEMA :: !Double,
      -- |Variance of '_blockReceiveLatencyEMA'
      _blockReceiveLatencyEMVar :: !Double,
      -- |Moving average time between receiving blocks
      _blockReceivePeriodEMA :: !(Maybe Double),
      -- |Variance of '_blockReceivePeriodEMA'
      _blockReceivePeriodEMVar :: !(Maybe Double),
      -- |The last time a block was verified (added to the tree)
      _blockLastArrive :: !(Maybe UTCTime),
      -- |Moving average latency between a block's slot time and its arrival
      _blockArriveLatencyEMA :: !Double,
      -- |Variance of '_blockArriveLatencyEMA'
      _blockArriveLatencyEMVar :: !Double,
      -- |Moving average time between block arrivals
      _blockArrivePeriodEMA :: !(Maybe Double),
      -- |Variance of '_blockArrivePeriodEMA'
      _blockArrivePeriodEMVar :: !(Maybe Double),
      -- |Moving average transactions per block
      _transactionsPerBlockEMA :: !Double,
      -- |Variance of '_transactionsPerBlockEMA'
      _transactionsPerBlockEMVar :: !Double,
      -- |Number of finalizations
      _finalizationCount :: !Int,
      -- |Time of last verified finalization
      _lastFinalizedTime :: !(Maybe UTCTime),
      -- |Moving average time between finalizations
      _finalizationPeriodEMA :: !(Maybe Double),
      -- |Variance of _finalizationPeriodEMA
      _finalizationPeriodEMVar :: !(Maybe Double)
    }

makeLenses ''ConsensusStatistics

-- |Initial statistics.
initialConsensusStatistics :: ConsensusStatistics
initialConsensusStatistics =
    ConsensusStatistics
        { _blocksReceivedCount = 0,
          _blocksVerifiedCount = 0,
          _blockLastReceived = Nothing,
          _blockReceiveLatencyEMA = 0,
          _blockReceiveLatencyEMVar = 0,
          _blockReceivePeriodEMA = Nothing,
          _blockReceivePeriodEMVar = Nothing,
          _blockLastArrive = Nothing,
          _blockArriveLatencyEMA = 0,
          _blockArriveLatencyEMVar = 0,
          _blockArrivePeriodEMA = Nothing,
          _blockArrivePeriodEMVar = Nothing,
          _transactionsPerBlockEMA = 0,
          _transactionsPerBlockEMVar = 0,
          _finalizationCount = 0,
          _lastFinalizedTime = Nothing,
          _finalizationPeriodEMA = Nothing,
          _finalizationPeriodEMVar = Nothing
        }

-- |Update the statistics when a block becomes fully validated (arrives).
updateStatsOnArrive ::
    -- |Nominal block time.
    UTCTime ->
    -- |Block arrival time.
    UTCTime ->
    -- |Number of transactions in the block.
    Int ->
    ConsensusStatistics ->
    ConsensusStatistics
updateStatsOnArrive nominalTime arrivalTime transactionCount =
    updateTransactionsPerBlock . updatePeriod . updateLatency . (blocksVerifiedCount +~ 1)
  where
    updateLatency s =
        s
            & (blockArriveLatencyEMA .~ oldEMA + emaWeight * delta)
            & (blockArriveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
      where
        oldEMA = s ^. blockArriveLatencyEMA
        delta = realToFrac (diffUTCTime arrivalTime nominalTime) - oldEMA

    updatePeriod s =
        case s ^. blockLastArrive of
            Nothing -> s & blockLastArrive ?~! arrivalTime
            Just lastBTime ->
                s
                    & (blockLastArrive ?~! arrivalTime)
                    & (blockArrivePeriodEMA ?~! oldEMA + emaWeight * delta)
                    & (blockArrivePeriodEMVar ?~! (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
              where
                blockTime = realToFrac (diffUTCTime arrivalTime lastBTime)
                oldEMA = fromMaybe blockTime (s ^. blockArrivePeriodEMA)
                delta = blockTime - oldEMA
                oldEMVar = fromMaybe 0 (s ^. blockArrivePeriodEMVar)

    updateTransactionsPerBlock s =
        s
            & (transactionsPerBlockEMA .~ oldEMA + emaWeight * delta)
            & (transactionsPerBlockEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
      where
        oldEMA = s ^. transactionsPerBlockEMA
        delta = fromIntegral transactionCount - oldEMA

-- |Render the statistics relating to block arrivals as a string.
showArriveStatistics :: ConsensusStatistics -> String
showArriveStatistics s =
    "Arrive statistics:"
        ++ " blocksVerifiedCount="
        ++ show (s ^. blocksVerifiedCount)
        ++ " blockLastArrive="
        ++ show (maybe (0 :: Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastArrive)
        ++ " blockArriveLatencyEMA="
        ++ show (s ^. blockArriveLatencyEMA)
        ++ " blockArriveLatencyEMSD="
        ++ show (sqrt $ s ^. blockArriveLatencyEMVar)
        ++ " blockArrivePeriodEMA="
        ++ show (s ^. blockArrivePeriodEMA)
        ++ " blockArrivePeriodEMSD="
        ++ show (sqrt <$> s ^. blockArrivePeriodEMVar)
        ++ " transactionsPerBlockEMA="
        ++ show (s ^. transactionsPerBlockEMA)
        ++ " transactionsPerBlockEMSD="
        ++ show (sqrt $ s ^. transactionsPerBlockEMVar)

-- |Update the statistics when a block is received by the consensus.
updateStatsOnReceive ::
    -- |Nominal block time.
    UTCTime ->
    -- |Block receive time.
    UTCTime ->
    ConsensusStatistics ->
    ConsensusStatistics
updateStatsOnReceive nominalTime receiveTime =
    updatePeriod . updateLatency . (blocksReceivedCount +~ 1)
  where
    updateLatency s =
        s
            & (blockReceiveLatencyEMA .~ oldEMA + emaWeight * delta)
            & (blockReceiveLatencyEMVar %~ \oldEMVar -> (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
      where
        oldEMA = s ^. blockReceiveLatencyEMA
        delta = realToFrac (diffUTCTime receiveTime nominalTime) - oldEMA
    updatePeriod s =
        case s ^. blockLastReceived of
            Nothing -> s & blockLastReceived ?~! receiveTime
            Just lastBTime ->
                s
                    & (blockLastReceived ?~! receiveTime)
                    & (blockReceivePeriodEMA ?~! oldEMA + emaWeight * delta)
                    & (blockReceivePeriodEMVar ?~! (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
              where
                blockTime = realToFrac (diffUTCTime receiveTime lastBTime)
                oldEMA = fromMaybe blockTime (s ^. blockReceivePeriodEMA)
                delta = blockTime - oldEMA
                oldEMVar = fromMaybe 0 (s ^. blockReceivePeriodEMVar)

-- |Render the statistics related to receiving blocks as a string.
showReceiveStatistics :: ConsensusStatistics -> String
showReceiveStatistics s =
    "Receive statistics:"
        ++ " blocksReceivedCount="
        ++ show (s ^. blocksReceivedCount)
        ++ " blockLastReceived="
        ++ show (maybe (0 :: Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. blockLastReceived)
        ++ " blockReceiveLatencyEMA="
        ++ show (s ^. blockReceiveLatencyEMA)
        ++ " blockReceiveLatencyEMSD="
        ++ show (sqrt $ s ^. blockReceiveLatencyEMVar)
        ++ " blockReceivePeriodEMA="
        ++ show (s ^. blockReceivePeriodEMA)
        ++ " blockReceivePeriodEMSD="
        ++ show (sqrt <$> s ^. blockReceivePeriodEMVar)

-- |Update the statistics when a block becomes (explicitly) finalized.
updateStatsOnFinalize ::
    -- |Current time
    UTCTime ->
    ConsensusStatistics ->
    ConsensusStatistics
updateStatsOnFinalize curTime =
    (lastFinalizedTime ?~! curTime) . updatePeriod . (finalizationCount +~ 1)
  where
    updatePeriod s
        | Just lastFinTime <- s ^. lastFinalizedTime =
            let
                finTime = realToFrac (diffUTCTime curTime lastFinTime)
                oldEMA = fromMaybe finTime (s ^. finalizationPeriodEMA)
                delta = finTime - oldEMA
                oldEMVar = fromMaybe 0 (s ^. finalizationPeriodEMVar)
            in
                s
                    & (finalizationPeriodEMA ?~! oldEMA + emaWeight * delta)
                    & (finalizationPeriodEMVar ?~! (1 - emaWeight) * (oldEMVar + emaWeight * delta * delta))
        | otherwise = s

-- |Render the statistics related to finalization.
showFinalizationStatistics :: ConsensusStatistics -> String
showFinalizationStatistics s =
    "Finalization statistics:"
        ++ " finalizationCount="
        ++ show (s ^. finalizationCount)
        ++ " lastFinalizedTime="
        ++ show (maybe (0 :: Double) (realToFrac . utcTimeToPOSIXSeconds) $ s ^. lastFinalizedTime)
        ++ " finalizationPeriodEMA="
        ++ show (s ^. finalizationPeriodEMA)
        ++ " finalizationPeriodEMSD="
        ++ show (sqrt <$> s ^. finalizationPeriodEMVar)
