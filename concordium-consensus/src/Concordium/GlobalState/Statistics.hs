{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Statistics where

import Lens.Micro.Platform
import Data.Time

-- |Weight factor to use in computing exponentially-weighted moving averages.
emaWeight :: Double
emaWeight = 0.1

-- |Statistics for the consensus layer
data ConsensusStatistics = ConsensusStatistics {
    -- |Total number of blocks received
    _blocksReceivedCount :: Int,
    -- |Total number of blocks received and verified
    _blocksVerifiedCount :: Int,
    -- |The last time a block was received
    _blockLastReceived :: Maybe UTCTime,
    -- |Moving average latency between a block's slot time and received time
    _blockReceiveLatencyEMA :: Double,
    -- |Variance of '_blockReceiveLatencyEMA'
    _blockReceiveLatencyEMVar :: Double,
    -- |Moving average time between receiving blocks
    _blockReceivePeriodEMA :: Maybe Double,
    -- |Variance of '_blockReceivePeriodEMA'
    _blockReceivePeriodEMVar :: Maybe Double,
    -- |The last time a block was verified (added to the tree)
    _blockLastArrive :: Maybe UTCTime,
    -- |Moving average latency between a block's slot time and its arrival
    _blockArriveLatencyEMA :: Double,
    -- |Variance of '_blockArriveLatencyEMA'
    _blockArriveLatencyEMVar :: Double,
    -- |Moving average time between block arrivals
    _blockArrivePeriodEMA :: Maybe Double,
    -- |Variance of '_blockArrivePeriodEMA'
    _blockArrivePeriodEMVar :: Maybe Double,
    -- |Moving average transactions per block
    _transactionsPerBlockEMA :: Double,
    -- |Variance of '_transactionsPerBlockEMA'
    _transactionsPerBlockEMVar :: Double,
    -- |Number of finalizations
    _finalizationCount :: Int,
    -- |Time of last verified finalization
    _lastFinalizedTime :: Maybe UTCTime,
    -- |Moving average time between finalizations
    _finalizationPeriodEMA :: Maybe Double,
    -- |Variance of _finalizationPeriodEMA
    _finalizationPeriodEMVar :: Maybe Double
}
makeLenses ''ConsensusStatistics

-- |Initial statistics.
initialConsensusStatistics :: ConsensusStatistics
initialConsensusStatistics = ConsensusStatistics {
    _blocksReceivedCount = 0,
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
