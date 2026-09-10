-- | Tests for consensus statistics updates.
module ConcordiumTests.GlobalState.Statistics (tests) where

import Data.Time
import Lens.Micro.Platform
import Test.Hspec

import Concordium.GlobalState.Statistics

-- | Register consensus-statistics tests.
tests :: Spec
tests = describe "GlobalState.Statistics" $ do
    it "preserves receive latency metrics for future nominal timestamps" $ do
        let
            firstReceiveTime = addUTCTime 10 baseTime
            futureNominalTime = addUTCTime 30 baseTime
            futureReceiveTime = addUTCTime 20 baseTime
            initialStats = updateStatsOnReceive baseTime firstReceiveTime initialConsensusStatistics
            updatedStats = updateStatsOnReceive futureNominalTime futureReceiveTime initialStats
        updatedStats ^. blockReceiveLatencyEMA `shouldBe` initialStats ^. blockReceiveLatencyEMA
        updatedStats ^. blockReceiveLatencyEMVar `shouldBe` initialStats ^. blockReceiveLatencyEMVar
        updatedStats ^. blocksReceivedCount `shouldBe` 2
        updatedStats ^. blockLastReceived `shouldBe` Just futureReceiveTime
        updatedStats ^. blockReceivePeriodEMA `shouldBe` Just 10
        updatedStats ^. blockReceivePeriodEMVar `shouldBe` Just 0

    it "preserves arrival latency metrics for future nominal timestamps" $ do
        let
            firstArrivalTime = addUTCTime 10 baseTime
            futureNominalTime = addUTCTime 30 baseTime
            futureArrivalTime = addUTCTime 20 baseTime
            initialStats = updateStatsOnArrive baseTime firstArrivalTime 10 initialConsensusStatistics
            updatedStats = updateStatsOnArrive futureNominalTime futureArrivalTime 20 initialStats
        updatedStats ^. blockArriveLatencyEMA `shouldBe` initialStats ^. blockArriveLatencyEMA
        updatedStats ^. blockArriveLatencyEMVar `shouldBe` initialStats ^. blockArriveLatencyEMVar
        updatedStats ^. blocksVerifiedCount `shouldBe` 2
        updatedStats ^. blockLastArrive `shouldBe` Just futureArrivalTime
        updatedStats ^. blockArrivePeriodEMA `shouldBe` Just 10
        updatedStats ^. blockArrivePeriodEMVar `shouldBe` Just 0
        updatedStats ^. transactionsPerBlockEMA `shouldNotBe` initialStats ^. transactionsPerBlockEMA
        updatedStats ^. transactionsPerBlockEMVar `shouldNotBe` initialStats ^. transactionsPerBlockEMVar

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2024 1 1) 0
