{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Tests for the payday-related functionality.
module SchedulerTests.Payday where

import Data.Ratio
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.TreeStateEnvironment
import Concordium.Scheduler.Types
import Concordium.Types.DummyData
import Concordium.Types.SeedState

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.CapitalDistribution
import GlobalStateMock

foundationAccount :: AccountAddress
foundationAccount = accountAddressFrom 0

testDoMintingP4 :: Spec
testDoMintingP4 = do
    it "no updates" $ assertEqual "" (runMock (events mintAmts0) (op [])) result
    it "last minute update" $ assertEqual "" (runMock (events mintAmts1) (op [(400, UVMintDistribution md1)])) result
    it "late update" $ assertEqual "" (runMock (events mintAmts0) (op [(401, UVMintDistribution md1)])) result
    it "two updates" $ assertEqual "" (runMock (events mintAmts2) (op [(0, UVMintDistribution md1), (100, UVMintDistribution md2)])) result
    it "two updates, one late" $ assertEqual "" (runMock (events mintAmts1) (op [(0, UVMintDistribution md1), (401, UVMintDistribution md2)])) result
    -- Note: the minting should not be affected by any updates to the time parameters
    it "mint rate update" $ assertEqual "" (runMock (events mintAmts0) (op [(0, UVTimeParameters (TimeParametersV1 100 (MintRate 2 0)))])) result
  where
    events :: MintAmounts -> [WithResult (Action 'P4)]
    events amts =
        [ BSO (BsoGetBankStatus (bs 0)) :-> bank,
          BSO (BsoGetSeedState (bs 0)) :-> seedState,
          BSO (BsoMint (bs 0) amts) :-> bs 1,
          BSO (BsoAddSpecialTransactionOutcome (bs 1) (mintSto amts)) :-> bs 2
        ]
    op upds = doMintingP4 dummyChainParameters targetEpoch mintRate foundationAccount upds (bs 0)
    result = bs 2
    bs = MockUpdatableBlockState
    mintRate = MintRate 1 0 -- 100% mint rate
    bank = emptyBankStatus{_totalGTU = 1000000000}
    targetEpoch = 4
    epochLength = 100
    seedState = initialSeedState (Hash.hash "NONCE") epochLength
    mintAmts0 =
        MintAmounts
            { mintBakingReward = 600000000,
              mintFinalizationReward = 300000000,
              mintDevelopmentCharge = 100000000
            }
    mintAmts1 =
        MintAmounts
            { mintBakingReward = 1000000000,
              mintFinalizationReward = 0,
              mintDevelopmentCharge = 0
            }
    mintAmts2 =
        MintAmounts
            { mintBakingReward = 0,
              mintFinalizationReward = 1000000000,
              mintDevelopmentCharge = 0
            }
    md1 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 100000) (makeAmountFraction 0)
    md2 = MintDistribution MintPerSlotForCPV0None (makeAmountFraction 0) (makeAmountFraction 100000)
    mintSto amts =
        Mint
            { stoMintBakingReward = mintBakingReward amts,
              stoMintFinalizationReward = mintFinalizationReward amts,
              stoMintPlatformDevelopmentCharge = mintDevelopmentCharge amts,
              stoFoundationAccount = foundationAccount
            }

-- |Test that 'scaleAmount' performs correctly by multiplying by the denominator and adding the remainder.
testScaleAmount1 :: Property
testScaleAmount1 = property $ \(a :: Amount) b c ->
    let num = min a b
        den = max 1 (max a b)
        cTimesNum = toInteger c * toInteger num
     in toInteger (scaleAmount num den c) * toInteger den + cTimesNum `mod` toInteger den === cTimesNum

-- |Test that 'scaleAmount' performs correctly
testScaleAmount2 :: Property
testScaleAmount2 = property $ \(a :: Amount) b c ->
    let num = min a b
        den = max 1 (max a b)
     in scaleAmount num den c === floor ((toInteger num % toInteger den) * toRational c)

-- |A test case for 'rewardDelegators'.
data DelegatorRewardTestCase = DelegatorRewardTestCase
    { -- |Name to identify the case
      drtcName :: String,
      -- |Baking reward distributed to delegators
      drtcBakingReward :: Amount,
      -- |Finalization reward distributed to delegators
      drtcFinalizationReward :: Amount,
      -- |Transaction fee reward distributed to delegators
      drtcTransactionFeeReward :: Amount,
      -- |Capital held by the pool owner
      drtcBakerCapital :: Amount,
      -- |Capital held by each delegator
      drtcDelegatorCapitals :: [Amount],
      -- |Expected baking, finalization and transaction fee rewards to each delegator.
      drtcDelegatorExpectedRewards :: [(Amount, Amount, Amount)]
    }

-- |Test cases for 'rewardDelegators'.
drtcs :: [DelegatorRewardTestCase]
drtcs =
    [ DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator",
          drtcBakingReward = 1_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for precision in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator (2)",
          drtcBakingReward = 1_000_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [999_999_999_999_999_999],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999_999, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for precision in distributing the baking reward
          drtcName = "big baker reward, 1 big delegator (3)",
          drtcBakingReward = 1_000_000_000_000_000_000,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [999_999_999_999_999_998],
          drtcDelegatorExpectedRewards = [(999_999_999_999_999_998, 0, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the finalization reward
          drtcName = "big finalization reward, 1 big delegator",
          drtcBakingReward = 0,
          drtcFinalizationReward = 1_000_000_000_000_000,
          drtcTransactionFeeReward = 0,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(0, 999_999_999_999_999, 0)]
        },
      DelegatorRewardTestCase
        { -- This tests for potential overflow in distributing the transaction fee reward
          drtcName = "big transaction fee reward, 1 big delegator",
          drtcBakingReward = 0,
          drtcFinalizationReward = 0,
          drtcTransactionFeeReward = 1_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards = [(0, 0, 999_999_999_999_999)]
        },
      DelegatorRewardTestCase
        { drtcName = "big rewards, 1 big delegator",
          drtcBakingReward = 1_000_000_000_000_000,
          drtcFinalizationReward = 1_000_000_000_000_000,
          drtcTransactionFeeReward = 1_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000],
          drtcDelegatorExpectedRewards =
            [ (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999)
            ]
        },
      DelegatorRewardTestCase
        { drtcName = "big rewards, 2 big delegators",
          drtcBakingReward = 2_000_000_000_000_000,
          drtcFinalizationReward = 2_000_000_000_000_000,
          drtcTransactionFeeReward = 2_000_000_000_000_000,
          drtcBakerCapital = 1,
          drtcDelegatorCapitals = [1_000_000_000_000_000, 1_000_000_000_000_000],
          drtcDelegatorExpectedRewards =
            [ (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999),
              (999_999_999_999_999, 999_999_999_999_999, 999_999_999_999_999)
            ]
        }
    ]

testRewardDelegators :: Spec
testRewardDelegators = describe "rewardDelegators" $ mapM_ p drtcs
  where
    p DelegatorRewardTestCase{..} = it drtcName $ do
        let (DelegatorRewardOutcomes{..}, _) =
                runMock events $
                    rewardDelegators (bs 0) drtcFinalizationReward drtcBakingReward drtcTransactionFeeReward totCap dels
        assertEqual "Total baking rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _1) _delegatorAccumBaking
        assertEqual "Total finalization rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _2) _delegatorAccumFinalization
        assertEqual "Total transaction fee rewards" (sum $ drtcDelegatorExpectedRewards ^.. each . _3) _delegatorAccumTransaction
        let makeSTOs delid (bak, fin, tran) =
                PaydayAccountReward
                    { stoAccount = accountAddressFrom delid,
                      stoBakerReward = bak,
                      stoFinalizationReward = fin,
                      stoTransactionFees = tran
                    }
        assertEqual "Special transaction outcomes" (zipWith makeSTOs [0 ..] drtcDelegatorExpectedRewards) (_delegatorOutcomes ^.. each)
      where
        bs = MockUpdatableBlockState
        events :: [WithResult (Action 'P4)]
        events =
            zipWith
                ( \i amts ->
                    BSO (BsoRewardAccount (bs i) (fromIntegral i) (sum (amts ^.. each)))
                        :-> (Just (accountAddressFrom (fromIntegral i)), bs (i + 1))
                )
                [0 ..]
                drtcDelegatorExpectedRewards
        totCap = drtcBakerCapital + sum (dcDelegatorCapital <$> dels)
        dels = Vec.fromList (zipWith DelegatorCapital [0 ..] drtcDelegatorCapitals)

tests :: Spec
tests = describe "Payday" $ do
    describe "Minting" testDoMintingP4
    describe "scaleAmount" $ do
        it "div-mod" testScaleAmount1
        it "via Rational" testScaleAmount2
    testRewardDelegators
