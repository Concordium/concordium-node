{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Tests for the payday-related functionality.
module SchedulerTests.Payday where

import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.TreeStateEnvironment
import Concordium.Scheduler.Types
import Concordium.Types.DummyData
import Concordium.Types.SeedState

import Concordium.GlobalState.BlockState
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

tests :: Spec
tests = describe "Payday" $ do
    describe "Minting" testDoMintingP4
