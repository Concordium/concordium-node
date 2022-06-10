{-| Test that reducing delegation and removing delegators always works, regardless
  of whether the new stake would violate any of the cap bounds.

  This currently only tests with the basic state implementation which is not
  ideal. The test should be expanded to also use the persistent state implementation.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module SchedulerTests.Delegation where

import Lens.Micro.Platform

import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import qualified  Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.Types.Accounts
import Concordium.ID.Types as ID

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import SchedulerTests.TestUtils
import System.Random
import Test.Hspec
import Test.HUnit (assertEqual)

-- |Account of the baker.
bakerAddress :: AccountAddress
bakerAddress = accountAddressFrom 16

-- |Account keys of the baker account.
bakerKP :: SigScheme.KeyPair
bakerKP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 16)

bakerVK :: SigScheme.VerifyKey
bakerVK = SigScheme.correspondingVerifyKey bakerKP

bakerAccount :: Account 'AccountV1
bakerAccount = (mkAccount @'AccountV1 bakerVK bakerAddress 1_000_000) {
  _accountStaking = AccountStakeBaker AccountBaker {
    _stakedAmount = 1_000_000,
    _stakeEarnings = True,
    _accountBakerInfo = BakerInfoExV1 {
        _bieBakerInfo = mkFullBaker 16 0 ^. _1 . theBakerInfo,
        _bieBakerPoolInfo = BakerPoolInfo {
            _poolOpenStatus = ClosedForAll,
            _poolMetadataUrl = UrlText "Some URL",
            _poolCommissionRates = CommissionRates {
                _finalizationCommission = makeAmountFraction 50_000,
                _bakingCommission = makeAmountFraction 50_000,
                _transactionCommission = makeAmountFraction 50_000
                }
            }
        },
      _bakerPendingChange = NoChange
    }
  }

-- |Account of the delegator
delegatorAddress :: AccountAddress
delegatorAddress = accountAddressFrom 17

-- |Account keys of the delegator account.
delegatorKP :: SigScheme.KeyPair
delegatorKP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 17)

delegatorVK :: SigScheme.VerifyKey
delegatorVK = SigScheme.correspondingVerifyKey delegatorKP

delegatorAccount :: Account 'AccountV1
delegatorAccount = (mkAccount @'AccountV1 delegatorVK delegatorAddress 20_000_000) {
  _accountStaking = AccountStakeDelegate AccountDelegationV1 {
      _delegationIdentity = 1,
      _delegationStakedAmount = 19_000_000, -- leverage cap is set to 5 in createBlockState, so this puts it over the cap.
      _delegationStakeEarnings = False,
      _delegationTarget = DelegateToBaker 0,
      _delegationPendingChange = NoChange
      }
  }

-- |Create initial block state with account index 0 being the baker, and account index 1 being
-- the delegator that delegates to the baker.
initialBlockState :: BlockState PV4
initialBlockState = 
    createBlockState (Acc.putAccountWithRegIds delegatorAccount (Acc.putAccountWithRegIds bakerAccount Acc.emptyAccounts))

-- Test removing a delegator even if the stake is over the threshold.
testCases1 :: [TestCase PV4]
testCases1 =
  [ TestCase
    { tcName = "Delegate"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 0,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Nothing
                            },
                          metadata = makeDummyHeader delegatorAddress 1 1_000,
                          keys = [(0,[(0, delegatorKP)])]
                        }
        , (Success (assertEqual "Remove delegation" [DelegationRemoved 1 delegatorAddress]), const (return ()))
        )
      ]
    }
  ]

-- Test reducing delegator stake in such a way that it stays above the cap threshold.
testCases2 :: [TestCase PV4]
testCases2 =
  [ TestCase
    { tcName = "Delegate"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 18_999_999,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Nothing
                            },
                          metadata = makeDummyHeader delegatorAddress 1 1_000,
                          keys = [(0,[(0, delegatorKP)])]
                        }
        , (Success (assertEqual "Reduce delegation stake" [DelegationStakeDecreased 1 delegatorAddress 18_999_999]), const (return ()))
        )
      ]
    }
  ]

tests :: Spec
tests = do
  describe "Delegator remove" $ mkSpecs testCases1
  describe "Delegator reduce stake" $ mkSpecs testCases2
