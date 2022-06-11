{-| Test that reducing delegation and removing delegators always works, regardless
  of whether the new stake would violate any of the cap bounds.

  This currently only tests with the basic state implementation which is not
  ideal. The test should be expanded to also use the persistent state implementation.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module SchedulerTests.Delegation (tests) where

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

-- |Account of the baker 0
baker0Address :: AccountAddress
baker0Address = accountAddressFrom 16

-- |Account keys of the baker0 account.
baker0KP :: SigScheme.KeyPair
baker0KP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 16)

baker0VK :: SigScheme.VerifyKey
baker0VK = SigScheme.correspondingVerifyKey baker0KP

baker0Account :: Account 'AccountV1
baker0Account = (mkAccount @'AccountV1 baker0VK baker0Address 1_000_000) {
  _accountStaking = AccountStakeBaker AccountBaker {
    _stakedAmount = 1_000_000,
    _stakeEarnings = True,
    _accountBakerInfo = BakerInfoExV1 {
        _bieBakerInfo = mkFullBaker 16 0 ^. _1 . theBakerInfo,
        _bieBakerPoolInfo = BakerPoolInfo {
            _poolOpenStatus = OpenForAll,
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

-- |Account of the baker 2
baker2Address :: AccountAddress
baker2Address = accountAddressFrom 18

-- |Account keys of the baker2 account.
baker2KP :: SigScheme.KeyPair
baker2KP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 18)

baker2VK :: SigScheme.VerifyKey
baker2VK = SigScheme.correspondingVerifyKey baker2KP

baker2Account :: Account 'AccountV1
baker2Account = (mkAccount @'AccountV1 baker2VK baker2Address 1_000_000) {
  _accountStaking = AccountStakeBaker AccountBaker {
    _stakedAmount = 1_000,
    _stakeEarnings = True,
    _accountBakerInfo = BakerInfoExV1 {
        _bieBakerInfo = mkFullBaker 18 2 ^. _1 . theBakerInfo,
        _bieBakerPoolInfo = BakerPoolInfo {
            _poolOpenStatus = OpenForAll,
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


-- |Account of the delegator1
delegator1Address :: AccountAddress
delegator1Address = accountAddressFrom 17

-- |Account keys of the delegator1 account.
delegator1KP :: SigScheme.KeyPair
delegator1KP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 17)

delegator1VK :: SigScheme.VerifyKey
delegator1VK = SigScheme.correspondingVerifyKey delegator1KP

delegator1Account :: Account 'AccountV1
delegator1Account = (mkAccount @'AccountV1 delegator1VK delegator1Address 20_000_000) {
  _accountStaking = AccountStakeDelegate AccountDelegationV1 {
      _delegationIdentity = 1,
      _delegationStakedAmount = 19_000_000, -- leverage cap is set to 5 in createBlockState, so this puts it over the cap.
      _delegationStakeEarnings = False,
      _delegationTarget = DelegateToBaker 0,
      _delegationPendingChange = NoChange
      }
  }

-- |Account of the delegator3
delegator3Address :: AccountAddress
delegator3Address = accountAddressFrom 19

-- |Account keys of the delegator3 account.
delegator3KP :: SigScheme.KeyPair
delegator3KP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 19)

delegator3VK :: SigScheme.VerifyKey
delegator3VK = SigScheme.correspondingVerifyKey delegator3KP

delegator3Account :: Account 'AccountV1
delegator3Account = (mkAccount @'AccountV1 delegator3VK delegator3Address 20_000_000) {
  _accountStaking = AccountStakeDelegate AccountDelegationV1 {
      _delegationIdentity = 3,
      _delegationStakedAmount = 1_000,
      _delegationStakeEarnings = False,
      _delegationTarget = DelegateToBaker 2,
      _delegationPendingChange = NoChange
      }
  }

-- |Account of the baker 2
baker4Address :: AccountAddress
baker4Address = accountAddressFrom 20

-- |Account keys of the baker4 account.
baker4KP :: SigScheme.KeyPair
baker4KP = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 20)

baker4VK :: SigScheme.VerifyKey
baker4VK = SigScheme.correspondingVerifyKey baker4KP

baker4Account :: Account 'AccountV1
baker4Account = (mkAccount @'AccountV1 baker4VK baker4Address 1_000_000) {
  _accountStaking = AccountStakeBaker AccountBaker {
    _stakedAmount = 1_000,
    _stakeEarnings = True,
    _accountBakerInfo = BakerInfoExV1 {
        _bieBakerInfo = mkFullBaker 20 4 ^. _1 . theBakerInfo,
        _bieBakerPoolInfo = BakerPoolInfo {
            _poolOpenStatus = OpenForAll,
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

-- |Create initial block state with account
-- account index 0 is baker0
-- account index 1 is delegator 1 (delegates to baker 0 with overdelegation)
-- account index 2 is baker 2
-- account index 3 is delegator3 (delegates to baker 2)
-- account index 4 is baker 4
initialBlockState :: BlockState PV4
initialBlockState =
  createBlockState
    ( foldr
        Acc.putAccountWithRegIds
        Acc.emptyAccounts
        [ baker4Account,
          delegator3Account,
          baker2Account,
          delegator1Account,
          baker0Account
        ]
    )

-- Test removing a delegator even if the stake is over the threshold.
testCases1 :: [TestCase PV4]
testCases1 =
  [ TestCase
    { tcName = "Remove delegation"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 0,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Nothing
                            },
                          metadata = makeDummyHeader delegator1Address 1 1_000,
                          keys = [(0,[(0, delegator1KP)])]
                        }
        , (Success (assertEqual "Remove delegation" [DelegationRemoved 1 delegator1Address]), const (return ()))
        )
      ]
    }
  ]

-- Test reducing delegator stake in such a way that it stays above the cap threshold.
testCases2 :: [TestCase PV4]
testCases2 =
  [ TestCase
    { tcName = "Reduce delegation stake with overstaking"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 18_999_999,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Nothing
                            },
                          metadata = makeDummyHeader delegator1Address 1 1_000,
                          keys = [(0,[(0, delegator1KP)])]
                        }
        , (Success (assertEqual "Reduce delegation stake" [DelegationStakeDecreased 1 delegator1Address 18_999_999]), const (return ()))
        )
      ]
    }
  ]

-- Test increasing stake.
testCases3 :: [TestCase PV4]
testCases3 =
  [ TestCase
    { tcName = "Increase stake with overstaking"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 19_000_001,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Nothing
                            },
                          metadata = makeDummyHeader delegator1Address 1 1_000,
                          keys = [(0,[(0, delegator1KP)])]
                        }
        , (Reject StakeOverMaximumThresholdForPool, const (return ()))
        )
      ]
    }
  ]

-- Test reducing delegator stake **and changing target** such that the new stake is above the cap for the new target.
testCases4 :: [TestCase PV4]
testCases4 =
  [ TestCase
    { tcName = "Reduce stake and change target 1"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 18_000_000,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Just (DelegateToBaker 2)
                            },
                          metadata = makeDummyHeader delegator1Address 1 1_000,
                          keys = [(0,[(0, delegator1KP)])]
                        }
        , (Reject StakeOverMaximumThresholdForPool, const (return ()))
        )
      ]
    }
  ]


-- Test changing the target and decreasing stake such that the new stake is acceptable for the new target.
-- This still fails because the change of target is only effected after the cooldown period.
testCases5 :: [TestCase PV4]
testCases5 =
  [ TestCase
    { tcName = "Reduce stake and change target 2"
    , tcParameters = (defaultParams @PV4){tpInitialBlockState=initialBlockState, tpChainMeta=ChainMetadata { slotTime = 100 }}
    , tcTransactions = [
        ( Runner.TJSON  { payload = Runner.ConfigureDelegation {
                            cdCapital = Just 1,
                            cdRestakeEarnings = Nothing,
                            cdDelegationTarget = Just (DelegateToBaker 2)
                            },
                          metadata = makeDummyHeader delegator1Address 1 1_000,
                          keys = [(0,[(0, delegator1KP)])]
                        },
          (Reject StakeOverMaximumThresholdForPool, const (return ()))
        )
      ]
    }
  ]

-- Increase stake successfully.
testCases6 :: [TestCase PV4]
testCases6 =
  [ TestCase
      { tcName = "Increase stake successfully.",
        tcParameters = (defaultParams @PV4) {tpInitialBlockState = initialBlockState, tpChainMeta = ChainMetadata {slotTime = 100}},
        tcTransactions =
          [ ( Runner.TJSON
                { payload =
                    Runner.ConfigureDelegation
                      { cdCapital = Just 1_001,
                        cdRestakeEarnings = Nothing,
                        cdDelegationTarget = Nothing
                      },
                  metadata = makeDummyHeader delegator3Address 1 1_000,
                  keys = [(0, [(0, delegator3KP)])]
                },
              (Success (assertEqual "Increase delegation stake" [DelegationStakeIncreased 3 delegator3Address 1_001]), const (return ()))
            )
          ]
      },
    TestCase
      { tcName = "Increase stake and change target successfully.",
        tcParameters = (defaultParams @PV4) {tpInitialBlockState = initialBlockState, tpChainMeta = ChainMetadata {slotTime = 100}},
        tcTransactions =
          [ ( Runner.TJSON
                { payload =
                    Runner.ConfigureDelegation
                      { cdCapital = Just 1_001,
                        cdRestakeEarnings = Nothing,
                        cdDelegationTarget = Just (DelegateToBaker 4)
                      },
                  metadata = makeDummyHeader delegator3Address 1 1_000,
                  keys = [(0, [(0, delegator3KP)])]
                },
              ( Success
                  ( assertEqual
                      "Increase delegation stake"
                      [ DelegationSetDelegationTarget 3 delegator3Address (DelegateToBaker 4),
                        DelegationStakeIncreased 3 delegator3Address 1_001
                      ]
                  ),
                const (return ())
              )
            )
          ]
      },
    TestCase
      { tcName = "Increase stake and change target so that results is overdelegation.",
        tcParameters = (defaultParams @PV4) {tpInitialBlockState = initialBlockState, tpChainMeta = ChainMetadata {slotTime = 100}},
        tcTransactions =
          [ ( Runner.TJSON
                { payload =
                    Runner.ConfigureDelegation
                      { cdCapital = Just 1_000_001,
                        cdRestakeEarnings = Nothing,
                        cdDelegationTarget = Just (DelegateToBaker 4)
                      },
                  metadata = makeDummyHeader delegator3Address 1 1_000,
                  keys = [(0, [(0, delegator3KP)])]
                },
              (Reject StakeOverMaximumThresholdForPool, const (return ()))
            )
          ]
      },
    TestCase
      { tcName = "Change target to overdelegated pool.",
        tcParameters = (defaultParams @PV4) {tpInitialBlockState = initialBlockState, tpChainMeta = ChainMetadata {slotTime = 100}},
        tcTransactions =
          [ ( Runner.TJSON
                { payload =
                    Runner.ConfigureDelegation
                      { cdCapital = Nothing,
                        cdRestakeEarnings = Nothing,
                        cdDelegationTarget = Just (DelegateToBaker 0)
                      },
                  metadata = makeDummyHeader delegator3Address 1 1_000,
                  keys = [(0, [(0, delegator3KP)])]
                },
              (Reject StakeOverMaximumThresholdForPool, const (return ()))
            )
          ]
      }
  ]


tests :: Spec
tests = describe "Delegate in different scenarios" $ do
  mkSpecs testCases1
  mkSpecs testCases2
  mkSpecs testCases3
  mkSpecs testCases4
  mkSpecs testCases5
  mkSpecs testCases6
