{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test that reducing delegation and removing delegators always works, regardless
--  of whether the new stake would violate any of the cap bounds.
--
--  This currently only tests with the basic state implementation which is not
--  ideal. The test should be expanded to also use the persistent state implementation.
module SchedulerTests.Delegation (tests) where

import Lens.Micro.Platform

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.ID.Types as ID
import Concordium.Types.Accounts

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Persistent.Account as BS
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types

import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import qualified SchedulerTests.Helpers as Helpers
import Test.HUnit
import Test.Hspec

-- | Deterministically generate a baker account from a seed.
makeTestBakerV1FromSeed ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    -- | The initial balance of the account.
    Amount ->
    -- | The initial staked amount of the account.
    -- Must be less than or equal to the initial balance.
    Amount ->
    -- | The baker id of the account.
    -- Must match the account index, which is the index of the account in the initial block state.
    BakerId ->
    -- | Seed used to generate account and baker keys.
    Int ->
    m (BS.PersistentAccount av)
makeTestBakerV1FromSeed amount stake bakerId seed = do
    account <- Helpers.makeTestAccountFromSeed amount seed
    let (fulBaker, _, _, _) = mkFullBaker seed bakerId
    let bakerInfoEx =
            BakerInfoExV1
                { _bieBakerInfo = fulBaker ^. theBakerInfo,
                  _bieBakerPoolInfo = poolInfo
                }
    BS.addAccountBakerV1 bakerInfoEx stake True account
  where
    poolInfo =
        BakerPoolInfo
            { _poolOpenStatus = OpenForAll,
              _poolMetadataUrl = UrlText "Some URL",
              _poolCommissionRates =
                CommissionRates
                    { _finalizationCommission = makeAmountFraction 50_000,
                      _bakingCommission = makeAmountFraction 50_000,
                      _transactionCommission = makeAmountFraction 50_000
                    }
            }

-- | Deterministically generate a delegator account from a seed.
makeTestDelegatorFromSeed ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    -- | The initial balance of the account.
    Amount ->
    -- | The delegating details added to the account.
    AccountDelegation av ->
    -- | Seed used to generate the account.
    Int ->
    m (BS.PersistentAccount av)
makeTestDelegatorFromSeed amount accountDelegation seed = do
    account <- Helpers.makeTestAccountFromSeed amount seed
    BS.addAccountDelegator accountDelegation account

-- Accounts

-- |Account of the baker 0.
baker0Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker0Account = makeTestBakerV1FromSeed 1_000_000 1_000_000 bakerId seed
  where
    bakerId = 0
    seed = 16

-- |Account of the delegator1.
delegator1Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
delegator1Account = makeTestDelegatorFromSeed 20_000_000 accountDelegation 17
  where
    accountDelegation =
        AccountDelegationV1
            { _delegationIdentity = 1,
              _delegationStakedAmount = 19_000_000, -- leverage cap is set to 5 in createBlockState, so this puts it over the cap.
              _delegationStakeEarnings = False,
              _delegationTarget = DelegateToBaker 0,
              _delegationPendingChange = NoChange
            }

-- |Account address of the delegator1.
delegator1Address :: AccountAddress
delegator1Address = Helpers.accountAddressFromSeed 17

-- |Account keys of the delegator1 account.
delegator1KP :: SigScheme.KeyPair
delegator1KP = Helpers.keyPairFromSeed 17

-- |Account of the baker 2.
baker2Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker2Account = makeTestBakerV1FromSeed balance stake bakerId seed
  where
    balance = 1_000_000
    stake = 1_000
    bakerId = 2
    seed = 18

-- |Account of the delegator3.
delegator3Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
delegator3Account = makeTestDelegatorFromSeed 20_000_000 accountDelegation 19
  where
    accountDelegation =
        AccountDelegationV1
            { _delegationIdentity = 3,
              _delegationStakedAmount = 1_000, -- leverage cap is set to 5 in createBlockState, so this puts it over the cap.
              _delegationStakeEarnings = False,
              _delegationTarget = DelegateToBaker 2,
              _delegationPendingChange = NoChange
            }

-- |Account address of the delegator3.
delegator3Address :: AccountAddress
delegator3Address = Helpers.accountAddressFromSeed 19

-- |Account keys of the delegator3 account.
delegator3KP :: SigScheme.KeyPair
delegator3KP = Helpers.keyPairFromSeed 19

-- |Account of the baker 4.
baker4Account ::
    (IsAccountVersion av, Blob.MonadBlobStore m, AVSupportsDelegation av) =>
    m (BS.PersistentAccount av)
baker4Account = makeTestBakerV1FromSeed 1_000_000 1_000 bakerId seed
  where
    bakerId = 4
    seed = 20

-- |Create initial block state with account
-- account index 0 is baker0
-- account index 1 is delegator 1 (delegates to baker 0 with overdelegation)
-- account index 2 is baker 2
-- account index 3 is delegator3 (delegates to baker 2)
-- account index 4 is baker 4
initialBlockState ::
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ baker0Account,
          delegator1Account,
          baker2Account,
          delegator3Account,
          baker4Account
        ]

-- | Test removing a delegator even if the stake is over the threshold.
testCase1 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase1 _ pvString =
    specify (pvString ++ ": Remove delegation") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 0,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 1_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        -- Run the test
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        -- Assertions
        let Sch.FilteredTransactions{..} = srTransactions
        events <- case Helpers.getResults ftAdded of
            [(_, Types.TxSuccess{vrEvents})] -> return vrEvents
            _ -> assertFailure "Transaction should succeed"
        assertEqual
            "Delegator was removed"
            [DelegationRemoved 1 delegator1Address]
            events
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState = do
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- | Test reducing delegator stake in such a way that it stays above the cap threshold.
testCase2 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase2 _ pvString =
    specify (pvString ++ ": Reduce delegation stake with overstaking") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 18_999_999,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 1_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents [DelegationStakeDecreased 1 delegator1Address 18_999_999] result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Test transaction rejects if increasing stake above the threshold of the pool
testCase3 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase3 _ pvString =
    specify (pvString ++ ": Increase stake with overstaking") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 19_000_001,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Nothing
                            },
                      metadata = makeDummyHeader delegator1Address 1 1_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeOverMaximumThresholdForPool result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Test reducing delegator stake **and changing target** such that the new stake is above the cap
-- for the new target.
testCase4 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase4 _ pvString =
    specify (pvString ++ ": Reduce stake and change target 1") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 18_000_000,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Just (DelegateToBaker 2)
                            },
                      metadata = makeDummyHeader delegator1Address 1 1_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeOverMaximumThresholdForPool result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Test changing the target and decreasing stake such that the new stake is acceptable for the new target.
-- This still fails because the change of target is only effected after the cooldown period.
testCase5 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase5 _ pvString =
    specify (pvString ++ ": Reduce stake and change target 2") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 1,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Just (DelegateToBaker 2)
                            },
                      metadata = makeDummyHeader delegator1Address 1 1_000,
                      keys = [(0, [(0, delegator1KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeOverMaximumThresholdForPool result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Increase stake successfully.
testCase6 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase6 _ pvString =
    specify (pvString ++ ": Increase stake successfully.") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 1_001,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Nothing
                            },
                      metadata = makeDummyHeader delegator3Address 1 1_000,
                      keys = [(0, [(0, delegator3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents [DelegationStakeIncreased 3 delegator3Address 1_001] result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Increase stake and change target successfully.
testCase7 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase7 _ pvString =
    specify (pvString ++ ": Increase stake and change target successfully.") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 1_001,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Just (DelegateToBaker 4)
                            },
                      metadata = makeDummyHeader delegator3Address 1 1_000,
                      keys = [(0, [(0, delegator3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertSuccessWithEvents
            [ DelegationSetDelegationTarget 3 delegator3Address (DelegateToBaker 4),
              DelegationStakeIncreased 3 delegator3Address 1_001
            ]
            result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Increase stake and change target rejects with reason: maximum threshold for pool.
testCase8 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase8 _ pvString =
    specify (pvString ++ ": Increase stake and change target so that results is overdelegation.") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Just 1_000_001,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Just (DelegateToBaker 4)
                            },
                      metadata = makeDummyHeader delegator3Address 1 1_000,
                      keys = [(0, [(0, delegator3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeOverMaximumThresholdForPool result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

-- |Increase stake and change target rejects with reason: maximum threshold for pool.
testCase9 ::
    forall pv.
    (IsProtocolVersion pv, PVSupportsDelegation pv) =>
    SProtocolVersion pv ->
    String ->
    Spec
testCase9 _ pvString =
    specify (pvString ++ ": Change target to overdelegated pool.") $ do
        let transactions =
                [ Runner.TJSON
                    { payload =
                        Runner.ConfigureDelegation
                            { cdCapital = Nothing,
                              cdRestakeEarnings = Nothing,
                              cdDelegationTarget = Just (DelegateToBaker 0)
                            },
                      metadata = makeDummyHeader delegator3Address 1 1_000,
                      keys = [(0, [(0, delegator3KP)])]
                    }
                ]
        (result, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                Helpers.defaultTestConfig
                (initialBlockState @pv)
                (Helpers.checkReloadCheck checkState)
                transactions
        Helpers.assertRejectWithReason StakeOverMaximumThresholdForPool result
        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockState =
        Helpers.assertBlockStateInvariantsH blockState (Helpers.srExecutionCosts result)

tests :: Spec
tests =
    describe "Delegate in different scenarios" $
        sequence_ $
            Helpers.forEveryProtocolVersion testCases
  where
    testCases :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> String -> Spec
    testCases spv pvString =
        case delegationSupport @(AccountVersionFor pv) of
            SAVDelegationNotSupported -> return ()
            SAVDelegationSupported -> do
                testCase1 spv pvString
                testCase2 spv pvString
                testCase3 spv pvString
                testCase4 spv pvString
                testCase5 spv pvString
                testCase6 spv pvString
                testCase7 spv pvString
                testCase8 spv pvString
                testCase9 spv pvString
