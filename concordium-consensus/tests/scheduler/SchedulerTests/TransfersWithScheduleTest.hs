{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.TransfersWithScheduleTest (tests) where

import Control.Monad
import qualified Data.ByteString.Short as BSS
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.ID.Types as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler.Runner as Runner
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Types as Types
import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

tests :: Spec
tests =
    describe "TransfersWithSchedule" $ do
        scheduledTransferWithMemoRejectsP1
        sequence_ $
            Helpers.forEveryProtocolVersion $ \spv pvString -> do
                scheduledTransferTest spv pvString
                when (supportsMemo spv) $
                    scheduledTransferWithMemoTest spv pvString
                when (supportsAccountAliases spv) $
                    scheduledTransferRejectsSelfTransferUsingAliases spv pvString

initialBlockState ::
    (IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 10_000_000_000 0,
          Helpers.makeTestAccountFromSeed 10_000_000_000 1
        ]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: Types.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

scheduledTransferWithMemoRejectsP1 :: SpecWith (Arg Assertion)
scheduledTransferWithMemoRejectsP1 =
    specify "P1: Scheduled transfer with memo rejects in protocol version 1" $ do
        let transactions =
                [ Runner.TJSON
                    { payload = Runner.TransferWithScheduleAndMemo accountAddress1 (Memo $ BSS.pack [0, 1, 2, 3]) [(101, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    }
                ]
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTestTransactionJson
                @'Types.P1
                Helpers.defaultTestConfig
                initialBlockState
                (Helpers.checkReloadCheck checkState)
                transactions
        let Sch.FilteredTransactions{..} = srTransactions
        assertEqual "There should be no failed transactions." [] ftFailed

        case Helpers.getResults ftAdded of
            [(_, Types.TxReject Types.SerializationFailure)] -> return ()
            err -> assertFailure $ "Incorrect transaction result: " ++ show err

        doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState PV1 ->
        Helpers.PersistentBSM PV1 Assertion
    checkState result state =
        Helpers.assertBlockStateInvariantsH state (Helpers.srExecutionCosts result)

scheduledTransferTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
scheduledTransferTest _ pvString =
    specify (pvString ++ ": Transfers with schedule") $
        do
            let contextState =
                    Helpers.defaultContextState
                        { Types._chainMetadata = dummyChainMeta{Types.slotTime = 100}
                        }
            let testConfig =
                    Helpers.defaultTestConfig
                        { Helpers.tcContextState = contextState
                        }

            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                testConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions =
        [ -- make a scheduled transfer
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithSchedule
                            accountAddress1
                            [(101, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    Helpers.assertSuccessWithEvents
                        [ TransferredWithSchedule
                            accountAddress0
                            accountAddress1
                            [(101, 10), (102, 11), (103, 12)]
                        ]
                        result
            },
          -- should get rejected since timestamps are not increasing
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithSchedule
                            accountAddress1
                            [(101, 10), (103, 11), (102, 12)],
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason NonIncreasingSchedule result
            },
          -- should get rejected since first timestamp has expired
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithSchedule
                            accountAddress1
                            [(99, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason FirstScheduledReleaseExpired result
            },
          -- should get rejected since sender = receiver
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithSchedule
                            accountAddress0
                            [(101, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason (ScheduledSelfTransfer accountAddress0) result
            },
          -- should get rejected since one of the amounts is 0
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithSchedule
                            accountAddress1
                            [(101, 10), (102, 0), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 5 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason ZeroScheduledAmount result
            }
        ]

scheduledTransferWithMemoTest ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
scheduledTransferWithMemoTest _ pvString =
    specify (pvString ++ ": Transfers with schedule and memo") $
        do
            let contextState =
                    Helpers.defaultContextState
                        { Types._chainMetadata = dummyChainMeta{Types.slotTime = 100}
                        }
            let testConfig =
                    Helpers.defaultTestConfig
                        { Helpers.tcContextState = contextState
                        }

            Helpers.runSchedulerTestAssertIntermediateStates
                @pv
                testConfig
                initialBlockState
                transactionsAndAssertions
  where
    transactionsAndAssertions =
        [ -- make a scheduled transfer
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithScheduleAndMemo
                            accountAddress1
                            (Memo $ BSS.pack [0, 1, 2, 3])
                            [(101, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 1 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $
                    Helpers.assertSuccessWithEvents
                        [ TransferredWithSchedule
                            accountAddress0
                            accountAddress1
                            [(101, 10), (102, 11), (103, 12)],
                          TransferMemo (Memo $ BSS.pack [0, 1, 2, 3])
                        ]
                        result
            },
          -- should get rejected since timestamps are not increasing
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithScheduleAndMemo
                            accountAddress1
                            (Memo $ BSS.pack [0, 1, 2, 3])
                            [(101, 10), (103, 11), (102, 12)],
                      metadata = makeDummyHeader accountAddress0 2 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason NonIncreasingSchedule result
            },
          -- should get rejected since first timestamp has expired
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithScheduleAndMemo
                            accountAddress1
                            (Memo $ BSS.pack [0, 1, 2, 3])
                            [(99, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 3 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason FirstScheduledReleaseExpired result
            },
          -- should get rejected since sender = receiver
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithScheduleAndMemo
                            accountAddress0
                            (Memo $ BSS.pack [0, 1, 2, 3])
                            [(101, 10), (102, 11), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 4 100_000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason (ScheduledSelfTransfer accountAddress0) result
            },
          -- should get rejected since one of the amounts is 0
          Helpers.TransactionAndAssertion
            { taaTransaction =
                Runner.TJSON
                    { payload =
                        Runner.TransferWithScheduleAndMemo
                            accountAddress1
                            (Memo $ BSS.pack [0, 1, 2, 3])
                            [(101, 10), (102, 0), (103, 12)],
                      metadata = makeDummyHeader accountAddress0 5 100000,
                      keys = [(0, [(0, keyPair0)])]
                    },
              taaAssertion = \result _ ->
                return $ Helpers.assertRejectWithReason ZeroScheduledAmount result
            }
        ]

scheduledTransferRejectsSelfTransferUsingAliases ::
    forall pv.
    Types.IsProtocolVersion pv =>
    Types.SProtocolVersion pv ->
    String ->
    SpecWith (Arg Assertion)
scheduledTransferRejectsSelfTransferUsingAliases _ pvString =
    specify (pvString ++ ": Transfers with schedule to self rejects for account aliases for self") $
        do
            let transactions =
                    [ Runner.TJSON
                        { payload =
                            Runner.TransferWithSchedule
                                (createAlias accountAddress0 2)
                                [(101, 10), (102, 11), (103, 12)],
                          metadata = makeDummyHeader (createAlias accountAddress0 1) 1 100_000,
                          keys = [(0, [(0, keyPair0)])]
                        }
                    ]
            (result, doBlockStateAssertions) <-
                Helpers.runSchedulerTestTransactionJson
                    Helpers.defaultTestConfig
                    initialBlockState
                    (Helpers.checkReloadCheck checkState)
                    transactions
            assertEqual "There should be no failed transactions." [] $
                Sch.ftFailed $
                    Helpers.srTransactions result
            -- the rejection reason is meant to have the sender address.
            Helpers.assertRejectWithReason (ScheduledSelfTransfer (createAlias accountAddress0 1)) result
            doBlockStateAssertions
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result state =
        Helpers.assertBlockStateInvariantsH state (Helpers.srExecutionCosts result)
