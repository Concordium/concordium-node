{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
--Testing of 'Concordium.Scheduler.filterTransactions'.
--
--See also 'SchedulerTests.TransactionGroupingSpec'.
--This module uses a different test setup as 'SchedulerTests.TransactionGroupingSpec' which makes
--it easier to define many test cases with expected result and variations thereof.
module SchedulerTests.TransactionGroupingSpec2 (tests) where

import Data.Foldable
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import Concordium.Scheduler.Types (Amount, Energy, FailureKind (..), Nonce)
import qualified Concordium.Scheduler.Types as Types
import Concordium.TransactionVerification
import qualified SchedulerTests.Helpers as Helpers

-- * Definition of test cases

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 2000000 0]

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

maxBlockEnergy :: Types.Energy
maxBlockEnergy = 20000

data ExpectedResult
    = Added
    | Failed FailureKind
    | Unprocessed
    deriving (Eq, Show)

-- | Make an (in itself valid ("okay")) test transaction (simple transfer with given amount to own
-- account).
tOkay :: Amount -> Nonce -> TransactionJSON
tOkay amount nonce = tOkayE amount nonce Helpers.simpleTransferCost

-- | Make an (in itself valid ("okay")) test transaction (simple transfer with given amount to own
-- account) with sepcifying the energy to be deposited.
tOkayE :: Amount -> Nonce -> Energy -> TransactionJSON
tOkayE amount nonce energy =
    TJSON
        { payload = Transfer{toaddress = accountAddress0, ..},
          metadata = makeDummyHeader accountAddress0 nonce energy,
          keys = [(0, [(0, keyPair0)])]
        }

-- | Make a test transaction (simple transfer with given amount) that will fail with 'tFailKind'.
tFail :: Amount -> Nonce -> TransactionJSON
tFail amount nonce =
    TJSON
        { payload = Transfer{toaddress = accountAddress0, ..},
          metadata = makeDummyHeader accountAddress0 nonce 0, -- will result in DepositInsufficient failure
          keys = [(0, [(0, keyPair0)])]
        }

-- | 'FailureKind' of 'tFail'.
tFailKind :: FailureKind
tFailKind = DepositInsufficient

-- | Runs of groups of transactions with expected result.
-- Currently does /not/ test
-- * Credential deployments
-- * Varying sender accounts (should not play a role)
-- * Order regarding arrival time
-- * Unprocessed transactions
testCases :: [(String, [[(TransactionJSON, ExpectedResult)]])]
testCases =
    -- The first number for each transaction is a parameter of the payload and is used to uniquely identify the transaction.
    -- The second number is the transaction nonce.
    [   ( "Single group, no transactions",
            [ []
            ]
        ),
        ( "Two groups, no transactions",
            [ [],
              []
            ]
        ),
        ( "Single group, single added transaction",
            [   [ (tOkay 1 1, Added)
                ]
            ]
        ),
        ( "Single group, single failed transaction",
            [   [ (tFail 1 1, Failed tFailKind)
                ]
            ]
        ),
        ( "Single group, single failed transaction (non-sequential nonce 1)",
            [   [ (tOkay 1 2, Failed (NonSequentialNonce 1))
                ]
            ]
        ),
        ( "Single group, single failed transaction (non-sequential nonce 2)",
            [   [ (tOkay 1 500, Failed (NonSequentialNonce 1))
                ]
            ]
        ),
        ( "Added transaction after non-sequential nonce",
            [   [ (tOkay 1 1, Added),
                  (tOkay 2 1, Failed (NonSequentialNonce 2)),
                  (tOkay 3 2, Added) -- correct nonce
                ]
            ]
        ),
        ( "Many added transactions in different sized groups",
            [   [ (tOkay 1 1, Added)
                ],
                [ (tOkay 2 2, Added),
                  (tOkay 3 3, Added)
                ],
              [], -- emtpy group
                [ (tOkay 4 4, Added),
                  (tOkay 5 5, Added),
                  (tOkay 6 6, Added)
                ]
            ]
        ),
        ( "Many added transactions in different sized groups, with non-sequential nonce in middle of last group",
            [   [ (tOkay 1 1, Added)
                ],
                [ (tOkay 2 2, Added),
                  (tOkay 3 3, Added)
                ],
              [], -- emtpy group
                [ (tOkay 4 4, Added),
                  (tOkay 5 6, Failed (NonSequentialNonce 5)),
                  (tOkay 6 5, Added) -- correct nonce
                ]
            ]
        ),
        ( "Many added transactions in different sized groups, with non-sequential nonce in middle group (beginning)",
            [   [ (tOkay 1 1, Added)
                ],
                [ (tOkay 2 3, Failed (NonSequentialNonce 2)),
                  (tOkay 3 2, Added), -- correct nonce
                  (tOkay 4 3, Added)
                ],
              [], -- emtpy group
                [ (tOkay 5 4, Added),
                  (tOkay 6 5, Added),
                  (tOkay 7 6, Added)
                ]
            ]
        ),
        ( "Many added transactions in different sized groups, with non-sequential nonce in middle group (middle)",
            [   [ (tOkay 1 1, Added)
                ],
                [ (tOkay 2 2, Added),
                  (tOkay 3 4, Failed (NonSequentialNonce 3)),
                  (tOkay 4 3, Added) -- correc tnonce
                ],
              [], -- emtpy group
                [ (tOkay 5 4, Added),
                  (tOkay 6 5, Added),
                  (tOkay 7 6, Added)
                ]
            ]
        ),
        ( "Transaction failure, with following successor in same group",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 2, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with following successor (but incorrect nonce) in same group",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 3, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with following same nonce in same group, then non-sequential nonce",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 1, Added),
                  (tOkay 3 3, Failed (NonSequentialNonce 2))
                ]
            ]
        ),
        ( "Transaction failure, with following successor (but incorrect nonce) in same group, then non-sequential nonce",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 3, Failed SuccessorOfInvalidTransaction),
                  (tOkay 3 3, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with many otherwise valid successors",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 2, Failed SuccessorOfInvalidTransaction),
                  (tOkay 3 3, Failed SuccessorOfInvalidTransaction),
                  (tOkay 4 4, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with many following invalid successors",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tFail 2 2, Failed SuccessorOfInvalidTransaction),
                  (tFail 3 3, Failed SuccessorOfInvalidTransaction),
                  (tFail 4 3, Failed SuccessorOfInvalidTransaction),
                  (tFail 5 4, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with many following invalid and otherwise valid successors",
            [   [ (tFail 1 1, Failed tFailKind),
                  (tOkay 2 2, Failed SuccessorOfInvalidTransaction),
                  (tFail 3 3, Failed SuccessorOfInvalidTransaction),
                  (tFail 4 4, Failed SuccessorOfInvalidTransaction),
                  (tOkay 5 5, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Transaction failure, with following successor in other group",
            [   [ (tFail 1 1, Failed tFailKind)
                ],
                [ (tOkay 2 2, Failed (NonSequentialNonce 1))
                ]
            ]
        ),
        ( "Transaction failure, with following same nonce in other group",
            [   [ (tFail 1 1, Failed tFailKind)
                ],
                [ (tOkay 2 1, Added)
                ]
            ]
        ),
      -- Tests with exceeding max block energy
        ( "Single transaction exceeding max block energy",
            [   [ (tOkayE 1 1 (maxBlockEnergy + 1), Failed ExceedsMaxBlockEnergy)
                ]
            ]
        ),
        ( "Single transaction exceeding max block energy, with following successor in same group",
            [   [ (tOkayE 1 1 (maxBlockEnergy + 1), Failed ExceedsMaxBlockEnergy),
                  (tOkay 2 2, Failed SuccessorOfInvalidTransaction)
                ]
            ]
        ),
        ( "Single transaction exceeding max block energy, with following same nonce in same group",
            [   [ (tOkayE 1 1 (maxBlockEnergy + 1), Failed ExceedsMaxBlockEnergy),
                  (tOkay 2 1, Added)
                ]
            ]
        ),
        ( "Single transaction exceeding max block energy, with following successor in other group",
            [   [ (tOkayE 1 1 (maxBlockEnergy + 1), Failed ExceedsMaxBlockEnergy)
                ],
                [ (tOkay 2 2, Failed (NonSequentialNonce 1))
                ]
            ]
        ),
        ( "Single transaction exceeding max block energy, with following same nonce in other group",
            [   [ (tOkayE 1 1 (maxBlockEnergy + 1), Failed ExceedsMaxBlockEnergy)
                ],
                [ (tOkay 2 1, Added)
                ]
            ]
        )
    ]

type TestResult =
    ( [(BlockItemWithStatus, Types.ValidResult)],
      [(TransactionWithStatus, Types.FailureKind)],
      [TransactionWithStatus],
      [TransactionWithStatus]
    )

testGroups ::
    forall pv.
    (Types.IsProtocolVersion pv) =>
    Types.SProtocolVersion pv ->
    [[TransactionJSON]] ->
    IO TestResult
testGroups _ groups = do
    let contextState =
            Helpers.defaultContextState
                { Types._maxBlockEnergy = maxBlockEnergy
                }
    let testConfig =
            Helpers.defaultTestConfig
                { Helpers.tcContextState = contextState
                }
    -- NOTE Checks should also succeed if arrival time is not 0 for all;
    -- It is only required that the order of 'ts' corresponds to the order in 'groups'.
    ts <- processGroupedTransactions groups
    (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
        Helpers.runSchedulerTest
            @pv
            testConfig
            initialBlockState
            (Helpers.checkReloadCheck checkState)
            ts
    doBlockStateAssertions
    let Sch.FilteredTransactions{..} = srTransactions
    return (Helpers.getResults ftAdded, ftFailed, ftUnprocessed, concat (Types.perAccountTransactions ts))
  where
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState pv ->
        Helpers.PersistentBSM pv Assertion
    checkState result blockstate =
        Helpers.assertBlockStateInvariantsH blockstate (Helpers.srExecutionCosts result)

-- NOTE: In case of failure, this currently does not show the actual lists of invalid and unprocessed
-- transactions, but only the transaction which is not in the expected list, with its expectation.
-- If needed, write a custom expectation at the end of this function.
tests :: Spec
tests =
    describe "Transaction grouping test (2)" $
        forM_ testCases $ \(name, tc) ->
            sequence_ $
                Helpers.forEveryProtocolVersion $ \spv pvString ->
                    describe (pvString ++ ": " ++ name) $ do
                        (valid, invalid, unproc, input) <- runIO $ testGroups spv $ map (map fst) tc
                        let tsjson = concat tc
                        -- NOTE: We do not check as part of this test whether the second component is a 'TxValid'
                        let (validTs, _) = unzip valid
                        -- Create expected lists of added, invalid and unprocessed transactions
                        -- (here in order of the input, even if the actual assertion might not check that).
                        let (eValid, _, _) =
                                mkExpected (zipWith (\(_, expectedRes) t -> (fst t, expectedRes)) tsjson input) [] [] []
                              where
                                mkExpected [] ev ei eu = (reverse ev, reverse ei, reverse eu)
                                mkExpected ((t, expectedRes) : rest) ev ei eu =
                                    case expectedRes of
                                        Added -> mkExpected rest ((Types.normalTransaction t) : ev) ei eu
                                        Failed fk -> mkExpected rest ev ((t, fk) : ei) eu
                                        Unprocessed -> mkExpected rest ev ei (t : eu)

                        specify "Number of transactions in result correct" $ do
                            assertEqual "Test setup does not change number of transactions" (length input) (length tsjson)
                            length valid + length invalid + length unproc `shouldBe` length input

                        -- NOTE: For the valid transactions, also the order is guaranteed (TODO the specification
                        -- does not guarantee this; check whether other properties should be checked or whether
                        -- just the membership in `ftAdded` is sufficient).
                        specify "List of valid transactions correct, including order" $ map fst validTs `shouldBe` eValid

                        -- NOTE: This only tests whether all transactions appear in the result if
                        -- all transactions are unique. Therefore the above number check is good to have.
                        describe "All transactions sorted to correct list"
                            $ mapM_
                                ( \(n, t, expectedRes) ->
                                    specify ("Transaction " ++ show n) $
                                        shouldSatisfy (t, expectedRes) $ \_ ->
                                            case expectedRes of
                                                -- NOTE: With a custom expectation could print list of
                                                -- invalid/unproc in case of failure.
                                                Added -> (Types.normalTransaction t) `elem` map fst validTs
                                                Failed fk -> (t, fk) `elem` map (\(x, y) -> (fst x, y)) invalid
                                                Unprocessed -> t `elem` map fst unproc
                                )
                            $ zipWith3 (\n (_, expectedRes) t -> (n, fst t, expectedRes)) ([1 ..] :: [Int]) tsjson input
