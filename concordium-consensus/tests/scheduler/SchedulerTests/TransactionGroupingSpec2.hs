{-# LANGUAGE OverloadedStrings #-}

{-|
Testing of 'Concordium.Scheduler.filterTransactions'.

See also 'SchedulerTests.TransactionGroupingSpec'.
This module uses a different test setup as 'SchedulerTests.TransactionGroupingSpec' which makes
it easier to define many test cases with expected result and variations thereof.
-}
module SchedulerTests.TransactionGroupingSpec2 where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Lens.Micro.Platform

import Data.Foldable

import Acorn.Core
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Scheduler.Types(Energy,Amount,Nonce,FailureKind(..))
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

-- * Definition of test cases

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 200000 Acc.emptyAccounts 200000

baker :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker = mkFullBaker 1 alesAccount

maxBlockEnergy :: Types.Energy
maxBlockEnergy = Types.Energy 20000

data ExpectedResult
  = Added
  | Failed FailureKind
  | Unprocessed
  deriving (Eq,Show)

-- | Make a test transaction (simple transfer) to own account.
tokay :: Amount -> Nonce -> TransactionJSON
tokay amount nonce = tokayE amount nonce simpleTransferCost

-- | Make a test transaction (simple transfer) to own account with sepcifying
-- the energy to be deposited.
tokayE :: Amount -> Nonce -> Energy -> TransactionJSON
tokayE amount nonce energy =
  TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, .. }
        , metadata = makeDummyHeader alesAccount nonce energy
        , keypair = alesKP
        }

-- | Make a test transaction thet will fail with 'tfailkind'.
tfail :: Amount -> Nonce -> TransactionJSON
tfail amount nonce =
  TJSON { payload = Transfer { toaddress = Types.AddressAccount alesAccount, .. }
        , metadata = makeDummyHeader alesAccount nonce 0 -- will result in DepositInsufficient failure
        , keypair = alesKP
        }

-- | 'FailureKind' of 'tfail'.
tfailkind :: FailureKind
tfailkind = DepositInsufficient

-- | Runs of groups of transactions with expected result.
-- Currently does /not/ test
-- * Credential deployments
-- * Varying sender accounts (should not play a role)
-- * Order regarding arrival time
-- * Unprocessed transactions
testCases :: [(String,[[(TransactionJSON, ExpectedResult)]])]
testCases =
  -- The first number for each transaction is a parameter of the payload and is used to uniquely identify the transaction.
  -- The second number is the transaction nonce.
  [ ( "Single group, no transactions",
    [ []
    ])
  , ( "Two groups, no transactions",
    [ []
    , []
    ])
  , ( "Single group, single added transaction",
    [ [ (tokay 1 1, Added)
      ]
    ])
  , ( "Single group, single failed transaction",
    [ [ (tfail 1 1, Failed tfailkind)
      ]
    ])
  , ( "Single group, single failed transaction (non-sequential nonce 1)",
    [ [ (tokay 1 2, Failed (NonSequentialNonce 1))
      ]
    ])
  , ( "Single group, single failed transaction (non-sequential nonce 2)",
    [ [ (tokay 1 500, Failed (NonSequentialNonce 1))
      ]
    ])
  , ( "Added transaction after non-sequential nonce",
    [ [ (tokay 1 1, Added)
      , (tokay 2 1, Failed (NonSequentialNonce 2))
      , (tokay 3 2, Added) -- correct nonce
      ]
    ])
  , ( "Many added transactions in different sized groups",
    [ [ (tokay 1 1, Added)
      ]
    , [ (tokay 2 2, Added)
      , (tokay 3 3, Added)
      ]
    , [] -- emtpy group
    , [ (tokay 4 4, Added)
      , (tokay 5 5, Added)
      , (tokay 6 6, Added)
      ]
    ])
  , ( "Many added transactions in different sized groups, with non-sequential nonce in middle of last group",
    [ [ (tokay 1 1, Added)
      ]
    , [ (tokay 2 2, Added)
      , (tokay 3 3, Added)
      ]
    , [] -- emtpy group
    , [ (tokay 4 4, Added)
      , (tokay 5 6, Failed (NonSequentialNonce 5))
      , (tokay 6 5, Added) -- correct nonce
      ]
    ])
  , ( "Many added transactions in different sized groups, with non-sequential nonce in middle group (beginning)",
    [ [ (tokay 1 1, Added)
      ]
    , [ (tokay 2 3, Failed (NonSequentialNonce 2))
      , (tokay 3 2, Added) -- correct nonce
      , (tokay 4 3, Added)
      ]
    , [] -- emtpy group
    , [ (tokay 5 4, Added)
      , (tokay 6 5, Added)
      , (tokay 7 6, Added)
      ]
    ])
  , ( "Many added transactions in different sized groups, with non-sequential nonce in middle group (middle)",
    [ [ (tokay 1 1, Added)
      ]
    , [ (tokay 2 2, Added)
      , (tokay 3 4, Failed (NonSequentialNonce 3))
      , (tokay 4 3, Added) -- correc tnonce
      ]
    , [] -- emtpy group
    , [ (tokay 5 4, Added)
      , (tokay 6 5, Added)
      , (tokay 7 6, Added)
      ]
    ])
  , ( "Transaction failure, with following successor in same group",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 2, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with following successor (but incorrect nonce) in same group",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 3, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with following same nonce in same group, then non-sequential nonce",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 1, Added)
      , (tokay 3 3, Failed (NonSequentialNonce 2))
    ] ])
  , ( "Transaction failure, with following successor (but incorrect nonce) in same group, then non-sequential nonce",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 3, Failed SuccessorOfInvalidTransaction)
      , (tokay 3 3, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with many otherwise valid successors",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 2, Failed SuccessorOfInvalidTransaction)
      , (tokay 3 3, Failed SuccessorOfInvalidTransaction)
      , (tokay 4 4, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with many following invalid successors",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tfail 2 2, Failed SuccessorOfInvalidTransaction)
      , (tfail 3 3, Failed SuccessorOfInvalidTransaction)
      , (tfail 4 3, Failed SuccessorOfInvalidTransaction)
      , (tfail 5 4, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with many following invalid and otherwise valid successors",
    [ [
        (tfail 1 1, Failed tfailkind)
      , (tokay 2 2, Failed SuccessorOfInvalidTransaction)
      , (tfail 3 3, Failed SuccessorOfInvalidTransaction)
      , (tfail 4 4, Failed SuccessorOfInvalidTransaction)
      , (tokay 5 5, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Transaction failure, with following successor in other group",
    [ [
        (tfail 1 1, Failed tfailkind)
      ]
    , [ (tokay 2 2, Failed (NonSequentialNonce 1))
    ] ])
  , ( "Transaction failure, with following same nonce in other group",
    [ [
        (tfail 1 1, Failed tfailkind)
      ]
    , [ (tokay 2 1, Added)
    ] ])
  -- Tests with exceeding max block energy
  , ( "Single transaction exceeding max block energy",
    [ [
        (tokayE 1 1 (maxBlockEnergy+1), Failed ExceedsMaxBlockEnergy)
    ] ])
  , ( "Single transaction exceeding max block energy, with following successor in same group",
    [ [
        (tokayE 1 1 (maxBlockEnergy+1), Failed ExceedsMaxBlockEnergy)
      , (tokay 2 2, Failed SuccessorOfInvalidTransaction)
    ] ])
  , ( "Single transaction exceeding max block energy, with following same nonce in same group",
    [ [
        (tokayE 1 1 (maxBlockEnergy+1), Failed ExceedsMaxBlockEnergy)
      , (tokay 2 1, Added)
    ] ])
  , ( "Single transaction exceeding max block energy, with following successor in other group",
    [ [
        (tokayE 1 1 (maxBlockEnergy+1), Failed ExceedsMaxBlockEnergy)
      ]
    , [ (tokay 2 2, Failed (NonSequentialNonce 1))
    ] ])
  , ( "Single transaction exceeding max block energy, with following same nonce in other group",
    [ [
        (tokayE 1 1 (maxBlockEnergy+1), Failed ExceedsMaxBlockEnergy)
      ]
    , [ (tokay 2 1, Added)
    ] ])
  ]



type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [Types.Transaction],
                   [Types.Transaction])

testGroups :: [[TransactionJSON]] -> PR.Context UA IO TestResult
testGroups groups = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source
    -- NOTE Checks should also succeed if arrival time is not 0 for all;
    -- It is only required that the order of 'ts' corresponds to the order in 'groups'.
    ts <- processGroupedTransactions groups
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize ts)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            maxBlockEnergy
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate of
        Left f -> liftIO $ assertFailure f
        Right _ -> return (getResults ftAdded, ftFailed, ftUnprocessed, concat (Types.perAccountTransactions ts))

-- NOTE: In case of failure, this currently does not show the actual lists of invalid and unprocessed
-- transactions, but only the transaction which is not in the expected list, with its expectation.
-- If needed, write a custom expectation at the end of this function.
tests :: Spec
tests =
  describe "Transaction grouping test (2)" $
    forM_ testCases $ \(name, tc) ->
            describe name $ do
              (valid, invalid, unproc, input) <- runIO $
                PR.evalContext Init.initialContextData (testGroups $ map (map fst) tc)
              let tsjson = concat tc
              -- NOTE: We do not check as part of this test whether the second component is a 'TxValid'
              let (validTs, _) = unzip valid
              -- Create expected lists of added, invalid and unprocessed transactions
              -- (here in order of the input, even if the actual assertion might not check that).
              let (eValid, _, _) =
                    mkExpected (zipWith (\(_, expectedRes) t -> (t, expectedRes)) tsjson input) [] [] []
                    where mkExpected [] ev ei eu = (reverse ev, reverse ei, reverse eu)
                          mkExpected ((t, expectedRes):rest) ev ei eu =
                            case expectedRes of
                              Added -> mkExpected rest ((Types.NormalTransaction <$> t):ev) ei eu
                              Failed fk -> mkExpected rest ev ((t, fk):ei) eu
                              Unprocessed -> mkExpected rest ev ei (t:eu)

              specify "Number of transactions in result correct" $ do
                assertEqual "Test setup does not change number of transactions" (length input) (length tsjson)
                length valid + length invalid + length unproc `shouldBe` length input

              -- NOTE: For the valid transactions, also the order is guaranteed
              specify "List of valid transactions correct, including order" $ validTs `shouldBe` eValid

              -- NOTE: This only tests whether all transactions appear in the result if
              -- all transactions are unique. Therefore the above number check is good to have.
              describe "All transactions sorted to correct list" $
                mapM_ (\(n, t, expectedRes) ->
                          specify ("Transaction " ++ show n) $
                            shouldSatisfy (t,expectedRes) $ \_ ->
                              case expectedRes of
                                -- NOTE: With a custom expectation could print list of
                                -- invalid/unproc in case of failure.
                                Added -> (Types.NormalTransaction <$> t) `elem` validTs
                                Failed fk -> (t, fk) `elem` invalid
                                Unprocessed -> t `elem` unproc
                          ) $
                  zipWith3 (\n (_, expectedRes) t -> (n, t, expectedRes)) ([1..] :: [Int]) tsjson input
