{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.SimpleTransfersTest where

import Test.Hspec
import Test.HUnit

import Lens.Micro.Platform
import Control.Monad.IO.Class

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState PV
initialBlockState = blockStateWithAlesAccount
    10000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000) Acc.emptyAccounts)

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = alesAccount, amount = 10000 }
         , metadata = makeDummyHeader alesAccount 1 1000
         , keys = [(0, alesKP)]
         }
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 8800 }
         , metadata = makeDummyHeader alesAccount 2 1000
         , keys = [(0, alesKP)]
         }
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 9870000 }
         , metadata = makeDummyHeader alesAccount 3 1000
         , keys = [(0, alesKP)]
         }
  ,TJSON { payload = Transfer {toaddress = alesAccount, amount = 10000 }
         , metadata = makeDummyHeader thomasAccount 1 500
         , keys = [(0, thomasKP)]
         }
    -- the next transaction should fail because the balance on alesAccount is now 1282, which is
    -- less than 600 + 700
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 60000 }
         , metadata = makeDummyHeader alesAccount 4 700
         , keys = [(0, alesKP)]
         }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   Types.Amount,
                   Types.Amount)


testSimpleTransfer :: IO TestResult
testSimpleTransfer = do
    transactions <- processUngroupedTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummyChainMeta
            maxBound
            maxBound
            initialBlockState
    let gstate = finState ^. Types.ssBlockState
    case invariantBlockState gstate (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (getResults ftAdded,
            ftFailed,
            gstate ^. blockAccounts . singular (ix alesAccount) . accountAmount,
            gstate ^. blockAccounts . singular (ix thomasAccount) . accountAmount)

checkSimpleTransferResult :: TestResult -> Assertion
checkSimpleTransferResult (suc, fails, alesamount, thomasamount) = do
  assertEqual "There should be no failed transactions." [] fails
  assertBool "Last transaction is rejected." reject
  assertBool "Initial transactions are accepted." nonreject
  assertEqual "Amount on first account." alesamount (10000000 - 100 * 4 * fromIntegral simpleTransferCost - 8800 - 9870000 + 10000)
  assertEqual "Amount on the second account." thomasamount (10000000 - 100 * fromIntegral simpleTransferCost + 8800 + 9870000 - 10000)
  where
    nonreject = all (\case (_, Types.TxSuccess{}) -> True
                           (_, Types.TxReject{}) -> False)
                    (init suc)
    reject = case last suc of
               (_, Types.TxReject (Types.AmountTooLarge _ _)) -> True
               _ -> False

tests :: SpecWith ()
tests =
  describe "Simple transfers test:" $
    specify "3 successful and 1 failed transaction" $
      testSimpleTransfer >>= checkSimpleTransferResult
