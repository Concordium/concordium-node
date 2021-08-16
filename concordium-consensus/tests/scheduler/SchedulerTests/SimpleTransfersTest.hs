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

initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount
    1000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 1000000) Acc.emptyAccounts)

transactionsInput :: [TransactionJSON]
transactionsInput =
  -- transfer 10000 from A to A
  [TJSON { payload = Transfer {toaddress = alesAccount, amount = 10000 }
         , metadata = makeDummyHeader alesAccount 1 simpleTransferCost
         , keys = [(0, [(0, alesKP)])]
         }
  -- transfer 8800 from A to T
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 8800 }
         , metadata = makeDummyHeader alesAccount 2 simpleTransferCost
         , keys = [(0, [(0, alesKP)])]
         }
  -- transfer everything from from A to T
  -- the (100 *) is conversion between NRG and GTU
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 1000000 - 8800 - 3 * 100 * fromIntegral simpleTransferCost }
         , metadata = makeDummyHeader alesAccount 3 simpleTransferCost
         , keys = [(0, [(0, alesKP)])]
         }
  -- transfer 10000 back from T to A
  ,TJSON { payload = Transfer {toaddress = alesAccount, amount = 100 * fromIntegral simpleTransferCost }
         , metadata = makeDummyHeader thomasAccount 1 simpleTransferCost
         , keys = [(0, [(0, thomasKP)])]
         }
    -- the next transaction should fail because the balance on A is now exactly enough to cover the transfer cost
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 1 }
         , metadata = makeDummyHeader alesAccount 4 simpleTransferCost
         , keys = [(0, [(0, alesKP)])]
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
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
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
  reject
  assertBool "Initial transactions are accepted." nonreject
  assertEqual "Amount on the A account." 0 alesamount 
  assertEqual "Amount on the T account." (2000000 - 5 * 100 * fromIntegral simpleTransferCost) thomasamount
  where
    nonreject = all (\case (_, Types.TxSuccess{}) -> True
                           (_, Types.TxReject{}) -> False)
                    (init suc)
    reject = case last suc of
               (_, Types.TxReject (Types.AmountTooLarge addr amnt)) -> do
                 assertEqual "Sending from A" (Types.AddressAccount alesAccount) addr
                 assertEqual "Exactly 1microGTU" 1 amnt
               err -> assertFailure $ "Incorrect result of the last transaction: " ++ show (snd err)

tests :: SpecWith ()
tests =
  describe "Simple transfers test:" $
    specify "3 successful and 1 failed transaction" $
      testSimpleTransfer >>= checkSimpleTransferResult
