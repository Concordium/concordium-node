{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.CredentialTest where

import Test.Hspec
import Test.HUnit

import Control.Monad.IO.Class

import Lens.Micro.Platform

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

-- Test that sending to and from an account without valid credentials fails.

-- Create initial state where alesAccount has a valid credential, but thomasAccount does not.
initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000
    (Acc.putAccountWithRegIds (mkAccountExpiredCredential thomasVK thomasAccount 10000000) Acc.emptyAccounts)


transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = Transfer {toaddress = alesAccount, amount = 0 }
         , metadata = makeDummyHeader alesAccount 1 1000
         , keys = [(0,[(0, alesKP)])]
         }
   -- The next one should fail because the recepient account is not valid.
   -- The transaction should be in a block, but rejected.
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 0 }
         , metadata = makeDummyHeader alesAccount 2 1000
         , keys = [(0,[(0, alesKP)])]
         }
   -- The next transaction should not be part of a block since it is being sent by an account
   -- without a credential
  ,TJSON { payload = Transfer {toaddress = thomasAccount, amount = 0 }
         , metadata = makeDummyHeader thomasAccount 1 1000
         , keys = [(0,[(0, thomasKP)])]
         }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [Types.Transaction])

testCredentialCheck :: IO TestResult
testCredentialCheck = do
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
    return (getResults ftAdded, ftFailed, concat (Types.perAccountTransactions transactions))

checkCredentialCheckResult :: TestResult -> Expectation
checkCredentialCheckResult (suc, fails, transactions) =
  failsCheck >> rejectCheck >> nonrejectCheck
  where
    -- last transaction failed with the right message
    failsCheck =
      case fails of
        [(tx, fr)] -> do
          assertEqual "Last transaction fails:" tx (transactions !! 2)
          assertEqual "With failure NoValidCredential:" fr Types.NoValidCredential
        xs -> assertFailure $ "List should be a singleton:" ++ show xs
    rejectCheck =
      case last suc of
        (tx, Types.TxReject (Types.ReceiverAccountNoCredential _)) ->
          assertEqual "The second transaction should be rejected." tx (Types.normalTransaction (transactions !! 1))
        other -> assertFailure $ "Last recorded transaction should fail with no account credential: " ++ show other
    nonrejectCheck =
      case head suc of
        (tx, Types.TxSuccess{}) ->
          assertEqual "The first transaction should be successful." tx (Types.normalTransaction (transactions !! 0))
        other -> assertFailure $ "First recorded transaction should be successful: " ++ show other


tests :: Spec
tests =
  describe "Credential check test:" $
    specify "one successful, one rejected, one failed transaction" $
      testCredentialCheck >>= checkCredentialCheckResult
