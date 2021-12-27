{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ChainMetatest where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances as Ins
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc

import Lens.Micro.Platform

import Control.Monad.IO.Class

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers
import SchedulerTests.TestUtils

initialBlockState :: BlockState PV1
initialBlockState = blockStateWithAlesAccount 1000000000 Acc.emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
      metadata = makeDummyHeader alesAccount 1 100000,
      payload = DeployModule 0 "./testdata/contracts/chain-meta-test.wasm",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 2 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_check_slot_time" "",
      keys = [(0,[(0, alesKP)])]
      }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testChainMeta :: IO TestResult
testChainMeta = do
    transactions <- processUngroupedTransactions transactionInputs
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
            chainMeta
            maxBound
            maxBound
            slotDuration
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkChainMetaResult :: TestResult -> Assertion
checkChainMetaResult (suc, fails, instances) = do
  assertEqual "There should be no failed transactions." [] fails
  assertEqual "There should be no rejected transactions." [] reject
  assertEqual "There should be 1 instance." 1 (length instances)
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
             suc

tests :: SpecWith ()
tests =
  describe "Chain metadata in transactions." $
    specify "Reading chain metadata." $
      testChainMeta >>= checkChainMetaResult
