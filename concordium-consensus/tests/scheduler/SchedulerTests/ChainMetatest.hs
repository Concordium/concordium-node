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

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 10000000 Acc.emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 111
        blockHeight = 222
        finalizedHeight = 333
        slotTime = 444

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
      metadata = makeDummyHeader alesAccount 1 100000,
      payload = DeployModule 0 "./testdata/contracts/chain-meta-test.wasm",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 2 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_check_slot" "",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 3 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_check_height" "",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 4 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_check_finalized_height" "",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 5 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_check_slot_time" "",
      keys = [(0, alesKP)]
      }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testChainMeta :: IO TestResult
testChainMeta = do
    transactions <- processUngroupedTransactions transactionInputs
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummySpecialBetaAccounts
            chainMeta
            maxBound
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkChainMetaResult :: TestResult -> Assertion
checkChainMetaResult (suc, fails, instances) = do
  assertEqual "There should be no failed transactions." [] fails
  assertEqual "There should be no rejected transactions." [] reject
  assertEqual "There should be 4 instances." 4 (length instances)
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
