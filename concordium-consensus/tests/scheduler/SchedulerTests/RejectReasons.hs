{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.RejectReasons where

import Test.Hspec
import Test.HUnit

import Data.Maybe (catMaybes)

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner
import Concordium.Wasm (WasmVersion(..))

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

wasmPath :: String
wasmPath = "./testdata/contracts/reject-reasons.wasm"

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
      metadata = makeDummyHeader alesAccount 1 100000,
      payload = DeployModule V0 wasmPath,
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 2 100000,
      payload = InitContract 0 V0 wasmPath "init_success" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 3 100000,
      payload = InitContract 0 V0 wasmPath "init_error_pos" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 4 100000,
      payload = InitContract 0 V0 wasmPath "init_fail_minus2" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 5 100000,
      payload = InitContract 0 V0 wasmPath "init_fail_big" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 6 100000,
      payload = Update 0 (Types.ContractAddress 0 0) "success.receive_error_no_action" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 7 100000,
      payload = Update 0 (Types.ContractAddress 0 0) "success.receive_success" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 8 100000,
      payload = Update 0 (Types.ContractAddress 0 0) "success.receive_error_pos" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 9 100000,
      payload = Update 0 (Types.ContractAddress 0 0) "success.receive_fail_minus5" "",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 10 100000,
      payload = Update 0 (Types.ContractAddress 0 0) "success.receive_fail_big" "",
      keys = [(0,[(0, alesKP)])]
      }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testRejectReasons :: IO TestResult
testRejectReasons = do
    transactions <- processUngroupedTransactions transactionInputs
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
            chainMeta
            maxBound
            maxBound
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkTransactionResults :: TestResult -> Assertion
checkTransactionResults (suc, fails, instances) = do
  assertEqual "There should be 10 successful transactions." 10 (length suc)
  assertEqual "There should be no failed transactions." 0 (length fails)
  assertEqual "There should be 3 runtime failures (from using positive return codes)." 3 (length runtimeFailures)
  assertEqual "There should be 2 rejected init and 2 rejected update transactions." [-2, -2147483648, -5, -2147483648] (catMaybes rejects)
  assertEqual "There should be 1 instance." 1 (length instances)
  where
    rejects = map (\case (_, Types.TxReject{ vrRejectReason = Types.RejectedInit {Types.rejectReason = reason} }) -> Just reason
                         (_, Types.TxReject{ vrRejectReason = Types.RejectedReceive {Types.rejectReason = reason} }) -> Just reason
                         _ -> Nothing) suc
    runtimeFailures = filter (\case (_, Types.TxReject{ vrRejectReason = Types.RuntimeFailure }) -> True
                                    _ -> False) suc

tests :: SpecWith ()
tests =
  describe "Testing error codes in rejected smart contracts." $
    specify "Processing reject-reasons.wasm smart contract" $
      testRejectReasons >>= checkTransactionResults
