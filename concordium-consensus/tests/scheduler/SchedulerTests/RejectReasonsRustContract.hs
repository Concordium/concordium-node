{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.RejectReasonsRustContract where

import Test.Hspec
import Test.HUnit

import Data.Maybe (catMaybes)

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
import Data.Text (Text)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 1000000000 Acc.emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

wasmPath :: String
wasmPath = "./testdata/contracts/error_code/target/concordium/wasm32-unknown-unknown/release/error_code.wasm"

transaction :: PayloadJSON -> Types.Nonce -> TransactionJSON
transaction payload n = TJSON {
  metadata = makeDummyHeader alesAccount n 100000,
  payload = payload,
  keys = [(0, [(0, alesKP)])]
}

initWithAmount :: Types.Amount -> Types.Nonce -> TransactionJSON
initWithAmount amount = transaction (InitContract amount 0 wasmPath "init_error_codes" "")

updateWithAmount :: Types.Amount -> Text -> Types.Nonce -> TransactionJSON
updateWithAmount amount fun = transaction (Update amount (Types.ContractAddress 0 0) fun "")

transactionInputs :: [TransactionJSON]
transactionInputs = zipWith (\t n -> t n) transactionFunctionList [1..]
                    where transactionFunctionList = [ transaction (DeployModule 0 wasmPath),

                                                      -- returns InitError::VeryBadError
                                                      -- error code: -101
                                                      initWithAmount 1,

                                                      -- returns ParseError::default() which gets converted into InitError::ParseErrorWrapper
                                                      -- error code: -102
                                                      initWithAmount 2,

                                                      -- returns InitError::ParseErrorWrapper
                                                      -- error code: -102
                                                      initWithAmount 3,

                                                      -- returns SpecialInitError::VerySpecialError which gets also converted into InitError::ParseErrorWrapper
                                                      -- error code: -102
                                                      initWithAmount 4,

                                                      -- returns MostSpecialInitError::SuperSpecialError which gets converted into InitError::SomeOtherError
                                                      -- error code: -103
                                                      initWithAmount 5,

                                                      -- returns MostSpecialInitError::TheAmazingError which gets also converted into InitError::SomeOtherError
                                                      -- error code: -103
                                                      initWithAmount 6,

                                                      -- successfully initializes contract
                                                      initWithAmount 7,

                                                      -- Now a similar sequence as above for receive functions:

                                                      -- returns ReceiveError::VeryBadError
                                                      -- error code: -101
                                                      updateWithAmount 1 "error_codes.receive",

                                                      -- returns a ParseError which gets wrapped into a ReceiveError::ParseErrorWrapper
                                                      -- error code: -102
                                                      updateWithAmount 2 "error_codes.receive",

                                                      -- directly returns a ReceiveError::ParseErrorWrapper
                                                      -- error code: -102
                                                      updateWithAmount 3 "error_codes.receive",

                                                      -- returns a SpecialReceiveError::VerySpecialError which gets wrapped into a ReceiveError::ParseError
                                                      -- error code: -102
                                                      updateWithAmount 4 "error_codes.receive",

                                                      -- returns a ParseError
                                                      -- error code: -2
                                                      updateWithAmount 0 "error_codes.receive2",

                                                      -- returns Err(()) (Unit)
                                                      -- error code: -1
                                                      updateWithAmount 0 "error_codes.receive3" ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testRejectReasons :: IO TestResult
testRejectReasons = do
    transactions <- processUngroupedTransactions transactionInputs
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
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
  assertEqual "There should be 14 successful transactions." 14 (length suc)
  assertEqual "There should be no failed transactions." 0 (length fails)
  assertEqual "There should be no runtime failures." 0 (length runtimeFailures)
  assertEqual "There should be 6 rejected init and 6 rejected update transactions." [-101, -102, -102, -102, -103, -103, -101, -102, -102, -102, -2, -1] (catMaybes rejects)
  assertEqual "There should be 1 instance." 1 (length instances)
  where
    rejects = map (\case (_, Types.TxReject{ vrRejectReason = Types.Rejected {Types.rejectReason = reason} }) -> Just reason
                         _ -> Nothing) suc
    runtimeFailures = filter (\case (_, Types.TxReject{ vrRejectReason = Types.RuntimeFailure }) -> True
                                    _ -> False) suc

tests :: SpecWith ()
tests =
  describe "Testing error codes in Rust smart contracts." $
    specify "Processing error_codes.wasm smart contract" $
      testRejectReasons >>= checkTransactionResults
