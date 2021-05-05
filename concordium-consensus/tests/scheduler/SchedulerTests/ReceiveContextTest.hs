{-# LANGUAGE OverloadedStrings #-}
{-| Test that the init context of a contract is passed correctly by the scheduler. -}
module SchedulerTests.ReceiveContextTest where

import Test.Hspec
import Test.HUnit
import Lens.Micro.Platform
import Control.Monad.IO.Class
import Data.FixedByteString (pack)

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.ID.Types (AccountAddress(..), accountAddressSize)

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

alesAccount, thomasAccount :: AccountAddress
alesAccount = AccountAddress $ pack $ take accountAddressSize $ repeat 1
thomasAccount = AccountAddress $ pack $ take accountAddressSize $ repeat 2

initialBlockState :: BlockState PV
initialBlockState = createBlockState $ putAccountWithRegIds (mkAccount alesVK alesAccount 1000000000)
                                     $ putAccountWithRegIds (mkAccount thomasVK thomasAccount 1000000000) emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

wasmPath :: String
wasmPath = "./testdata/contracts/send/target/concordium/wasm32-unknown-unknown/release/send.wasm"

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
        metadata = makeDummyHeader alesAccount 1 100000,
        payload = DeployModule 0 wasmPath,
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
        metadata = makeDummyHeader alesAccount 2 100000,
        payload = InitContract 0 0 wasmPath "init_c10" "",
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
          metadata = makeDummyHeader alesAccount 3 100000,
          payload = InitContract 42 0 wasmPath "init_c10" "",
          keys = [(0, [(0, alesKP)])]
          },
  TJSON{
        metadata = makeDummyHeader alesAccount 4 100000,
        payload = InitContract 0 0 wasmPath "init_c20" "",
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
        metadata = makeDummyHeader thomasAccount 1 100000,
        payload = Update 5 (Types.ContractAddress 2 0) "c20.call_c10" "",
        keys = [(0, [(0, thomasKP)])]
        }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testReceive :: IO TestResult
testReceive = do
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

checkReceiveResult :: TestResult -> Assertion
checkReceiveResult (suc, fails, instances) = do
  assertEqual "There should be no failed transactions." [] fails
  assertEqual "There should be no rejected transactions." [] reject
  assertEqual "There should be 3 instances." 3 (length instances)
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
             suc

tests :: SpecWith ()
tests =
  describe "Receive context in transactions." $
    specify "Passing receive context to contract." $
      testReceive >>= checkReceiveResult
