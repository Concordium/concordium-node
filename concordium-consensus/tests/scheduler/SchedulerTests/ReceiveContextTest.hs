{-# LANGUAGE OverloadedStrings #-}
{-| Test that the init context of a contract is passed correctly by the scheduler. -}
module SchedulerTests.ReceiveContextTest where

import Test.Hspec
import Test.HUnit
import Lens.Micro.Platform
import Control.Monad.IO.Class
import Data.Foldable

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.Wasm

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 1000000000 emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
      metadata = makeDummyHeader alesAccount 1 100000,
      payload = DeployModule 0 "./testdata/contracts/chain-meta-test.wasm",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 2 100000,
      payload = InitContract 9 0 "./testdata/contracts/chain-meta-test.wasm" "init_origin" "",
      keys = [(0, alesKP)]
      },
  TJSON{
      metadata = makeDummyHeader alesAccount 3 100000,
      payload = Update 9 (Types.ContractAddress 0 0) "receive_context" "",
      keys = [(0, alesKP)]
      }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testReceive :: IO TestResult
testReceive = do
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

checkReceiveResult :: TestResult -> Assertion
checkReceiveResult (suc, fails, instances) = do
  assertEqual "There should be no failed transactions." [] fails
  assertEqual "There should be no rejected transactions." [] reject
  assertEqual "There should be 1 instance." 1 (length instances)
  let model = contractState . instanceModel . snd . head $ instances
  let receiveCtx = ReceiveContext{
        invoker = alesAccount,
        selfAddress = Types.ContractAddress 0 0,
        selfBalance = 9, -- balance it was initialized with
        sender = Types.AddressAccount alesAccount,
        owner = alesAccount,
        rcSenderPolicies = map mkSenderPolicy $ toList $ mkAccount alesVK alesAccount 0 ^. accountPersisting . accountCredentials
        }
  let expectedState = Types.encodeChainMeta chainMeta <> encodeReceiveContext receiveCtx
  assertEqual "Instance model is the chain metadata + receive context." model expectedState
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
             suc

tests :: SpecWith ()
tests =
  xdescribe "Receive context in transactions." $
    specify "Passing receive context to contract." $
      testReceive >>= checkReceiveResult
