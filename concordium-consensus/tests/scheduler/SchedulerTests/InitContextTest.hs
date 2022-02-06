{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-| Test that the init context of a contract is passed correctly by the scheduler. -}
module SchedulerTests.InitContextTest where

import Test.Hspec
import Test.HUnit
import Lens.Micro.Platform
import Control.Monad.IO.Class
import Data.Serialize
import Data.Proxy
import Concordium.Types.ProtocolVersion

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner

import Concordium.GlobalState.Instance
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.Wasm

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers
import SchedulerTests.TestUtils

initialBlockState :: IsProtocolVersion pv => BlockState pv
initialBlockState = blockStateWithAlesAccount 1000000000 emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

senderAccount :: forall pv . IsProtocolVersion pv => Proxy pv -> Types.AccountAddress
senderAccount Proxy
  | demoteProtocolVersion (protocolVersion @pv) >= P3 = createAlias alesAccount 17
  | otherwise = alesAccount

transactionInputs :: forall pv . IsProtocolVersion pv => Proxy pv -> [TransactionJSON]
transactionInputs proxy = [
  TJSON{
      metadata = makeDummyHeader (senderAccount proxy) 1 100000,
      payload = DeployModule V0 "./testdata/contracts/chain-meta-test.wasm",
      keys = [(0,[(0, alesKP)])]
      },
  TJSON{
      metadata = makeDummyHeader (senderAccount proxy) 2 100000,
      payload = InitContract 9 V0 "./testdata/contracts/chain-meta-test.wasm" "init_origin" "",
      keys = [(0,[(0, alesKP)])]
      }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testInit :: forall pv . IsProtocolVersion pv => Proxy pv -> IO TestResult
testInit proxy = do
    transactions <- processUngroupedTransactions (transactionInputs proxy)
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
            chainMeta
            maxBound
            maxBound
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState @pv gs (finState ^. Types.schedulerExecutionCosts)of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkInitResult :: forall pv . IsProtocolVersion pv => Proxy pv -> TestResult -> Assertion
checkInitResult proxy (suc, fails, instances) = do
  assertEqual "There should be no failed transactions." [] fails
  assertEqual "There should be no rejected transactions." [] reject
  assertEqual "There should be 1 instance." 1 (length instances)
  model <- case snd . head $ instances of
        InstanceV0 InstanceV{_instanceVModel = InstanceStateV0 s} -> return (contractState s)
        _ -> assertFailure "Expected instance version 0"
  assertEqual "Instance model is the sender address of the account which inialized it." model (encode (senderAccount proxy))
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
             suc

tests :: SpecWith ()
tests =
  describe "Init context in transactions." $ do
    specify "Passing init context to contract P1"
      (testInit (Proxy @'P1) >>= checkInitResult (Proxy @'P1))
    specify "Passing init context to contract P2" $
      testInit (Proxy @'P2) >>= checkInitResult (Proxy @'P2)
    specify "Passing init context to contract P3" $
      testInit (Proxy @'P3) >>= checkInitResult (Proxy @'P3)
    specify "Passing init context to contract P4" $
      testInit (Proxy @'P4) >>= checkInitResult (Proxy @'P4)
