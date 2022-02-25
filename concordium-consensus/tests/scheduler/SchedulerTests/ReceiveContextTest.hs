{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-| Test that the receive context of a contract is passed correctly by the scheduler. -}
module SchedulerTests.ReceiveContextTest where

import Test.Hspec
import Test.HUnit
import Lens.Micro.Platform
import Control.Monad.IO.Class
import Data.FixedByteString (pack)
import Data.Proxy

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner
import Concordium.Types.ProtocolVersion
import Concordium.Wasm (WasmVersion(..))
import Concordium.TransactionVerification

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.ID.Types (AccountAddress(..), accountAddressSize)

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers
import SchedulerTests.TestUtils

alesAccount, thomasAccount :: AccountAddress
alesAccount = AccountAddress $ pack $ take accountAddressSize $ repeat 1
thomasAccount = AccountAddress $ pack $ take accountAddressSize $ repeat 2

sender1 :: forall pv . IsProtocolVersion pv => Proxy pv -> Types.AccountAddress
sender1 Proxy
  | demoteProtocolVersion (protocolVersion @pv) >= P3 = createAlias alesAccount 17
  | otherwise = alesAccount

sender2 :: forall pv . IsProtocolVersion pv => Proxy pv -> Types.AccountAddress
sender2 Proxy
  | demoteProtocolVersion (protocolVersion @pv) >= P3 = createAlias thomasAccount 77
  | otherwise = thomasAccount


-- See the contract in /testdata/contracts/send/src/lib.rs from which the wasm
-- module is derived. The contract calls check that the invoker or sender is the
-- account address consisting of only 2's, and that the owner is an account
-- consisting of only 1's. We set up the state with addresses different from
-- those and use the above-mentioned addresses as alias for the same account.
-- This only applies to protocol P3 and up.
initialBlockState :: forall pv. (IsProtocolVersion pv, ChainParametersVersionFor pv ~ 'ChainParametersV0, AccountVersionFor pv ~ 'AccountV0) => BlockState pv
initialBlockState = createBlockState $ putAccountWithRegIds (mkAccount alesVK (sender1 (Proxy @pv)) 1000000000)
                                     $ putAccountWithRegIds (mkAccount thomasVK (sender2 (Proxy @pv)) 1000000000) emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{ slotTime = 444 }

wasmPath :: String
wasmPath = "./testdata/contracts/send/target/concordium/wasm32-unknown-unknown/release/send.wasm"

transactionInputs :: [TransactionJSON]
transactionInputs = [
  TJSON{
        metadata = makeDummyHeader alesAccount 1 100000,
        payload = DeployModule V0 wasmPath,
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
        metadata = makeDummyHeader alesAccount 2 100000,
        payload = InitContract 0 V0 wasmPath "init_c10" "",
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
          metadata = makeDummyHeader alesAccount 3 100000,
          payload = InitContract 42 V0 wasmPath "init_c10" "",
          keys = [(0, [(0, alesKP)])]
          },
  TJSON{
        metadata = makeDummyHeader alesAccount 4 100000,
        payload = InitContract 0 V0 wasmPath "init_c20" "",
        keys = [(0, [(0, alesKP)])]
        },
  TJSON{
        metadata = makeDummyHeader thomasAccount 1 100000,
        payload = Update 5 (Types.ContractAddress 2 0) "c20.call_c10" "",
        keys = [(0, [(0, thomasKP)])]
        }
  ]

type TestResult = ([(BlockItemWithStatus, Types.ValidResult)],
                   [(TransactionWithStatus, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testReceive :: forall pv. (IsProtocolVersion pv, ChainParametersVersionFor pv ~ 'ChainParametersV0, AccountVersionFor pv ~ 'AccountV0) => Proxy pv -> IO TestResult
testReceive Proxy = do
    transactions <- processUngroupedTransactions transactionInputs
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize dummyBlockTimeout transactions)
            chainMeta
            maxBound
            maxBound
            slotDuration
            (initialBlockState @pv)
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs (finState ^. Types.schedulerExecutionCosts) of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (instanceAddress i, i)))

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
  describe "Receive context in transactions." $ do
    specify "Passing receive context to contract P1." $
      testReceive (Proxy @'P1) >>= checkReceiveResult
    specify "Passing receive context to contract P2." $
      testReceive (Proxy @'P2) >>= checkReceiveResult
    specify "Passing receive context to contract P3." $
      testReceive (Proxy @'P3) >>= checkReceiveResult
