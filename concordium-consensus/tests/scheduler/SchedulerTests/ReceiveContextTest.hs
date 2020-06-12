{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ReceiveContextTest where

-- Tests that the receive context in acorn is properly instantiated.

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Acorn.Core as Core

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances as Ins
import Concordium.GlobalState.Basic.BlockState.Account as Acc

import Lens.Micro.Platform

import qualified Data.Text.IO as TIO
import qualified Data.Sequence as Seq

import Control.Monad.IO.Class

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 10000000 (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000) Acc.emptyAccounts)

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10
        slotTime = dummySlotTime

initialAmount :: Types.Amount
initialAmount = 123

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = DeployModule "ReceiveContextTest"
           , metadata = makeDummyHeader alesAccount 1 1000000
           , keypair = alesKP
           }
    ,TJSON { payload = InitContract {amount = initialAmount
                                    ,contractName = "Simple"
                                    ,moduleName = "ReceiveContextTest"
                                    ,parameter = "<13,18>"
                                    }
           , metadata = makeDummyHeader alesAccount 2 1000000
           , keypair = alesKP
           }
    ,TJSON { payload = Update {amount = 108
                              ,address = Types.ContractAddress 0 0
                              ,moduleName = "ReceiveContextTest"
                              ,message = "Unit.Unit"
                              }
           , metadata = makeDummyHeader thomasAccount 1 10000
           , keypair = thomasKP
           }
    ]


type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testReceiveContext :: PR.Context Core.UA IO TestResult
testReceiveContext = do
    let file = "test/contracts/ReceiveContextTest.acorn"
    source <- liftIO $ TIO.readFile file
    (_, _) <- PR.processModule file source -- execute only for effect on global state, i.e., load into cache
    transactions <- processUngroupedTransactions transactionsInput
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

assertTrue :: HasCallStack => String -> Bool -> Assertion
assertTrue msg = assertEqual msg True

checkReceiveContextResult :: TestResult -> Assertion
checkReceiveContextResult (suc, fails, instances) = do
  assertTrue "No failed transactions" (null fails)
  assertTrue "No rejected transactions." (null reject)
  assertEqual "Only a single instance." 1 (length instances)
  checkLocalState (snd (head instances)) -- and the local state should match the receive context
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc
    checkLocalState inst =
      case Types.instanceModel inst of
        Types.VConstructor _ (Types.VLiteral (Core.AAddress originAddr) Seq.:<|
                              Types.VLiteral (Core.CAddress selfAddress) Seq.:<|
                              Types.VLiteral (Core.AmountLiteral amount) Seq.:<| Seq.Empty) -> do
            assertEqual "Origin address is correct." thomasAccount originAddr
            -- because it is the first and only smart contract in the given state
            assertEqual "Self address is 0,0" (Types.ContractAddress 0 0) selfAddress 
            assertEqual "Self balance is the initial amount." initialAmount amount
        v -> assertFailure $ "Instance model not of the correct shape: " ++ show v

tests :: SpecWith ()
tests =
  describe "Receive context in smart contract calls." $
    specify "Reading receive context metadata." $
      PR.evalContext Init.initialContextData testReceiveContext >>= checkReceiveContextResult
