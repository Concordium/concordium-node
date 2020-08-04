{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ChainMetatest where

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
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc

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
initialBlockState = blockStateWithAlesAccount 10000000 Acc.emptyAccounts

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10
        slotTime = dummySlotTime

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = DeployModule "ChainMetaTest"
           , metadata = makeDummyHeader alesAccount 1 1000000
           , keys = [(0, alesKP)]
           }
    ,TJSON { payload = InitContract {amount = 123
                                    ,contractName = "Simple"
                                    ,moduleName = "ChainMetaTest"
                                    ,parameter = "Unit.Unit"
                                    }
           , metadata = makeDummyHeader alesAccount 2 1000000
           , keys = [(0, alesKP)]
           }
    ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.Transaction, Types.FailureKind)],
                   [(Types.ContractAddress, Instance)])

testChainMeta :: PR.Context Core.UA IO TestResult
testChainMeta = do
    let file = "test/contracts/ChainMetaTest.acorn"
    source <- liftIO $ TIO.readFile file
    (_, _) <- PR.processModule file source -- execute only for effect on global state, i.e., load into cache
    transactions <- processUngroupedTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            chainMeta
            maxBound
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkChainMetaResult :: TestResult -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  null reject && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc
    checkLocalState inst =
      case Types.instanceModel inst of
        Types.VConstructor _ (Types.VLiteral (Core.Word64 8) Seq.:<|  -- NB: These should match those in chainMeta
                              Types.VLiteral (Core.Word64 13) Seq.:<|
                              Types.VLiteral (Core.Word64 10) Seq.:<| Seq.Empty) -> True
        _ -> False

tests :: SpecWith ()
tests =
  describe "Chain metadata in transactions." $
    specify "Reading chain metadata." $
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
