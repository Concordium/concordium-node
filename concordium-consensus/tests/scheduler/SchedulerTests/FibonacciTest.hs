{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.FibonacciTest where

import Test.Hspec
import Test.HUnit

import Data.List as List
import Data.Int
import qualified Data.Sequence as Seq

import qualified Acorn.Core as Core

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState.Instances as Ins
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class

import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 1000000000 Acc.emptyAccounts 1000000000

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "FibContract"
         , metadata = makeDummyHeader alesAccount 1 10000
         , keypair = alesKP
         }

  ,TJSON { payload = InitContract { amount = 100
                                  , moduleName = "FibContract"
                                  , parameter = "Unit.Unit"
                                  , contractName = "Fibonacci"
                                  }
        , metadata = makeDummyHeader alesAccount 2 100000
        , keypair = alesKP
        }
  ,TJSON { payload = Update { amount = 0
                            , moduleName = "FibContract"
                            , message = "Fib 30"
                            , address = Types.ContractAddress { contractIndex = 0, contractSubindex = 0}
                            }
        , metadata = makeDummyHeader alesAccount 3 1000000
        , keypair = alesKP
        }
  ]

type TestResult = ([(Types.BlockItem, Types.ValidResult)],
                   [(Types.WithMetadata Types.BareTransaction, Types.FailureKind)],
                   [(Types.ContractAddress, Types.Instance)])

testFibonacci ::
  PR.Context Core.UA IO TestResult
    
testFibonacci = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processUngroupedTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
          Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
            dummySpecialBetaAccounts
            Types.dummyChainMeta
            maxBound
            initialBlockState
    let gs = finState ^. Types.ssBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (getResults ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

fib :: [Int64]
fib = 1:1:zipWith (+) fib (tail fib)

checkFibonacciResult :: TestResult -> Bool
checkFibonacciResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  null reject && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the actual list of fibonacci numbers
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc
    checkLocalState inst =
      let results = List.sort . map snd $ extractMap (Types.instanceModel inst)
      in results == take 31 fib
    extractMap (Types.VConstructor _ (Types.VLiteral (Core.Int64 k) Seq.:<|
                                      Types.VLiteral (Core.Int64 v) Seq.:<|
                                      l Seq.:<| r Seq.:<| Seq.Empty)) = (k, v) : extractMap l ++ extractMap r
    extractMap _ = []

tests :: Spec
tests =
  describe "Fibonacci with self reference." $
    specify "Check first 31 fibonacci are correct." $
      PR.evalContext Init.initialContextData testFibonacci `shouldReturnP` checkFibonacciResult
