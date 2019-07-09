{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.FibonacciTest where

import Test.Hspec
import Test.HUnit

import Data.List as List
import Data.Int

import qualified Acorn.Core as Core

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Instances as Ins
import Concordium.GlobalState.Modules as Mod
import qualified Concordium.GlobalState.Rewards as Rew
import Concordium.GlobalState.Basic.Invariants

import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class

import Lens.Micro.Platform

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState emptyBirkParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK 1000000000) Acc.emptyAccounts) .
    (blockBank . Rew.totalGTU .~ 1000000000) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "FibContract"
         , metadata = makeHeader alesKP 1 10000
         , keypair = alesKP
         }

  ,TJSON { payload = InitContract { amount = 100
                                  , moduleName = "FibContract"
                                  , parameter = "Unit.Unit"
                                  , contractName = "Fibonacci"
                                  }
        , metadata = makeHeader alesKP 2 10000
        , keypair = alesKP
        }
  ,TJSON { payload = Update { amount = 0
                            , moduleName = "FibContract"
                            , message = "Fib 30"
                            , address = Types.ContractAddress { contractIndex = 0, contractSubindex = 0}
                            }
        , metadata = makeHeader alesKP 3 1000000
        , keypair = alesKP
        }
  ]


testFibonacci ::
  PR.Context Core.UA
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)],
     [(Types.ContractAddress, Types.Instance)])
testFibonacci = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gs) = Types.runSI (Sch.filterTransactions transactions)
                                         Types.dummyChainMeta
                                         initialBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure f
        Right _ -> return ()
    return (suc, fails, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

fib :: [Int64]
fib = 1:1:zipWith (+) fib (tail fib)

checkFibonacciResult ::
  ([(a, Types.ValidResult)], [b], [(Types.ContractAddress, Types.Instance)]) -> Bool
checkFibonacciResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the actual list of fibonacci numbers
  where
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc
    checkLocalState inst = 
      let results = List.sort . map snd $ (extractMap (Types.instanceModel inst))
      in results == take 31 fib
    extractMap (Types.VConstructor _ [Types.VLiteral (Core.Int64 k),
                                      Types.VLiteral (Core.Int64 v),
                                      l,
                                      r]) = (k, v) : extractMap l ++ extractMap r
    extractMap _ = []

tests :: Spec
tests =
  describe "Fibonacci with self reference." $ do
    specify "Check first 31 fibonacci are correct." $ do
      PR.evalContext Init.initialContextData testFibonacci `shouldReturnP` checkFibonacciResult
