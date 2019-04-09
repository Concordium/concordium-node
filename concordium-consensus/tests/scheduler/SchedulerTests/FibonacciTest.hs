{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.FibonacciTest where

import Test.Hspec

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
import System.Random

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler.Scheduler as Sch
import qualified Acorn.Core as Core

import qualified Data.HashMap.Strict as Map

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.List as List
import Data.Int

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 1))))

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI

initialGlobalState :: Types.GlobalState
initialGlobalState = (Types.GlobalState Map.empty (Map.fromList [(alesAccount, Types.Account alesAccount 1 1000000000 alesACI)])
                      (let (_, _, gs) = Init.baseState in gs))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "FibContract"
         , metadata = Types.Header { sender = alesAccount
                                   , nonce = 1
                                   , gasAmount = 1000}}
  
  ,TJSON { payload = InitContract { amount = 100
                                  , moduleName = "FibContract"
                                  , parameter = "Unit.Unit"
                                  , contractName = "Fibonacci"
                                  }
        , metadata = Types.Header { sender = alesAccount
                                  , nonce = 2
                                  , gasAmount = 1000}}
  ,TJSON { payload = Update { amount = 0
                            , moduleName = "FibContract"
                            , message = "Fib 30 <0,0>"
                            , address = Types.ContractAddress { contractIndex = 0, contractVersion = 0}
                            }
        , metadata = Types.Header { sender = alesAccount
                                  , nonce = 3
                                  , gasAmount = 100000}}
  ]


testFibonacci ::
  PR.Context
    IO
    ([(Types.MessageTy, Types.ValidResult)],
     [(Types.MessageTy, Types.FailureKind)],
     Map.HashMap Types.ContractAddress Types.Instance)
testFibonacci = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gs) = Types.runSI (Sch.makeValidBlock transactions)
                                         Types.dummyChainMeta
                                         initialGlobalState
    return (suc, fails, Types.instances gs)

fib :: [Int64]
fib = 1:1:zipWith (+) fib (tail fib)

checkFibonacciResult ::
  ([(a, Types.ValidResult)], [b], Map.HashMap Types.ContractAddress Types.Instance) -> Bool
checkFibonacciResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  Map.size instances == 1 && -- only a single contract instance should be created
  Map.member (Types.ContractAddress 0 0) instances && -- and it should have the 0, 0 index
  checkLocalState (instances Map.! Types.ContractAddress 0 0) -- and the local state should match the 
  where
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc
    checkLocalState (Types.Instance{..}) = do
      case lState of
        Types.VDict mp ->
          let results = List.sort (mapMaybe (\case (Types.VLiteral (Core.Int64 i)) -> Just i
                                                   _ -> Nothing) (Map.elems mp))
          in results == take 31 fib
        _ -> False

tests :: Spec
tests =
  describe "Fibonacci with self reference." $ do
    specify "Check first 31 fibonacci are correct." $ do
      PR.evalContext Init.initialContextData testFibonacci `shouldReturnP` checkFibonacciResult
