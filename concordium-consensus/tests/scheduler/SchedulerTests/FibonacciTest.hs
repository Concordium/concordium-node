{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.FibonacciTest where

import Test.Hspec

import qualified Data.HashMap.Strict as Map
import Data.List as List
import Data.Int

import qualified Acorn.Core as Core

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Crypto.Signature as S
import System.Random

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Instances as Ins
import Concordium.GlobalState.Modules as Mod

import qualified Data.Text.IO as TIO
import Data.Maybe
import Control.Monad.IO.Class

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

alesKP :: S.KeyPair
alesKP = fst (S.randomKeyPair (mkStdGen 1))

alesACI :: AH.AccountCreationInformation
alesACI = AH.createAccount (S.verifyKey alesKP)

alesAccount :: Types.AccountAddress
alesAccount = AH.accountAddress alesACI


initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState
    { blockAccounts = Acc.putAccount (Types.Account alesAccount 1 1000000000 alesACI) Acc.emptyAccounts
    , blockModules = (let (_, _, gs) = Init.baseState in Mod.Modules gs) }

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "FibContract"
         , metadata = Types.TransactionHeader alesAccount 1 1000
         , keypair = alesKP
         }

  ,TJSON { payload = InitContract { amount = 100
                                  , moduleName = "FibContract"
                                  , parameter = "Unit.Unit"
                                  , contractName = "Fibonacci"
                                  }
        , metadata = Types.TransactionHeader alesAccount 2 1000
        , keypair = alesKP
        }
  ,TJSON { payload = Update { amount = 0
                            , moduleName = "FibContract"
                            , message = "Fib 30"
                            , address = Types.ContractAddress { contractIndex = 0, contractVersion = 0}
                            }
        , metadata = Types.TransactionHeader alesAccount 3 100000
        , keypair = alesKP
        }
  ]


testFibonacci ::
  PR.Context
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)],
     [(Types.ContractAddress, Types.Instance)])
testFibonacci = do
    source <- liftIO $ TIO.readFile "test/contracts/FibContract.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gs) = Types.runSI (Sch.makeValidBlock transactions)
                                         Types.dummyChainMeta
                                         initialBlockState
    return (suc, fails, Ins.toList (blockInstances gs))

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
    checkLocalState (Types.Instance{..}) = do
      case imodel of
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
