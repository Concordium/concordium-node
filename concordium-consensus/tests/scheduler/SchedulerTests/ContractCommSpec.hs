{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.ContractCommSpec where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew
import Concordium.GlobalState.Basic.Invariants

import qualified Data.Text.IO as TIO

import Lens.Micro.Platform

import Control.Monad.IO.Class

import qualified Acorn.Core as Core
import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState emptyBirkParameters dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK 1000000) Acc.emptyAccounts) .
    (blockBank . Rew.totalGTU .~ 1000000) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "CommCounter"
         , metadata = makeHeader alesKP 1 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Recorder"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeHeader alesKP 2 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Counter"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "let pair :: Int64 -> <address> -> Prod.Pair Int64 <address> = Prod.Pair [Int64, <address>] in pair 0 <0, 0>"
                                  }
         , metadata = makeHeader alesKP 3 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 101
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Inc 100"
                            }
         , metadata = makeHeader alesKP 4 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = makeHeader alesKP 5 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = makeHeader alesKP 6 120000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 1"
                            }
         , metadata = makeHeader alesKP 7 120000
         , keypair = alesKP
         }
  ]

testCommCounter ::
  PR.Context Core.UA
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)])
testCommCounter = do
    source <- liftIO $ TIO.readFile "test/contracts/CommCounter.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactions <- processTransactions transactionsInput
    let ((suc, fails), endState) = Types.runSI (Sch.filterTransactions transactions)
                                    Types.dummyChainMeta
                                    initialBlockState
    case invariantBlockState endState of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show endState
        _ -> return ()
    return (suc, fails)

checkCommCounterResult :: ([(a, Types.ValidResult)], [b]) -> Bool
checkCommCounterResult (suc, fails) =
  null fails && -- should be no failed transactions
  length reject == 1 &&  -- one rejected (which is also the last one)
  length nonreject == 6  -- and 6 successful ones
  where 
    nonreject = filter (\case (_, Types.TxSuccess _ _) -> True
                              (_, Types.TxReject _ _) -> False)
                        suc
    reject = filter (\case (_, Types.TxSuccess _ _) -> False
                           (_, Types.TxReject _ _) -> True
                    )
                        suc

tests :: SpecWith ()
tests = 
  describe "Communicating counter." $ do
    specify "6 successful and 1 failed transaction" $ do
      PR.evalContext Init.initialContextData testCommCounter `shouldReturnP` checkCommCounterResult
