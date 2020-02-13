{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ContractCommSpec where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

import qualified Acorn.Core as Core
import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 1000000 Acc.emptyAccounts 1000000

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "CommCounter"
         , metadata = makeDummyHeader alesAccount 1 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Recorder"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeDummyHeader alesAccount 2 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Counter"
                                  ,moduleName = "CommCounter"
                                  ,parameter = "let pair :: Int64 -> <address> -> Prod.Pair Int64 <address> = Prod.Pair [Int64, <address>] in pair 0 <0, 0>"
                                  }
         , metadata = makeDummyHeader alesAccount 3 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 101
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Inc 100"
                            }
         , metadata = makeDummyHeader alesAccount 4 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = makeDummyHeader alesAccount 5 100000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 50"
                            }
         , metadata = makeDummyHeader alesAccount 6 120000
         , keypair = alesKP
         }
  ,TJSON { payload = Update {amount = 100
                            ,address = Types.ContractAddress {contractIndex = 1, contractSubindex = 0}
                            ,moduleName = "CommCounter"
                            ,message = "Dec 1"
                            }
         , metadata = makeDummyHeader alesAccount 7 120000
         , keypair = alesKP
         }
  ]

testCommCounter ::
  PR.Context Core.UA
    IO
    ([(Types.BareTransaction, Types.ValidResult)],
     [(Types.BareTransaction, Types.FailureKind)])
testCommCounter = do
    source <- liftIO $ TIO.readFile "test/contracts/CommCounter.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactions <- processUngroupedTransactions transactionsInput
    let ((Sch.FilteredTransactions{..}, _), endState) =
            Types.runSI (Sch.filterTransactions dummyBlockSize (Types.Energy maxBound) transactions)
              dummySpecialBetaAccounts
              Types.dummyChainMeta
              initialBlockState
    case invariantBlockState endState of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show endState
        _ -> return ()
    return (ftAdded, ftFailed)

checkCommCounterResult :: ([(a, Types.ValidResult)], [b]) -> Bool
checkCommCounterResult (suc, fails) =
  null fails && -- should be no failed transactions
  length reject == 1 &&  -- one rejected (which is also the last one)
  length nonreject == 6  -- and 6 successful ones
  where
    nonreject = filter (\case (_, Types.TxSuccess{}) -> True
                              (_, Types.TxReject{}) -> False)
                        suc
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc

tests :: SpecWith ()
tests =
  describe "Communicating counter." $
    specify "6 successful and 1 failed transaction" $
      PR.evalContext Init.initialContextData testCommCounter `shouldReturnP` checkCommCounterResult
