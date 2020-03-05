{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ContractSimpleTransfersSpec where

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

import Lens.Micro.Platform

import Control.Monad.IO.Class

import qualified Acorn.Core as Core
import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount 1000000 Acc.emptyAccounts 1000000

transactionsInput :: [TransactionJSON]
transactionsInput =
  [TJSON { payload = DeployModule "SimpleTransfers"
         , metadata = makeDummyHeader alesAccount 1 100000
         , keypair = alesKP
         }
  -- create three contracts with addresses 0, 1, 2
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeDummyHeader alesAccount 2 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeDummyHeader alesAccount 3 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeDummyHeader alesAccount 4 100000
         , keypair = alesKP
         }
  -- and then invoke the first to send a message to the last two,
  -- but make it two messages to contract 1, and one message to contract 2
  ,TJSON { payload = Update {amount = 66
                            ,address = Types.ContractAddress 0 0
                            ,moduleName = "SimpleTransfers"
                            ,message = "let one :: ListBase.List Blockchain.Caller = singletonC <1,0> in \
                                        \let two :: ListBase.List Blockchain.Caller = consC <2,0> one in \
                                        \consC <1,0> two"
                            }
         , metadata = makeDummyHeader alesAccount 5 10000
         , keypair = alesKP
         }
  ]

testSimpleTransfers ::
  PR.Context Core.UA
    IO
    ([(Types.BareTransaction, Types.ValidResult)],
     [(Types.BareTransaction, Types.FailureKind)],
     BlockState)
testSimpleTransfers = do
    source <- liftIO $ TIO.readFile "test/contracts/SimpleContractTransfers.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state
    transactions <- processUngroupedTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, finState) =
            Types.runSI (Sch.filterTransactions dummyBlockSize transactions)
              dummySpecialBetaAccounts
              Types.dummyChainMeta
              maxBound
              initialBlockState
    let endState = finState ^. Types.ssBlockState
    case invariantBlockState endState of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show endState
        _ -> return ()
    return (getResults ftAdded, ftFailed, endState)

checkSimpleTransfersResult :: ([(a, Types.ValidResult)], [b], BlockState) -> Bool
checkSimpleTransfersResult (suc, fails, gs) =
  null fails && -- should be no failed transactions
  null reject &&
  length nonreject == 5 &&
  stateCheck
  where
    nonreject = filter (\case (_, Types.TxSuccess{}) -> True
                              (_, Types.TxReject{}) -> False)
                        suc
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc

    stateInstances = gs ^. blockInstances
    stateCheck = let i00 = stateInstances ^. singular (ix $ Types.ContractAddress 0 0)
                     i10 = stateInstances ^. singular (ix $ Types.ContractAddress 1 0)
                     i20 = stateInstances ^. singular (ix $ Types.ContractAddress 2 0)
                 in Types.instanceAmount i00 == 100 &&
                    Types.instanceAmount i10 == 144 &&
                    Types.instanceAmount i20 == 122

tests :: Spec
tests =
  specify "Send simple transfers from contract." $
    PR.evalContext Init.initialContextData testSimpleTransfers `shouldReturnP` checkSimpleTransfersResult
