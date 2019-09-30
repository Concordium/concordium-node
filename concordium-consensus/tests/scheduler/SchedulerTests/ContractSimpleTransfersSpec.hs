{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.ContractSimpleTransfersSpec where

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
  [TJSON { payload = DeployModule "SimpleTransfers"
         , metadata = makeHeader alesKP 1 100000
         , keypair = alesKP
         }
  -- create three contracts with addresses 0, 1, 2
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeHeader alesKP 2 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeHeader alesKP 3 100000
         , keypair = alesKP
         }
  ,TJSON { payload = InitContract {amount = 100
                                  ,contractName = "Transfer"
                                  ,moduleName = "SimpleTransfers"
                                  ,parameter = "Unit.Unit"
                                  }
         , metadata = makeHeader alesKP 4 100000
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
         , metadata = makeHeader alesKP 5 10000
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
    transactions <- processTransactions transactionsInput
    let ((Sch.FilteredTransactions{..}, _), endState) =
            Types.runSI (Sch.filterTransactions blockSize transactions)
              dummySpecialBetaAccounts
              Types.dummyChainMeta
              initialBlockState
    case invariantBlockState endState of
        Left f -> liftIO $ assertFailure $ f ++ "\n" ++ show endState
        _ -> return ()
    return (ftAdded, ftFailed, endState)

checkSimpleTransfersResult :: ([(a, Types.ValidResult)], [b], BlockState) -> Bool
checkSimpleTransfersResult (suc, fails, gs) =
  null fails && -- should be no failed transactions
  length reject == 0 &&
  length nonreject == 5 &&
  stateCheck
  where 
    nonreject = filter (\case (_, Types.TxSuccess _ _ _) -> True
                              (_, Types.TxReject _ _ _) -> False)
                        suc
    reject = filter (\case (_, Types.TxSuccess _ _ _) -> False
                           (_, Types.TxReject _ _ _) -> True
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
