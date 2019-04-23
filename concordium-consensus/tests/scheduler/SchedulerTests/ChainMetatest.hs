{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.ChainMetatest where

import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Acorn.Core as Core

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Instances as Ins
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod

import Lens.Micro.Platform

import qualified Data.Text.IO as TIO

import Control.Monad.IO.Class

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState &
    (blockAccounts .~ Acc.putAccount (Types.Account alesAccount 1 100000 alesACI) Acc.emptyAccounts) . 
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.Modules gs))

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = DeployModule "ChainMetaTest"
           , metadata = makeHeader alesKP 1 1000
           , keypair = alesKP
           }
    ,TJSON { payload = InitContract {amount = 100
                                    ,contractName = "Simple"
                                    ,moduleName = "ChainMetaTest"
                                    ,parameter = "Unit.Unit"
                                    }
           , metadata = makeHeader alesKP 2 1000
           , keypair = alesKP
           }
    ]


testChainMeta ::
  PR.Context
    IO
    ([(Types.Transaction, Types.ValidResult)],
     [(Types.Transaction, Types.FailureKind)],
     [(Types.ContractAddress, Instance)])
testChainMeta = do
    source <- liftIO $ TIO.readFile "test/contracts/ChainMetaTest.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((suc, fails), gs) = Types.runSI (Sch.filterTransactions transactions)
                                         chainMeta
                                         initialBlockState
    return (suc, fails, Ins.toList (gs ^. blockInstances))

checkChainMetaResult :: ([(a1, Types.ValidResult)], [b], [(a3, Instance)]) -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the 
  where 
    reject = filter (\case (_, Types.TxSuccess _) -> False
                           (_, Types.TxReject _) -> True
                    )
                        suc
    checkLocalState (Types.Instance{..}) = do
      case imodel of
        Types.VConstructor _ [Types.VLiteral (Core.Word64 8)  -- NB: These should match those in chainMeta
                             ,Types.VLiteral (Core.Word64 13)
                             ,Types.VLiteral (Core.Word64 10)] -> True
        _ -> False                                                          

tests :: SpecWith ()
tests = 
  describe "Chain metadata in transactions." $ do
    specify "Reading chain metadata." $ do
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
