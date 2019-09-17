{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
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
import Concordium.GlobalState.Basic.Invariants
import Concordium.GlobalState.Instances as Ins
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew

import Lens.Micro.Platform

import qualified Data.Text.IO as TIO
import qualified Data.Sequence as Seq

import Control.Monad.IO.Class

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState emptyBirkParameters dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK 100000) Acc.emptyAccounts) . 
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs))) .
    (blockBank . Rew.totalGTU .~ 100000)

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
    ,TJSON { payload = InitContract {amount = 123
                                    ,contractName = "Simple"
                                    ,moduleName = "ChainMetaTest"
                                    ,parameter = "Unit.Unit"
                                    }
           , metadata = makeHeader alesKP 2 10000
           , keypair = alesKP
           }
    ]


testChainMeta ::
  PR.Context Core.UA
    IO
    ([(Types.BareTransaction, Types.ValidResult)],
     [(Types.BareTransaction, Types.FailureKind)],
     [(Types.ContractAddress, Instance)])
testChainMeta = do
    source <- liftIO $ TIO.readFile "test/contracts/ChainMetaTest.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let (Sch.FilteredTransactions{..}, gs) =
          Types.runSI (Sch.filterTransactions blockSize transactions)
          chainMeta
          initialBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkChainMetaResult :: ([(a1, Types.ValidResult)], [b], [(a3, Instance)]) -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  length reject == 0 && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the 
  where 
    reject = filter (\case (_, Types.TxSuccess _ _) -> False
                           (_, Types.TxReject _ _) -> True
                    )
                        suc
    checkLocalState inst = do
      case Types.instanceModel inst of
        Types.VConstructor _ (Types.VLiteral (Core.Word64 8) Seq.:<|  -- NB: These should match those in chainMeta
                              Types.VLiteral (Core.Word64 13) Seq.:<|
                              Types.VLiteral (Core.Word64 10) Seq.:<| Seq.Empty) -> True
        _ -> False                                                          

tests :: SpecWith ()
tests = 
  describe "Chain metadata in transactions." $ do
    specify "Reading chain metadata." $ do
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
