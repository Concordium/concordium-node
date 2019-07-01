{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module SchedulerTests.BakerTransactions where

import Test.Hspec

import qualified Data.Map as Map
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod

import qualified Concordium.Crypto.BlockSignature as BlockSig

import qualified Acorn.Core as Core

import Lens.Micro.Platform

import SchedulerTests.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = 
  emptyBlockState emptyBirkParameters &
    (blockAccounts .~ Acc.putAccount (mkAccount alesVK 100000)
                      (Acc.putAccount (mkAccount thomasVK 100000) Acc.emptyAccounts)) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))

baker0 :: Types.BakerInfo
baker0 = mkBaker 0 alesAccount

baker1 :: Types.BakerInfo
baker1 = mkBaker 1 thomasAccount

baker2 :: Types.BakerInfo
baker2 = mkBaker 2 thomasAccount

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = AddBaker (baker0 ^. bakerElectionVerifyKey)
                                (baker0 ^. bakerSignatureVerifyKey)
                                (baker0 ^. bakerAccount)
                                "<dummy proof>"
           , metadata = makeHeader alesKP 1 10000
           , keypair = alesKP
           },
     TJSON { payload = AddBaker (baker1 ^. bakerElectionVerifyKey)
                                (baker1 ^. bakerSignatureVerifyKey)
                                (baker1 ^. bakerAccount) "<dummy proof>"
           , metadata = makeHeader alesKP 2 10000
           , keypair = alesKP
           },     
     TJSON { payload = AddBaker (baker2 ^. bakerElectionVerifyKey)
                                (baker2 ^. bakerSignatureVerifyKey)
                                (baker2 ^. bakerAccount) "<dummy proof>"
           , metadata = makeHeader alesKP 3 10000
           , keypair = alesKP
           },     
     TJSON { payload = RemoveBaker 1 "<dummy proof>"
           , metadata = makeHeader alesKP 4 10000
           , keypair = alesKP
           },
     TJSON { payload = UpdateBakerAccount 2 alesAccount "<dummy proof>"
           , metadata = makeHeader alesKP 5 10000
           , keypair = alesKP
           },
     TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 3)) "<dummy proof>"
           , metadata = makeHeader alesKP 6 10000
           , keypair = alesKP
           }      
    ]

runWithIntermediateStates :: PR.Context Core.UA IO [([(Types.Transaction, Types.ValidResult)],
                                                     [(Types.Transaction, Types.FailureKind)],
                                                     Types.BirkParameters)]
runWithIntermediateStates = do
  txs <- processTransactions transactionsInput
  let (res, _) = foldl (\(acc, st) tx ->
                            let ((suc, failtx), st') =
                                  Types.runSI (Sch.filterTransactions [tx])
                                              Types.dummyChainMeta
                                              st
                            in (acc ++ [(suc, failtx, st' ^. blockBirkParameters)], st'))
                         ([], initialBlockState)
                         txs
  return res

tests :: Spec
tests = do
  results <- runIO (PR.evalContext Init.initialContextData runWithIntermediateStates)
  describe "Baker transactions." $ do
    specify "Correct number of transactions" $
        length results == length transactionsInput
    specify "Adding three bakers from initial empty state" $
        case take 3 results of 
          [([(_,Types.TxSuccess [Types.BakerAdded 0])],[],bps1),
           ([(_,Types.TxSuccess [Types.BakerAdded 1])],[],bps2),
           ([(_,Types.TxSuccess [Types.BakerAdded 2])],[],bps3)] ->
            Map.keys (bps1 ^. Types.birkBakers . bakerMap) == [0] &&
            Map.keys (bps2 ^. Types.birkBakers . bakerMap) == [0,1] &&
            Map.keys (bps3 ^. Types.birkBakers . bakerMap) == [0,1,2]
          _ -> False

    specify "Remove second baker." $
      case results !! 3 of
        ([(_,Types.TxSuccess [Types.BakerRemoved 1])], [], bps4) ->
            Map.keys (bps4 ^. Types.birkBakers . bakerMap) == [0,2]
        _ -> False

    specify "Update third baker's account." $
      -- first check that before the account was thomasAccount, and now it is alesAccount
      case (results !! 3, results !! 4) of
        ((_, _, bps4), ([(_,Types.TxSuccess [Types.BakerAccountUpdated 2 _])], [], bps5)) ->
          Map.keys (bps5 ^. Types.birkBakers . bakerMap) == [0,2] &&
          let b2 = (bps5 ^. Types.birkBakers . bakerMap) Map.! 2
          in b2 ^. bakerAccount == alesAccount &&
             ((bps4 ^. Types.birkBakers . bakerMap) Map.! 2) ^. bakerAccount == thomasAccount
        _ -> False


    specify "Update first baker's sign key." $
      case (results !! 4, results !! 5) of
        ((_, _, bps5), ([(_,Types.TxSuccess [Types.BakerKeyUpdated 0 _])], [], bps6)) ->
          Map.keys (bps6 ^. Types.birkBakers . bakerMap) == [0,2] &&
          let b0 = (bps6 ^. Types.birkBakers . bakerMap) Map.! 0
          in b0 ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 3) &&
             ((bps5 ^. Types.birkBakers . bakerMap) Map.! 0) ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 0)
        _ -> False
        
