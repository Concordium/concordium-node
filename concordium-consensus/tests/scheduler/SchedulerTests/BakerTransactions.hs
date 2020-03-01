{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.BakerTransactions where

import Test.Hspec

import qualified Data.Map as Map
import qualified Data.HashSet as Set
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls

import qualified Acorn.Core as Core

import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    100000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 100000) Acc.emptyAccounts)
    200000

baker0 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker0 = mkFullBaker 0 alesAccount

baker1 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker1 = mkFullBaker 1 alesAccount

baker2 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker2 = mkFullBaker 2 thomasAccount

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = AddBaker (baker0 ^. _1 . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerSignatureVerifyKey)
                                (baker0 ^. _1 . bakerAggregationVerifyKey)
                                (baker0 ^. _3)
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 1 10000
           , keypair = alesKP
           },
     TJSON { payload = AddBaker (baker1 ^. _1 . bakerElectionVerifyKey)
                                (baker1 ^. _2)
                                (baker1 ^. _1 . bakerSignatureVerifyKey)
                                (baker1 ^. _1 . bakerAggregationVerifyKey)
                                (baker1 ^. _3)
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 2 10000
           , keypair = alesKP
           },
     TJSON { payload = AddBaker (baker2 ^. _1 . bakerElectionVerifyKey)
                                (baker2 ^. _2)
                                (baker2 ^. _1 . bakerSignatureVerifyKey)
                                (baker2 ^. _1 . bakerAggregationVerifyKey)
                                (baker2 ^. _3)
                                thomasAccount
                                thomasKP
           , metadata = makeDummyHeader alesAccount 3 10000
           , keypair = alesKP
           },
     TJSON { payload = RemoveBaker 1 "<dummy proof>"
           , metadata = makeDummyHeader alesAccount 4 10000
           , keypair = alesKP
           },
     TJSON { payload = UpdateBakerAccount 2 alesAccount alesKP
           , metadata = makeDummyHeader thomasAccount 1 10000
           , keypair = thomasKP
           -- baker 2's account is Thomas account, so only it can update it
           },
     TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 3)) (BlockSig.signKey (bakerSignKey 3))
           , metadata = makeDummyHeader alesAccount 5 10000
           , keypair = alesKP
           -- baker 0's account is Thomas account, so only it can update it
           }
    ]

runWithIntermediateStates :: PR.Context Core.UA IO ([([(Types.BareTransaction, Types.ValidResult)],
                                                     [(Types.BareTransaction, Types.FailureKind)],
                                                     Types.BirkParameters)], BlockState)
runWithIntermediateStates = do
  txs <- processUngroupedTransactions transactionsInput
  let (res, state) = foldl (\(acc, st) tx ->
                            let (Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions dummyBlockSize [tx])
                                    Set.empty -- special beta accounts
                                    Types.dummyChainMeta
                                    maxBound
                                    st
                            in (acc ++ [(getResults ftAdded, ftFailed, st' ^. Types.ssBlockState . blockBirkParameters)], st' ^. Types.schedulerBlockState))
                         ([], initialBlockState)
                         txs
  return (res, state)

tests :: Spec
tests = do
  (results, endState) <- runIO (PR.evalContext Init.initialContextData runWithIntermediateStates)
  describe "Baker transactions." $ do
    specify "Result state satisfies invariant" $
        case invariantBlockState endState of
            Left f -> expectationFailure f
            Right _ -> return ()
    specify "Correct number of transactions" $
        length results == length transactionsInput
    specify "Adding three bakers from initial empty state" $
        case take 3 results of
          [([(_,Types.TxSuccess [Types.BakerAdded 0])],[],bps1),
           ([(_,Types.TxSuccess [Types.BakerAdded 1])],[],bps2),
           ([(_,Types.TxSuccess [Types.BakerAdded 2])],[],bps3)] ->
            Map.keys (bps1 ^. Types.birkCurrentBakers . bakerMap) == [0] &&
            Map.keys (bps2 ^. Types.birkCurrentBakers . bakerMap) == [0,1] &&
            Map.keys (bps3 ^. Types.birkCurrentBakers . bakerMap) == [0,1,2]
          _ -> False

    specify "Remove second baker." $
      case results !! 3 of
        ([(_,Types.TxSuccess [Types.BakerRemoved 1])], [], bps4) ->
            Map.keys (bps4 ^. Types.birkCurrentBakers . bakerMap) == [0,2]
        _ -> False

    specify "Update third baker's account." $
      -- first check that before the account was thomasAccount, and now it is alesAccount
      case (results !! 3, results !! 4) of
        ((_, _, bps4), ([(_,Types.TxSuccess [Types.BakerAccountUpdated 2 _])], [], bps5)) ->
          Map.keys (bps5 ^. Types.birkCurrentBakers . bakerMap) == [0,2] &&
          let b2 = (bps5 ^. Types.birkCurrentBakers . bakerMap) Map.! 2
          in b2 ^. bakerAccount == alesAccount &&
             ((bps4 ^. Types.birkCurrentBakers . bakerMap) Map.! 2) ^. bakerAccount == thomasAccount
        _ -> False


    specify "Update first baker's sign key." $
      case (results !! 4, results !! 5) of
        ((_, _, bps5), ([(_,Types.TxSuccess [Types.BakerKeyUpdated 0 _])], [], bps6)) ->
          Map.keys (bps6 ^. Types.birkCurrentBakers . bakerMap) == [0,2] &&
          let b0 = (bps6 ^. Types.birkCurrentBakers . bakerMap) Map.! 0
          in b0 ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 3) &&
             ((bps5 ^. Types.birkCurrentBakers . bakerMap) Map.! 0) ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 0)
        _ -> False
