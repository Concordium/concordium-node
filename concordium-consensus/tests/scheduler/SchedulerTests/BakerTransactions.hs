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

baker0 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker0 = mkFullBaker 0 alesAccount

baker1 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker1 = mkFullBaker 1 alesAccount

baker2 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker2 = mkFullBaker 2 thomasAccount

baker3 :: (BakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker3 = mkFullBaker 3 alesAccount

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = AddBaker (baker0 ^. _1 . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerSignatureVerifyKey)
                                (baker0 ^. _1 . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
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
                                (baker1 ^. _4)
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
                                (baker2 ^. _4)
                                (baker2 ^. _3)
                                thomasAccount
                                thomasKP
           , metadata = makeDummyHeader alesAccount 3 10000
           , keypair = alesKP
           },
     TJSON { payload = AddBaker (baker3 ^. _1 . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker3 ^. _1 . bakerSignatureVerifyKey)
                                (baker3 ^. _1 . bakerAggregationVerifyKey)
                                (baker0 ^. _4) -- WRONG KEY, intentional! We want this to fail
                                (baker3 ^. _3)
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 4 10000
           , keypair = alesKP
           },
     TJSON { payload = RemoveBaker 1
           , metadata = makeDummyHeader alesAccount 5 10000
           , keypair = alesKP
           },
     TJSON { payload = UpdateBakerAccount 2 alesAccount alesKP
           , metadata = makeDummyHeader thomasAccount 1 10000
           , keypair = thomasKP
           -- baker 2's account is Thomas account, so only it can update it
           },
     TJSON { payload = UpdateBakerSignKey 0 (BlockSig.verifyKey (bakerSignKey 55)) (BlockSig.signKey (bakerSignKey 55))
           , metadata = makeDummyHeader alesAccount 6 10000
           , keypair = alesKP
           },
     -- Readd baker1 (new bakerId will be 3), which shouldn't result in an error due to duplicated keys, since they
     -- were deleted
     TJSON { payload = AddBaker (baker1 ^. _1 . bakerElectionVerifyKey)
                                (baker1 ^. _2)
                                (baker1 ^. _1 . bakerSignatureVerifyKey)
                                (baker1 ^. _1 . bakerAggregationVerifyKey)
                                (baker1 ^. _4)
                                (baker1 ^. _3)
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 7 10000
           , keypair = alesKP
           },
     -- Update baker1 (id 3) signature key to be the same as baker 2's, SHOULD FAIL
     TJSON { payload = UpdateBakerSignKey 3 (baker2 ^. _1 . bakerSignatureVerifyKey) (baker2 ^. _3)
           , metadata = makeDummyHeader alesAccount 8 10000
           , keypair = alesKP
           },
     -- Add a baker with a duplicate signature key, SHOULD FAIL
     TJSON { payload = AddBaker (baker3 ^. _1 . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker2 ^. _1 . bakerSignatureVerifyKey) -- signature key of baker2
                                (baker3 ^. _1 . bakerAggregationVerifyKey)
                                (baker3 ^. _4)
                                (baker2 ^. _3) -- signature key of baker2
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 9 10000
           , keypair = alesKP
           },
     TJSON { payload = UpdateBakerAggregationVerifyKey 0 (Bls.derivePublicKey $ bakerAggregationKey 42) (bakerAggregationKey 42)
           , metadata = makeDummyHeader alesAccount 10 10000
           , keypair = alesKP
           },
     -- Add a baker with a duplicate aggregation key, SHOULD FAIL
     TJSON { payload = AddBaker (baker3 ^. _1 . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker3 ^. _1 . bakerSignatureVerifyKey)
                                (baker2 ^. _1 . bakerAggregationVerifyKey) -- aggregation key of baker 2
                                (baker2 ^. _4) -- aggregation key of baker 2
                                (baker3 ^. _3)
                                alesAccount
                                alesKP
           , metadata = makeDummyHeader alesAccount 11 10000
           , keypair = alesKP
           },
     -- Update baker1 (id 3) aggregation key to be the same as the one baker0 just changed to
     TJSON { payload = UpdateBakerAggregationVerifyKey 3 (Bls.derivePublicKey $ bakerAggregationKey 42) (bakerAggregationKey 42)
           , metadata = makeDummyHeader alesAccount 12 10000
           , keypair = alesKP
           }
    ]

type TestResult = ([([(Types.BlockItem, Types.ValidResult)],
                     [(Types.Transaction, Types.FailureKind)],
                     BasicBirkParameters)],
                    BlockState)

runWithIntermediateStates :: PR.Context Core.UA IO TestResult
runWithIntermediateStates = do
  txs <- processUngroupedTransactions transactionsInput
  let (res, state) = foldl (\(acc, st) tx ->
                            let (Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions dummyBlockSize (Types.fromTransactions [tx]))
                                    Set.empty -- special beta accounts
                                    Types.dummyChainMeta
                                    maxBound
                                    st
                            in (acc ++ [(getResults ftAdded, ftFailed, st' ^. Types.ssBlockState . blockBirkParameters)], st' ^. Types.schedulerBlockState))
                         ([], initialBlockState)
                         (Types.perAccountTransactions txs)
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
            Map.keys (bps1 ^. birkCurrentBakers . bakerMap) == [0] &&
            Map.keys (bps2 ^. birkCurrentBakers . bakerMap) == [0,1] &&
            Map.keys (bps3 ^. birkCurrentBakers . bakerMap) == [0,1,2]
          _ -> False

    specify "Attempt to add baker with incorrect proof of knowledge of aggregation secret key" $
      case results !! 3 of
        ([(_, Types.TxReject Types.InvalidProof)], [], bps) ->
          Map.keys (bps ^. birkCurrentBakers . bakerMap) == [0,1,2]
        _ -> False

    specify "Remove second baker." $
      case results !! 4 of
        ([(_,Types.TxSuccess [Types.BakerRemoved 1])], [], bps4) ->
            Map.keys (bps4 ^. birkCurrentBakers . bakerMap) == [0,2]
        _ -> False

    specify "Update third baker's account." $
      -- first check that before the account was thomasAccount, and now it is alesAccount
      case (results !! 4, results !! 5) of
        ((_, _, bps4), ([(_,Types.TxSuccess [Types.BakerAccountUpdated 2 _])], [], bps5)) ->
          Map.keys (bps5 ^. birkCurrentBakers . bakerMap) == [0,2] &&
          let b2 = (bps5 ^. birkCurrentBakers . bakerMap) Map.! 2
          in b2 ^. bakerAccount == alesAccount &&
             ((bps4 ^. birkCurrentBakers . bakerMap) Map.! 2) ^. bakerAccount == thomasAccount
        _ -> False

    specify "Update first baker's sign key." $
      case (results !! 5, results !! 6) of
        ((_, _, bps5), ([(_,Types.TxSuccess [Types.BakerKeyUpdated 0 _])], [], bps6)) ->
          Map.keys (bps6 ^. birkCurrentBakers . bakerMap) == [0,2] &&
          let b0 = (bps6 ^. birkCurrentBakers . bakerMap) Map.! 0
          in b0 ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 55) &&
             ((bps5 ^. birkCurrentBakers . bakerMap) Map.! 0) ^. bakerSignatureVerifyKey == BlockSig.verifyKey (bakerSignKey 0)
        _ -> False

    specify "Readding removed baker shouldn't fail due to duplicated keys" $
      case results !! 7 of
        ([(_,Types.TxSuccess [Types.BakerAdded 3])], [], bps7) ->
            Map.keys (bps7 ^. birkCurrentBakers . bakerMap) == [0,2,3]
        _ -> False

    specify "Fail to update baker's signature key to a duplicate" $
      case (results !! 7, results !! 8) of
        ((_,_, bps7), ([(_, Types.TxReject (Types.DuplicateSignKey duplicated))], [], bps8)) ->
            let b0_bps7 = (bps7 ^. birkCurrentBakers . bakerMap) Map.! 3
                b0_bps8 = (bps8 ^. birkCurrentBakers . bakerMap) Map.! 3
            in b0_bps7 ^. bakerSignatureVerifyKey == b0_bps8 ^. bakerSignatureVerifyKey
              && duplicated == (baker2 ^. _1 . bakerSignatureVerifyKey)
        _ -> False

    specify "Fail to add baker with duplicated signature key" $
      case (results !! 8, results !! 9) of
        ((_,_, bps7), ([(_, Types.TxReject (Types.DuplicateSignKey duplicated))], [], bps8)) ->
            Map.keys (bps7 ^. birkCurrentBakers . bakerMap) == Map.keys (bps8 ^. birkCurrentBakers . bakerMap)
              && duplicated == (baker2 ^. _1 . bakerSignatureVerifyKey)
        _ -> False

    specify "Update first baker's aggregation key." $
      case (results !! 9, results !! 10) of
        ((_, _, bps9), ([(_,Types.TxSuccess [Types.BakerAggregationKeyUpdated 0 k])], [], bps10)) ->
          ((bps10 ^. birkCurrentBakers . bakerMap) Map.! 0) ^. bakerAggregationVerifyKey == (Bls.derivePublicKey $ bakerAggregationKey 42) &&
          ((bps9 ^. birkCurrentBakers . bakerMap) Map.! 0) ^. bakerAggregationVerifyKey == baker0 ^. _1 ^. bakerAggregationVerifyKey &&
          k == (Bls.derivePublicKey $ bakerAggregationKey 42)
        _ -> False

    specify "Fail to add baker with duplicated aggregation key" $
      case (results !! 10, results !! 11) of
        ((_,_, bps10), ([(_, Types.TxReject (Types.DuplicateAggregationKey duplicated))], [], bps11)) ->
            Map.keys (bps10 ^. birkCurrentBakers . bakerMap) == Map.keys (bps11 ^. birkCurrentBakers . bakerMap)
              && duplicated == (baker2 ^. _1 . bakerAggregationVerifyKey)
        _ -> False

    specify "Fail to update baker's aggregation key to a duplicate" $
      case (results !! 11, results !! 12) of
        ((_,_, bps11), ([(_, Types.TxReject (Types.DuplicateAggregationKey duplicated))], [], bps12)) ->
            let b0_bps11 = (bps11 ^. birkCurrentBakers . bakerMap) Map.! 3
                b0_bps12 = (bps12 ^. birkCurrentBakers . bakerMap) Map.! 3
            in b0_bps11 ^. bakerSignatureVerifyKey == b0_bps12 ^. bakerSignatureVerifyKey
              && duplicated == (Bls.derivePublicKey $ bakerAggregationKey 42)
        _ -> False
