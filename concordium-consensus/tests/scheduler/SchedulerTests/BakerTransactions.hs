{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
module SchedulerTests.BakerTransactions where

import Test.Hspec

import Control.Monad
import System.Random
import Data.Foldable
import Data.Maybe
import qualified Data.Set as Set
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SignatureScheme as SigScheme

import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as L

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

keyPair :: Int -> SigScheme.KeyPair
keyPair = uncurry SigScheme.KeyPairEd25519 . fst . randomEd25519KeyPair . mkStdGen

account :: Int -> Types.AccountAddress
account = accountAddressFrom

initialBlockState :: BlockState PV
initialBlockState = createBlockState $ foldr putAccountWithRegIds Acc.emptyAccounts
  [mkAccount (SigScheme.correspondingVerifyKey (keyPair i)) (account i) 10_000_000 | i <- reverse [0..3]]

baker0 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker0 = mkFullBaker 0 0

baker1 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker1 = mkFullBaker 1 1

baker2 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker2 = mkFullBaker 2 2

baker3 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker3 = mkFullBaker 3 3

transactionsInput :: [TransactionJSON]
transactionsInput =
    [
      -- Add baker on account 0 (OK)
      TJSON { payload = AddBaker (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
                                8_000_000
                                True
           , metadata = makeDummyHeader (account 0) 1 10000
           , keys = [(0,[(0, keyPair 0)])]
           },
      -- Add baker on account 1 (OK)
      TJSON { payload = AddBaker (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker1 ^. _2)
                                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker1 ^. _3)
                                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker1 ^. _4)
                                7_500_000
                                False
           , metadata = makeDummyHeader (account 1) 1 10000
           , keys = [(0,[(0, keyPair 1)])]
           },
      -- Add baker on account 2, duplicate aggregation key of baker 0 (FAIL)
      TJSON { payload = AddBaker (baker2 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker2 ^. _2)
                                (baker2 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker2 ^. _3)
                                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
                                3_000_000
                                False
           , metadata = makeDummyHeader (account 2) 1 10000
           , keys = [(0,[(0, keyPair 2)])]
           },
      -- Add baker on account 2, duplicate sign and election key of baker 0 (OK)
      TJSON { payload = AddBaker (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker2 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker2 ^. _4)
                                4_000_000
                                False
           , metadata = makeDummyHeader (account 2) 2 10000
           , keys = [(0,[(0, keyPair 2)])]
           },
      -- Update baker 0 with original keys (OK)
      TJSON { payload = UpdateBakerKeys (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
           , metadata = makeDummyHeader (account 0) 2 10000
           , keys = [(0,[(0, keyPair 0)])]
           },
      -- Update baker 0 with baker1's aggregation key (FAIL)
      TJSON { payload = UpdateBakerKeys (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker1 ^. _4)
           , metadata = makeDummyHeader (account 0) 3 10000
           , keys = [(0,[(0, keyPair 0)])]
           },
      -- Add baker on account 3, bad election key proof (FAIL)
      TJSON { payload = AddBaker (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker3 ^. _3)
                                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker3 ^. _4)
                                4_000_000
                                False
           , metadata = makeDummyHeader (account 3) 1 10000
           , keys = [(0,[(0, keyPair 3)])]
           },
      -- Add baker on account 3, bad sign key proof (FAIL)
      TJSON { payload = AddBaker (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker3 ^. _4)
                                4_000_000
                                False
           , metadata = makeDummyHeader (account 3) 2 10000
           , keys = [(0,[(0, keyPair 3)])]
           },
      -- Add baker on account 3, bad aggregation key proof (FAIL)
      TJSON { payload = AddBaker (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker3 ^. _3)
                                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
                                4_000_000
                                False
           , metadata = makeDummyHeader (account 3) 3 10000
           , keys = [(0,[(0, keyPair 3)])]
           },
      -- Remove baker 3 (FAIL)
      TJSON { payload = RemoveBaker
            , metadata = makeDummyHeader (account 3) 4 10000
            , keys = [(0,[(0, keyPair 3)])]
            },
      -- Remove baker 0 (OK)
      TJSON { payload = RemoveBaker
            , metadata = makeDummyHeader (account 0) 4 10000
            , keys = [(0,[(0, keyPair 0)])]
            },
      -- Add baker on account 3 with baker 0's keys (FAIL)
      -- This fails because baker 0 remains valid during cooldown.
      TJSON { payload = AddBaker (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker0 ^. _2)
                                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker0 ^. _3)
                                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker0 ^. _4)
                                4_000_000
                                False
           , metadata = makeDummyHeader (account 3) 5 10000
           , keys = [(0,[(0, keyPair 3)])]
           },
      -- Update baker 1 with bad sign key proof (FAIL)
      TJSON { payload = UpdateBakerKeys (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker1 ^. _3)
                                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker1 ^. _4)
           , metadata = makeDummyHeader (account 1) 2 10000
           , keys = [(0,[(0, keyPair 1)])]
           },
      -- Update baker 1 with bad election key proof (FAIL)
      TJSON { payload = UpdateBakerKeys (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker1 ^. _2)
                                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker3 ^. _3)
                                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker1 ^. _4)
           , metadata = makeDummyHeader (account 1) 3 10000
           , keys = [(0,[(0, keyPair 1)])]
           },
      -- Update baker 1 with bad aggregation key proof (FAIL)
      TJSON { payload = UpdateBakerKeys (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                                (baker3 ^. _2)
                                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                                (baker1 ^. _3)
                                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                                (baker3 ^. _4)
           , metadata = makeDummyHeader (account 1) 4 10000
           , keys = [(0,[(0, keyPair 1)])]
           }
    ]

type TestResult = ([([(Types.BlockItem, Types.ValidResult)],
                     [(Types.Transaction, Types.FailureKind)],
                     BasicBirkParameters)],
                    BlockState PV,
                    Types.Amount)

runWithIntermediateStates :: IO TestResult
runWithIntermediateStates = do
  txs <- processUngroupedTransactions transactionsInput
  let (res, state, feeTotal) = foldl' (\(acc, st, fees) tx ->
                            let (Sch.FilteredTransactions{..}, st') =
                                  Types.runSI
                                    (Sch.filterTransactions dummyBlockSize (Types.fromTransactions [tx]))
                                    dummyChainMeta
                                    maxBound
                                    maxBound
                                    st
                            in (acc ++ [(getResults ftAdded, ftFailed, st' ^. Types.ssBlockState . blockBirkParameters)], st' ^. Types.schedulerBlockState, fees + st' ^. Types.schedulerExecutionCosts))
                         ([], initialBlockState, 0)
                         (Types.perAccountTransactions txs)
  return (res, state, feeTotal)

keysL :: L.LFMBTree Types.BakerId (Maybe FullBakerInfo) -> [Types.BakerId]
keysL t = let l = L.toAscPairList t in
   [ i | (i, Just _) <- l ]

getL :: L.LFMBTree Types.BakerId (Maybe FullBakerInfo) -> Types.BakerId -> FullBakerInfo
getL t bid = fromJust $ join $ L.lookup bid t

tests :: Spec
tests = do
  (results, endState, feeTotal) <- runIO runWithIntermediateStates
  describe "Baker transactions." $ do
    specify "Result state satisfies invariant" $
        case invariantBlockState endState feeTotal of
            Left f -> expectationFailure f
            Right _ -> return ()
    specify "Correct number of transactions" $
        length results `shouldBe` length transactionsInput
    specify "Adding two bakers from initial empty state" $
        case take 2 results of
          [([(_,Types.TxSuccess [Types.BakerAdded {ebaBakerId = 0}])],[],bps1),
           ([(_,Types.TxSuccess [Types.BakerAdded {ebaBakerId = 1}])],[],bps2)] -> do
            Set.toAscList (bps1 ^. birkActiveBakers . activeBakers) `shouldBe` [0]
            Set.toAscList (bps2 ^. birkActiveBakers . activeBakers) `shouldBe` [0,1]
          _ -> expectationFailure $ show (take 2 results)
    specify "Fail to add baker with duplicate aggregation key" $
      case results !! 2 of
        ([(_, Types.TxReject (Types.DuplicateAggregationKey _))], [], bps) ->
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1]
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Add baker with duplicate sign and election keys" $
      case results !! 3 of
        ([(_,Types.TxSuccess [Types.BakerAdded {ebaBakerId = 2}])],[],bps) ->
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1,2]
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Update baker 0 with original keys" $
      case results !! 4 of
        ([(_,Types.TxSuccess [Types.BakerKeysUpdated 0 _ _ _ _])],[],bps) ->
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1,2]
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to update baker 0 with baker 1's aggregation key" $
      case results !! 5 of
        ([(_,Types.TxReject (Types.DuplicateAggregationKey _))],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to add baker with bad election key proof" $
      case results !! 6 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to add baker with bad sign key proof" $
      case results !! 7 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to add baker with bad aggregation key proof" $
      case results !! 8 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to remove non-existent baker" $ 
      case results !! 9 of
        ([(_,Types.TxReject (Types.NotABaker acct))],[],bps) -> do
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1,2]
          acct `shouldBe` account 3
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Remove baker 0" $
      case results !! 10 of
        ([(_,Types.TxSuccess [Types.BakerRemoved 0 acct])],[],bps) -> do
          -- The baker should still be in the active bakers due to cooldown
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1,2]
          acct `shouldBe` account 0
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to add baker with baker 0's keys" $ 
      case results !! 11 of
        ([(_,Types.TxReject (Types.DuplicateAggregationKey _))],[],bps) ->
          -- The baker should still be in the active bakers due to cooldown
          Set.toAscList (bps ^. birkActiveBakers . activeBakers) `shouldBe` [0,1,2]
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to update baker with bad election key proof" $
      case results !! 12 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to update baker with bad sign key proof" $
      case results !! 13 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r
    specify "Fail to update baker with bad aggregation key proof" $
      case results !! 14 of
        ([(_,Types.TxReject Types.InvalidProof)],[],_) -> return ()
        r -> expectationFailure $ "Unexpected outcome: " ++ show r