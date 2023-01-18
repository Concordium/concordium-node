{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.BakerTransactions (tests) where

import Control.Monad
import Data.Bifunctor (first)
import qualified Data.List as List
import Lens.Micro.Platform
import Test.Hspec

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import qualified Concordium.Scheduler as Sch
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types.Accounts (
    bakerAggregationVerifyKey,
    bakerElectionVerifyKey,
    bakerInfo,
    bakerSignatureVerifyKey,
 )

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.DummyData
import Concordium.Scheduler.DummyData
import System.IO.Unsafe

import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.TestUtils

keyPair :: Int -> SigScheme.KeyPair
keyPair = Helpers.keyPairFromSeed

account :: Int -> Types.AccountAddress
account = Helpers.accountAddressFromSeed

initialBlockState ::
    (Types.IsProtocolVersion pv) =>
    Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM $
        fmap (Helpers.makeTestAccountFromSeed 400_000_000_000) [0 .. 3]

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
    [ -- Add baker on account 0 (OK)
      TJSON
        { payload =
            AddBaker
                (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker0 ^. _4)
                300_000_000_000
                True,
          metadata = makeDummyHeader (account 0) 1 10_000,
          keys = [(0, [(0, keyPair 0)])]
        },
      -- Add baker on account 1 (OK)
      TJSON
        { payload =
            AddBaker
                (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker1 ^. _2)
                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker1 ^. _3)
                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker1 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 1) 1 10_000,
          keys = [(0, [(0, keyPair 1)])]
        },
      -- Add baker on account 2, duplicate aggregation key of baker 0 (FAIL)
      TJSON
        { payload =
            AddBaker
                (baker2 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker2 ^. _2)
                (baker2 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker2 ^. _3)
                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker0 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 2) 1 10_000,
          keys = [(0, [(0, keyPair 2)])]
        },
      -- Add baker on account 2, duplicate sign and election key of baker 0 (OK)
      TJSON
        { payload =
            AddBaker
                (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker2 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker2 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 2) 2 10_000,
          keys = [(0, [(0, keyPair 2)])]
        },
      -- Update baker 0 with original keys (OK)
      TJSON
        { payload =
            UpdateBakerKeys
                (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker0 ^. _4),
          metadata = makeDummyHeader (account 0) 2 10_000,
          keys = [(0, [(0, keyPair 0)])]
        },
      -- Update baker 0 with baker1's aggregation key (FAIL)
      TJSON
        { payload =
            UpdateBakerKeys
                (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker1 ^. _4),
          metadata = makeDummyHeader (account 0) 3 10_000,
          keys = [(0, [(0, keyPair 0)])]
        },
      -- Add baker on account 3, bad election key proof (FAIL)
      TJSON
        { payload =
            AddBaker
                (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker3 ^. _3)
                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker3 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 3) 1 10_000,
          keys = [(0, [(0, keyPair 3)])]
        },
      -- Add baker on account 3, bad sign key proof (FAIL)
      TJSON
        { payload =
            AddBaker
                (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker3 ^. _2)
                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker3 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 3) 2 10_000,
          keys = [(0, [(0, keyPair 3)])]
        },
      -- Add baker on account 3, bad aggregation key proof (FAIL)
      TJSON
        { payload =
            AddBaker
                (baker3 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker3 ^. _2)
                (baker3 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker3 ^. _3)
                (baker3 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker0 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 3) 3 10_000,
          keys = [(0, [(0, keyPair 3)])]
        },
      -- Remove baker 3 (FAIL)
      TJSON
        { payload = RemoveBaker,
          metadata = makeDummyHeader (account 3) 4 10_000,
          keys = [(0, [(0, keyPair 3)])]
        },
      -- Remove baker 0 (OK)
      TJSON
        { payload = RemoveBaker,
          metadata = makeDummyHeader (account 0) 4 10_000,
          keys = [(0, [(0, keyPair 0)])]
        },
      -- Add baker on account 3 with baker 0's keys (FAIL)
      -- This fails because baker 0 remains valid during cooldown.
      TJSON
        { payload =
            AddBaker
                (baker0 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker0 ^. _2)
                (baker0 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker0 ^. _3)
                (baker0 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker0 ^. _4)
                300_000_000_000
                False,
          metadata = makeDummyHeader (account 3) 5 10_000,
          keys = [(0, [(0, keyPair 3)])]
        },
      -- Update baker 1 with bad sign key proof (FAIL)
      TJSON
        { payload =
            UpdateBakerKeys
                (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker3 ^. _2)
                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker1 ^. _3)
                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker1 ^. _4),
          metadata = makeDummyHeader (account 1) 2 10_000,
          keys = [(0, [(0, keyPair 1)])]
        },
      -- Update baker 1 with bad election key proof (FAIL)
      TJSON
        { payload =
            UpdateBakerKeys
                (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker1 ^. _2)
                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker3 ^. _3)
                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker1 ^. _4),
          metadata = makeDummyHeader (account 1) 3 10_000,
          keys = [(0, [(0, keyPair 1)])]
        },
      -- Update baker 1 with bad aggregation key proof (FAIL)
      TJSON
        { payload =
            UpdateBakerKeys
                (baker1 ^. _1 . bakerInfo . bakerElectionVerifyKey)
                (baker3 ^. _2)
                (baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey)
                (baker1 ^. _3)
                (baker1 ^. _1 . bakerInfo . bakerAggregationVerifyKey)
                (baker3 ^. _4),
          metadata = makeDummyHeader (account 1) 4 10_000,
          keys = [(0, [(0, keyPair 1)])]
        }
    ]

tests :: Spec
tests = do
    let (outcomes, endState) = unsafePerformIO $ do
            txs <- processUngroupedTransactions transactionsInput
            Helpers.runSchedulerTestWithIntermediateStates
                @PV1
                Helpers.defaultTestConfig
                initialBlockState
                (const BS.bsoGetActiveBakers)
                txs
    let results = first (Helpers.getResults . Sch.ftAdded . Helpers.srTransactions) <$> outcomes

    describe "P1: Baker transactions." $ do
        specify "Result state satisfies invariant" $ do
            let feeTotal = sum $ Helpers.srExecutionCosts . fst <$> outcomes
            join $ Helpers.runTestBlockState $ Helpers.assertBlockStateInvariants endState feeTotal
        specify "Correct number of transactions" $
            length results `shouldBe` length transactionsInput
        specify "No failed transactions" $ do
            let failedTransactions =
                    List.concatMap
                        (Sch.ftFailed . Helpers.srTransactions . fst)
                        outcomes
            failedTransactions `shouldSatisfy` List.null
        specify "Adding two bakers from initial empty state" $
            case take 2 results of
                [ ([(_, Types.TxSuccess [Types.BakerAdded{ebaBakerId = 0}])], bakers1),
                  ([(_, Types.TxSuccess [Types.BakerAdded{ebaBakerId = 1}])], bakers2)
                    ] -> do
                        bakers1 `shouldBe` [0]
                        bakers2 `shouldBe` [0, 1]
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to add baker with duplicate aggregation key" $
            case results !! 2 of
                ([(_, Types.TxReject (Types.DuplicateAggregationKey _))], bakers) ->
                    bakers `shouldBe` [0, 1]
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Add baker with duplicate sign and election keys" $
            case results !! 3 of
                ([(_, Types.TxSuccess [Types.BakerAdded{ebaBakerId = 2}])], bakers) ->
                    bakers `shouldBe` [0, 1, 2]
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Update baker 0 with original keys" $
            case results !! 4 of
                ([(_, Types.TxSuccess [Types.BakerKeysUpdated 0 _ _ _ _])], bakers) ->
                    bakers `shouldBe` [0, 1, 2]
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to update baker 0 with baker 1's aggregation key" $
            case results !! 5 of
                ([(_, Types.TxReject (Types.DuplicateAggregationKey _))], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to add baker with bad election key proof" $
            case results !! 6 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to add baker with bad sign key proof" $
            case results !! 7 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to add baker with bad aggregation key proof" $
            case results !! 8 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to remove non-existent baker" $
            case results !! 9 of
                ([(_, Types.TxReject (Types.NotABaker acct))], bakers) -> do
                    bakers `shouldBe` [0, 1, 2]
                    acct `shouldBe` account 3
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Remove baker 0" $
            case results !! 10 of
                ([(_, Types.TxSuccess [Types.BakerRemoved 0 acct])], bakers) -> do
                    -- The baker should still be in the active bakers due to cooldown
                    bakers `shouldBe` [0, 1, 2]
                    acct `shouldBe` account 0
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to add baker with baker 0's keys" $
            case results !! 11 of
                ([(_, Types.TxReject (Types.DuplicateAggregationKey _))], bakers) ->
                    -- The baker should still be in the active bakers due to cooldown
                    bakers `shouldBe` [0, 1, 2]
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to update baker with bad election key proof" $
            case results !! 12 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to update baker with bad sign key proof" $
            case results !! 13 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
        specify "Fail to update baker with bad aggregation key proof" $
            case results !! 14 of
                ([(_, Types.TxReject Types.InvalidProof)], _) -> return ()
                r -> expectationFailure $ "Unexpected outcome: " ++ show r
