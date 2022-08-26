{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ConcordiumTests.LeaderElectionTest where

import Test.Hspec
import Test.QuickCheck
import Data.Word

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types.SeedState

import Concordium.Kontrol.UpdateLeaderElectionParameters
import System.Random
import Concordium.Scheduler.Types (Slot)

testPredictFuture :: [Expectation]
testPredictFuture = [
                        predictLeadershipElectionNonce ss 7 Nothing 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15, computeLeadershipElectionNonce ss' 15],
                        predictLeadershipElectionNonce ss 7 (Just 8) 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15],
                        predictLeadershipElectionNonce ss 7 (Just 11) 15 `shouldBe` Just [computeLeadershipElectionNonce ss' 15],
                        predictLeadershipElectionNonce ss 7 (Just 8) 20 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 7 (Just 11) 20 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 7 Nothing 20 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 (Just 8) 15 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 (Just 11) 15 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 Nothing 15 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 (Just 8) 20 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 (Just 11) 20 `shouldBe` Nothing,
                        predictLeadershipElectionNonce ss 5 Nothing 20 `shouldBe` Nothing
                    ]
    where
        ss = SeedState {
            epochLength = 9,
            epoch = 0,
            currentLeadershipElectionNonce = H.hash "1",
            updatedNonce = H.hash "2"
        }
        vrfKP = fst $ VRF.randomKeyPair (mkStdGen 0)
        bn = VRF.prove vrfKP "3"
        ss' = updateSeedState 10 bn ss

genSeedstate :: Gen SeedState
genSeedstate =
    do
        (el :: Word64) <- (*9) <$> choose (1,10000)
        epoch <- choose (0, 1000)
        return SeedState {
            epochLength = fromIntegral el,
            currentLeadershipElectionNonce = H.hash "1",
            updatedNonce = H.hash "2",
            ..
        }


makePropertyJust :: Property
makePropertyJust = property $ do
    ss@SeedState{..} <- genSeedstate
    let epochSlot = epoch * fromIntegral epochLength
    let nextEpoch = epochSlot + fromIntegral epochLength
    let toThirds = 2 * fromIntegral epochLength `div` 3
    -- Find a slot in the last 2/3 of the epoch
    seedStateSlot :: Slot <- fromIntegral <$> choose (epochSlot + toThirds, nextEpoch-1)
    parentInThisEpoch :: Slot <- fromIntegral <$> choose (fromIntegral seedStateSlot, nextEpoch-1)
    parentInNextEpoch :: Slot <- fromIntegral <$> choose (nextEpoch, nextEpoch + fromIntegral epochLength-1)
    let vrfKP = fst $ VRF.randomKeyPair (mkStdGen 0)
        bn = VRF.prove vrfKP "3"
        ss' = updateSeedState (fromIntegral nextEpoch + 1) bn ss
    slotToPredictFor :: Slot <- fromIntegral <$> choose (fromIntegral parentInNextEpoch, nextEpoch + fromIntegral epochLength-1)

    return $ conjoin [
            counterexample "Without knowledge of pending block's parent" $ predictLeadershipElectionNonce ss seedStateSlot Nothing slotToPredictFor === Just [computeLeadershipElectionNonce ss slotToPredictFor, computeLeadershipElectionNonce ss' slotToPredictFor],
            counterexample "With parent in same epoch as the last finalized block" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInThisEpoch) slotToPredictFor === Just [computeLeadershipElectionNonce ss slotToPredictFor],
            counterexample "With parent in the same epoch as the target slot" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictFor === Just [computeLeadershipElectionNonce ss' slotToPredictFor]
        ]

makePropertyNothing :: Property
makePropertyNothing = property $ do
    ss@SeedState{..} <- genSeedstate
    let epochSlot = epoch * fromIntegral epochLength
    let nextEpoch = epochSlot + fromIntegral epochLength
    let toThirds = 2 * fromIntegral epochLength `div` 3
    -- Find a slot in the last 2/3 of the epoch
    seedStateSlot :: Slot <- fromIntegral <$> choose (epochSlot + toThirds, nextEpoch-1)
    seedStateSlotTooEarly :: Slot <- fromIntegral <$> choose (epochSlot, epochSlot + toThirds-1)
    parentInThisEpoch :: Slot <- fromIntegral <$> choose (fromIntegral seedStateSlot, nextEpoch-1)
    parentInNextEpoch :: Slot <- fromIntegral <$> choose (nextEpoch, nextEpoch + fromIntegral epochLength-1)
    slotToPredictFor :: Slot <- fromIntegral <$> choose (fromIntegral parentInNextEpoch, nextEpoch + fromIntegral epochLength-1)
    slotToPredictTooLate :: Slot <- fromIntegral <$> choose (nextEpoch + fromIntegral epochLength, nextEpoch + 2*fromIntegral epochLength - 1)
    return $ conjoin [
                        counterexample "Seedstate slot too early" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly Nothing slotToPredictFor === Nothing,
                        counterexample "Seedstate slot too early" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInThisEpoch) slotToPredictFor === Nothing,
                        counterexample "Seedstate slot too early" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInNextEpoch) slotToPredictFor === Nothing,
                        counterexample "Prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlot Nothing slotToPredictTooLate === Nothing,
                        counterexample "Prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictTooLate === Nothing,
                        counterexample "Prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInThisEpoch) slotToPredictTooLate === Nothing,
                        counterexample "Seedstate slot too early and prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly Nothing slotToPredictTooLate === Nothing,
                        counterexample "Seedstate slot too early and prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInThisEpoch) slotToPredictTooLate === Nothing,
                        counterexample "Seedstate slot too early and prediction slot too late" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInNextEpoch) slotToPredictTooLate === Nothing
                    ]

testPredictionJust :: Property
testPredictionJust = label "Prediction corrrect" makePropertyJust


testPredictionNothing :: Property
testPredictionNothing = label "No prediction possible" makePropertyNothing

tests :: Spec
tests = describe "LeaderElectionTest" $ do
    specify "Correct prediction of leadership election nonces" $ withMaxSuccess 100 testPredictionJust
    specify "No prediction due to too early finalized slot or prediction slot too far in future" $ withMaxSuccess 100 testPredictionNothing
    mapM_ (it "PredictFuture") testPredictFuture