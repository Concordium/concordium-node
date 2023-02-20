{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConcordiumTests.LeaderElectionTest where

import Data.Word
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types.SeedState

import Concordium.Types

import Concordium.Kontrol.UpdateLeaderElectionParameters
import System.Random

-- Tests that both Maybe List's are Just, that the second Just is a singleton,
-- and that the element of the singleton is an element of the first list.
singletonListElementOfExpectation :: Maybe [LeadershipElectionNonce] -> Maybe [LeadershipElectionNonce] -> Expectation
singletonListElementOfExpectation (Just a) (Just [b]) = a `shouldSatisfy` elem b
singletonListElementOfExpectation _ _ = expectationFailure "Both arguments should be Just, and the second Just should contain a list with one element."

-- Some concrete test cases for @predictLeadershipElectionNonce@.
testPredictFuture :: [Expectation]
testPredictFuture =
    [ predictLeadershipElectionNonce ss 6 Nothing 8 `shouldBe` Just [computeLeadershipElectionNonce ss 8],
      predictLeadershipElectionNonce ss 6 (Just 7) 8 `shouldBe` Just [computeLeadershipElectionNonce ss 8],
      predictLeadershipElectionNonce ss 7 Nothing 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15, computeLeadershipElectionNonce ss' 15],
      predictLeadershipElectionNonce ss 7 (Just 8) 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15],
      predictLeadershipElectionNonce ss 7 (Just 11) 15 `shouldBe` Just [computeLeadershipElectionNonce ss' 15],
      singletonListElementOfExpectation (predictLeadershipElectionNonce ss 7 Nothing 15) (predictLeadershipElectionNonce ss' 10 Nothing 15),
      predictLeadershipElectionNonce ss 6 (Just 8) 15 `shouldBe` predictLeadershipElectionNonce ss'' 7 (Just 8) 15,
      predictLeadershipElectionNonce ss 6 Nothing 15 `shouldBe` predictLeadershipElectionNonce ss'' 7 Nothing 15,
      predictLeadershipElectionNonce ss 6 Nothing 8 `shouldBe` predictLeadershipElectionNonce ss'' 7 Nothing 8,
      predictLeadershipElectionNonce ss 7 (Just 11) 15 `shouldBe` predictLeadershipElectionNonce ss' 10 (Just 11) 15,
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
    ss =
        SeedStateV0
            { epochLength = 9,
              epoch = 0,
              currentLeadershipElectionNonce = H.hash "1",
              updatedNonce = H.hash "2"
            }
    vrfKP = fst $ VRF.randomKeyPair (mkStdGen 0)
    bn = VRF.prove vrfKP "3"
    ss' = updateSeedState 10 bn ss
    ss'' = updateSeedState 7 bn ss

genSeedstate :: Gen (SeedState 'SeedStateVersion0)
genSeedstate =
    do
        (el :: Word64) <- (* 9) <$> choose (1, 10000)
        epoch <- choose (0, 1000)
        return
            SeedStateV0
                { epochLength = fromIntegral el,
                  currentLeadershipElectionNonce = H.hash "1",
                  updatedNonce = H.hash "2",
                  ..
                }

-- Tests that both Maybe List's are Just, that the second Just is a singleton,
-- and that the element of the singleton is an element of the first list.
singletonElementOfProperty :: Maybe [LeadershipElectionNonce] -> Maybe [LeadershipElectionNonce] -> Property
singletonElementOfProperty (Just a) (Just [b]) = counterexample "Prediction from second seedstate should be one of the predictions in the first." $ elem b a
singletonElementOfProperty _ _ = counterexample "Both arguments should be Just, and the second Just should contain a list with one element." False

-- Generates a seedstate meant to be from a block in the last third of an epoch and tests @predictLeadershipElectionNonce@
-- on this seedstate and a target slot in the next epoch. It tests against @computeLeadershipElectionNonce@ and tests that
-- @predictLeadershipElectionNonce@ gives the same prediction on an updated seedstate for the same target slot.
makePropertyJust :: Property
makePropertyJust = property $ do
    ss <- genSeedstate
    let SeedStateV0{..} = ss
    let epochSlot = epoch * fromIntegral epochLength
    let nextEpoch = epochSlot + fromIntegral epochLength
    let twoThirds = 2 * fromIntegral epochLength `div` 3
    -- Find a slot in the last 2/3 of the epoch
    seedStateSlot :: Slot <- fromIntegral <$> choose (epochSlot + twoThirds, nextEpoch - 2) -- We subtract 2 so that we can update the seedstate and still be in the same epoch.
    parentInThisEpoch :: Slot <- fromIntegral <$> choose (fromIntegral seedStateSlot + 1, nextEpoch - 1) -- For test cases where the parent of the pending block is in the same epoch as the seed state
    parentInNextEpoch :: Slot <- fromIntegral <$> choose (nextEpoch + 1, nextEpoch + fromIntegral epochLength - 1) -- For test cases where the parent of the pending block in the same epoch as the target slot
    let vrfKP = fst $ VRF.randomKeyPair (mkStdGen 0)
        bn = VRF.prove vrfKP "3"
        seedStateSlot' = fromIntegral nextEpoch + 1
        ss' = updateSeedState seedStateSlot' bn ss -- update the seedstate for a slot in the next epoch
        seedStateSlot'' = seedStateSlot + 1
        ss'' = updateSeedState seedStateSlot'' bn ss -- update the seedstate for a slot in the same epoch
    slotToPredictFor :: Slot <- fromIntegral <$> choose (fromIntegral parentInNextEpoch, nextEpoch + fromIntegral epochLength - 1) -- the target slot in the next epoch
    return $
        conjoin
            [ counterexample "Without knowledge of pending block's parent" $ predictLeadershipElectionNonce ss seedStateSlot Nothing slotToPredictFor === Just [computeLeadershipElectionNonce ss slotToPredictFor, computeLeadershipElectionNonce ss' slotToPredictFor],
              counterexample "With parent in same epoch as the last finalized block" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInThisEpoch) slotToPredictFor === Just [computeLeadershipElectionNonce ss slotToPredictFor],
              counterexample "With parent in the same epoch as the target slot" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictFor === Just [computeLeadershipElectionNonce ss' slotToPredictFor],
              singletonElementOfProperty (predictLeadershipElectionNonce ss seedStateSlot Nothing slotToPredictFor) (predictLeadershipElectionNonce ss' seedStateSlot' Nothing slotToPredictFor),
              counterexample "Prediction invariant under seedstate update, with parent in same epoch as the last finalized block" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInThisEpoch) slotToPredictFor === predictLeadershipElectionNonce ss'' seedStateSlot'' (Just parentInThisEpoch) slotToPredictFor,
              counterexample "Prediction invariant under seedstate update across an epoch, with parent in the same epoch as the target slot" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictFor === predictLeadershipElectionNonce ss' seedStateSlot' (Just parentInNextEpoch) slotToPredictFor,
              counterexample "Prediction invariant under seedstate update in same epoch, with parent in the same epoch as the target slot" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictFor === predictLeadershipElectionNonce ss'' seedStateSlot'' (Just parentInNextEpoch) slotToPredictFor
            ]

-- Tests cases where the leadership election nonce cannot be predicted, either because the seedstate slot is too early (i.e. in the first 2/3 of the epoch), or
-- if the target slot is too far in the future (i.e. two epochs after the seed state or later)
makePropertyNothing :: Property
makePropertyNothing = property $ do
    ss <- genSeedstate
    let SeedStateV0{..} = ss
    let epochSlot = epoch * fromIntegral epochLength
    let nextEpoch = epochSlot + fromIntegral epochLength
    let twoThirds = 2 * fromIntegral epochLength `div` 3
    -- Find a slot in the last 2/3 of the epoch
    seedStateSlot :: Slot <- fromIntegral <$> choose (epochSlot + twoThirds, nextEpoch - 1)
    seedStateSlotTooEarly :: Slot <- fromIntegral <$> choose (epochSlot, epochSlot + twoThirds - 1)
    parentInThisEpoch :: Slot <- fromIntegral <$> choose (fromIntegral seedStateSlot, nextEpoch - 1)
    parentInNextEpoch :: Slot <- fromIntegral <$> choose (nextEpoch, nextEpoch + fromIntegral epochLength - 1)
    slotToPredictFor :: Slot <- fromIntegral <$> choose (fromIntegral parentInNextEpoch, nextEpoch + fromIntegral epochLength - 1)
    slotToPredictTooLate :: Slot <- fromIntegral <$> choose (nextEpoch + fromIntegral epochLength, nextEpoch + 2 * fromIntegral epochLength - 1)
    return $
        conjoin
            [ counterexample "Seedstate slot too early, parent unknown" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly Nothing slotToPredictFor === Nothing,
              counterexample "Seedstate slot too early, parent in seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInThisEpoch) slotToPredictFor === Nothing,
              counterexample "Seedstate slot too early, parent in epoch after seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInNextEpoch) slotToPredictFor === Nothing,
              counterexample "Prediction slot too late, parent unknown" $ predictLeadershipElectionNonce ss seedStateSlot Nothing slotToPredictTooLate === Nothing,
              counterexample "Prediction slot too late, parent in seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInNextEpoch) slotToPredictTooLate === Nothing,
              counterexample "Prediction slot too late, parent in epoch after seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlot (Just parentInThisEpoch) slotToPredictTooLate === Nothing,
              counterexample "Seedstate slot too early and prediction slot too late, parent unknown" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly Nothing slotToPredictTooLate === Nothing,
              counterexample "Seedstate slot too early and prediction slot too late, parent in seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInThisEpoch) slotToPredictTooLate === Nothing,
              counterexample "Seedstate slot too early and prediction slot too late, parent in epoch after seedstate epoch" $ predictLeadershipElectionNonce ss seedStateSlotTooEarly (Just parentInNextEpoch) slotToPredictTooLate === Nothing
            ]

tests :: Spec
tests = describe "LeaderElectionTest" $ do
    specify "Correct prediction of leadership election nonces" $ withMaxSuccess 100 makePropertyJust
    specify "No prediction due to too early finalized slot or prediction slot too far in future" $ withMaxSuccess 100 makePropertyNothing
    it "PredictFuture" $ sequence_ testPredictFuture
