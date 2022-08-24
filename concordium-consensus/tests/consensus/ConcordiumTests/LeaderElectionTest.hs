{-# LANGUAGE OverloadedStrings #-}
module ConcordiumTests.LeaderElectionTest where

import Test.Hspec

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types.SeedState

import Concordium.Kontrol.UpdateLeaderElectionParameters
import System.Random

testPredictFuture :: [Expectation]
testPredictFuture = [
                        predictLeadershipElectionNonce ss 7 Nothing 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15, computeLeadershipElectionNonce ss' 15],
                        predictLeadershipElectionNonce ss 7 (Just 8) 15 `shouldBe` Just [computeLeadershipElectionNonce ss 15],
                        predictLeadershipElectionNonce ss 7 (Just 11) 15 `shouldBe` Just [computeLeadershipElectionNonce ss' 15]
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

tests :: Spec
tests = describe "LeaderElectionTest" $ do
    mapM_ (it "PredictFuture") testPredictFuture