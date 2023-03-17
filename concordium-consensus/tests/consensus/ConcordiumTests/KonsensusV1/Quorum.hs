-- |Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Concordium.KonsensusV1.Consensus.Quorum


testReceiveQuorumMessage :: Spec
testReceiveQuorumMessage = describe "receiveQuorumMessage" $ do
    it "obsolete round" $ foo `shouldBe` Rejected
  where
    foo = Rejected

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    describe "receiveQuorumMessage" $ do
        testReceiveQuorumMessage
  
