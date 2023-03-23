{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity
import Data.IORef
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.IO.Unsafe
import Test.HUnit hiding (State)
import Test.Hspec
import Test.QuickCheck
import Unsafe.Coerce

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.KonsensusV1.Consensus.Quorum
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types

import Concordium.KonsensusV1.TreeState.Implementation
import ConcordiumTests.KonsensusV1.TreeStateTest
import ConcordiumTests.KonsensusV1.Types

-- |Generate a random 'VoterPower'.
genFinalizerWeight :: Gen VoterPower
genFinalizerWeight = VoterPower <$> arbitrary

-- |Generate a 'QuorumMessage' for a particular block.
genQuorumMessageFor :: BlockHash -> Gen QuorumMessage
genQuorumMessageFor bh = do
    qmSignature <- genQuorumSignature
    qmFinalizerIndex <- genFinalizerIndex
    qmRound <- genRound
    qmEpoch <- genEpoch
    return QuorumMessage{qmBlock = bh, ..}

-- |Test for ensuring that when a
-- new 'QuorumMessage' is added to the 'QuorumMessages' type,
-- then the weight is being accummulated and signatures are aggregated.
propAddQuorumMessage :: Property
propAddQuorumMessage =
    forAll genQuorumMessage $ \qm0 ->
        forAll (genQuorumMessageFor (qmBlock qm0)) $ \qm1 ->
            (qm0 /= qm1) ==>
                forAll genFinalizerWeight $ \weight -> do
                    let verifiedQuorumMessage0 = VerifiedQuorumMessage qm0 weight
                        qsm' = addQuorumMessage verifiedQuorumMessage0 emptyQuorumMessages
                    assertEqual
                        "The quorum message should have been added"
                        (qsm' ^? smFinalizerToQuorumMessage . ix (qmFinalizerIndex qm0))
                        (Just qm0)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0)))
                    assertEqual
                        "The finalizer weight, signature and finalizer index should be present"
                        (weight, qmSignature qm0, Set.singleton $ qmFinalizerIndex qm0)
                        (fromJust $! qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0))
                    let verifiedQuorumMessage1 = VerifiedQuorumMessage qm1 weight
                        qsm'' = addQuorumMessage verifiedQuorumMessage1 qsm'
                    assertEqual
                        "The quorum message should have been added"
                        (qsm'' ^? smFinalizerToQuorumMessage . ix (qmFinalizerIndex qm1))
                        (Just qm1)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1)))
                    assertEqual
                        "The finalizer weight, aggregated signature and finalizer indecies should be present"
                        (2 * weight, qmSignature qm1 <> qmSignature qm0, Set.insert (qmFinalizerIndex qm1) (Set.singleton $ qmFinalizerIndex qm0))
                        (fromJust $! qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1))

-- |Test that checks that a 'QuorumCertificate' can be formed when
-- there are enough finalizers (weighted) who have signed off a round.
testMakeQuorumCertificate :: Spec
testMakeQuorumCertificate = describe "Quorum Certificate creation" $ do
    it "should not create a qc as there are not enough weight" $ do
        assertEqual
            "No quorum certificate should be created"
            Nothing
            (makeQuorumCertificate bh sd)
    it "should create a certificate when there is enough weight" $ do
        assertEqual
            "A quorum certificate should have been generated"
            (Just (QuorumCertificate bh 0 0 (emptyQuorumSignature <> emptyQuorumSignature) (finalizerSet $ FinalizerIndex <$> [1, 2])))
            (makeQuorumCertificate bh sd')
  where
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (unsafeCoerce fIdx))
    blsPublicKey = Bls.derivePublicKey someBlsSecretKey
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    sigPublicKey = Sig.verifyKey $ unsafePerformIO Sig.newKeyPair
    bfs =
        BakersAndFinalizers
            { _bfBakers = FullBakers Vec.empty 0,
              _bfFinalizers = FinalizationCommittee (Vec.fromList $ fi <$> [1, 2, 3]) 3
            }
    -- A skov data not capable of forming a quorum certificate
    sd =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers 0 bfs bfs bfs 1
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 1 1)
    -- A skov data capable of forming a quorum certificate
    sd' =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers 0 bfs bfs bfs 1
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 1 1)
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 2 1)
    bh = BlockHash minBound
    verifiedQuorumMessage finalizerIndex weight = VerifiedQuorumMessage (quorumMessage finalizerIndex) weight
    quorumMessage finalizerIndex = QuorumMessage emptyQuorumSignature bh (FinalizerIndex finalizerIndex) 0 0
    emptyQuorumSignature = QuorumSignature $ Bls.emptySignature

-- |Tests for receiving a quorum message.
-- In particular this test checks that the return codes are as expected
-- with respect to the received 'QuorumMessage'
testReceiveQuorumMessage :: Spec
testReceiveQuorumMessage = describe "Receive quorum message" $ do
    it "future epoch triggers catchup" $ receiveAndCheck (quorumMessage 1 0 2) CatchupRequired
    it "obsolete round rejects" $ receiveAndCheck (quorumMessage 1 0 0) Rejected
    it "invalid finalizer rejects" $ receiveAndCheck (quorumMessage 2 1 1) Rejected
  where
    bh = BlockHash minBound
    quorumMessage finalizerIndex theRound epoch = QuorumMessage emptyQuorumSignature bh (FinalizerIndex finalizerIndex) theRound epoch
    emptyQuorumSignature = QuorumSignature $ Bls.emptySignature
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (unsafeCoerce fIdx))
    blsPublicKey = Bls.derivePublicKey someBlsSecretKey
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    sigPublicKey = Sig.verifyKey $ unsafePerformIO Sig.newKeyPair
    bakersAndFinalizers = BakersAndFinalizers (FullBakers Vec.empty 0) (FinalizationCommittee (Vec.singleton (fi 1)) 1)
    -- A skov data where
    -- - the current round and epoch is set to 1.
    -- - there is one finalizer (with index 1).
    sd =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round 1
            & skovEpochBakers . currentEpoch .~ 1
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
    -- Run the 'receiveQuorumMessage' action.
    receiveAndCheck qm expect = do
        resultCode <- runTestLLDB (lldbWithGenesis @'P6) $ receiveQuorumMessage qm sd
        resultCode `shouldBe` expect

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    it "Adding a quorum message" propAddQuorumMessage
    testMakeQuorumCertificate
    testReceiveQuorumMessage
