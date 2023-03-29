{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.IO.Unsafe
import Test.HUnit hiding (State)
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache)
import Concordium.KonsensusV1.Consensus.Quorum
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.Transactions

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
    it "should not create a qc as there are no signatures present" $ do
        assertEqual
            "No quorum certificate should be created"
            Nothing
            (makeQuorumCertificate bh sdNoMessages)
  where
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (fromIntegral fIdx))
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
    sdNoMessages =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers 0 bfs bfs bfs 1
    bh = BlockHash minBound
    verifiedQuorumMessage finalizerIndex weight = VerifiedQuorumMessage (quorumMessage finalizerIndex) weight
    quorumMessage finalizerIndex = QuorumMessage emptyQuorumSignature bh (FinalizerIndex finalizerIndex) 0 0
    emptyQuorumSignature = QuorumSignature $ Bls.emptySignature

-- |Tests for receiving a quorum message.
-- In particular this test checks that the return codes are as expected
-- with respect to the received 'QuorumMessage'
testReceiveQuorumMessage :: Spec
testReceiveQuorumMessage = describe "Receive quorum message" $ do
    it "future epoch triggers catchup" $ receiveAndCheck sd messageFromFuture CatchupRequired
    it "obsolete round rejects" $ receiveAndCheck sd obsoleteMessage $ Rejected ObsoleteRound
    it "invalid finalizer rejects" $ receiveAndCheck sd invalidFinalizerMessage $ Rejected NotAFinalizer
    it "duplicate message" $ receiveAndCheck sd duplicateMessage Duplicate
    it "invalid signature" $ receiveAndCheck sd invalidSignatureMessage $ Rejected InvalidSignature
    it "double signing" $ receiveAndCheck sd doubleSigningMessage $ Rejected AlreadySigned
    it "unknown block" $ receiveAndCheck sd unknownBlockMessage $ CatchupRequired
    it "invalid block | dead" $ receiveAndCheck sd deadBlockMessage $ Rejected InvalidBlock
    it "round inconsistency" $ receiveAndCheck (sd' 0 1) inconsistentRoundsMessage $ Rejected InconsistentRounds
    it "epoch inconsistency" $ receiveAndCheck (sd' 1 1) inconsistentEpochsMessage $ Rejected InconsistentEpochs
    it "receives" $ receiveAndCheck (sd' 1 1) verifiableMessage $ Received $ VerifiedQuorumMessage verifiableMessage 1
  where
    bh = BlockHash minBound
    emptyQuorumSignature = QuorumSignature $ Bls.emptySignature
    -- a quorum message from the specified finalizer and for the specified round and epoch.
    -- The signature is invalid on this quorum message.
    messageFromFuture = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 1) 1 2
    obsoleteMessage = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 1) 0 0
    invalidFinalizerMessage = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 4) 1 1
    duplicateMessage = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 1) 1 1
    invalidSignatureMessage = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 2) 1 1
    doubleSigningMessage =
        let someQM = QuorumMessage emptyQuorumSignature bh (FinalizerIndex 1) 1 1
        in  someQM{qmSignature = quorumMessageSignature someQM}
    unknownBlockMessage =
        let someQM = QuorumMessage emptyQuorumSignature (BlockHash $ Hash.hash "someOtherBlockHash") (FinalizerIndex 2) 1 1
        in  someQM{qmSignature = quorumMessageSignature someQM}
    deadBlockMessage =
        let someQM = QuorumMessage emptyQuorumSignature deadBlock (FinalizerIndex 2) 1 1
        in  someQM{qmSignature = quorumMessageSignature someQM}
    inconsistentRoundsMessage =
        let someQM = QuorumMessage emptyQuorumSignature liveBlock (FinalizerIndex 2) 1 1
        in  someQM{qmSignature = quorumMessageSignature someQM}
    inconsistentEpochsMessage =
        let someQM = QuorumMessage emptyQuorumSignature liveBlock (FinalizerIndex 2) 1 0
        in  someQM{qmSignature = quorumMessageSignature someQM}
    verifiableMessage =
        let someQM = QuorumMessage emptyQuorumSignature liveBlock (FinalizerIndex 2) 1 1
        in  someQM{qmSignature = quorumMessageSignature someQM}
    -- Compute the signature for the message.
    quorumMessageSignature qm = signQuorumSignatureMessage (quorumSignatureMessageFor qm dummyGenesisBlockHash) someBlsSecretKey
    -- A finalizer with the specified finalizer (and bakerid) with a weight of 1 in the finalization committee.
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (fromIntegral fIdx))
    blsPublicKey = Bls.derivePublicKey someBlsSecretKey
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    sigPublicKey = Sig.verifyKey $ unsafePerformIO Sig.newKeyPair
    bakersAndFinalizers = BakersAndFinalizers (FullBakers Vec.empty 0) (FinalizationCommittee (Vec.fromList [fi 1, fi 2, fi 3]) 3)
    -- A skov data where
    -- - the current round and epoch is set to 1 (next payday is at epoch 2).
    -- - there is 3 finalizers 1, 2 and 3 each with a weight of 1 (total weight is 3).
    -- - one collected quorum message from the present finalizer index for round 1 and epoch 1.
    deadBlock = BlockHash $ Hash.hash "dead block"
    liveBlock = BlockHash $ Hash.hash "live block"
    -- A dummy block pointer with no meaningful state.
    -- It has round and epoch set to 0 for testing round/epoch inconsistencies.
    bakedBlock r e = BakedBlock r e 0 0 (dummyQuorumCertificate $ BlockHash minBound) Absent Absent dummyBlockNonce Vec.empty emptyTransactionOutcomesHashV1 (StateHashV0 $ Hash.hash "empty state hash")
    liveBlockPointer r e =
        BlockPointer
            { bpInfo =
                BlockMetadata
                    { bmHeight = 0,
                      bmReceiveTime = timestampToUTCTime 0,
                      bmArriveTime = timestampToUTCTime 0
                    },
              bpBlock = NormalBlock $ SignedBlock (bakedBlock r e) liveBlock (Sig.sign (unsafePerformIO Sig.newKeyPair) "foo"),
              bpState = dummyBlockState
            }
    -- the round and epoch here is for making it easier to trigger the various cases
    -- with respect to the alive block (i.e. we set the focus block here).
    sd = sd' 1 1
    -- the round and epoch here is for making it easier to trigger the various cases
    -- with respect to the alive block (i.e. we set the focus block here).
    sd' r e =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round r
            & skovEpochBakers . currentEpoch .~ e
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . nextPayday .~ 2
            & currentQuorumMessages %~ addQuorumMessage (VerifiedQuorumMessage duplicateMessage 1)
            & blockTable . deadBlocks %~ insertDeadCache deadBlock
            & skovPendingTransactions . focusBlock .~ (liveBlockPointer (Round r) e)
    -- Run the 'receiveQuorumMessage' action.
    receiveAndCheck skovData qm expect = do
        resultCode <- runTestLLDB (lldbWithGenesis @'P6) $ receiveQuorumMessage qm skovData
        resultCode `shouldBe` expect

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    it "Adding a quorum message" propAddQuorumMessage
    testMakeQuorumCertificate
    testReceiveQuorumMessage
