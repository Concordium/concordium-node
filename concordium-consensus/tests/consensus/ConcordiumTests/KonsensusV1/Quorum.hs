{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit hiding (State)
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache)
import Concordium.KonsensusV1.Consensus.Quorum
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Types.Parameters

import Concordium.KonsensusV1.TreeState.Implementation
import qualified ConcordiumTests.KonsensusV1.Common as Common
import ConcordiumTests.KonsensusV1.TreeStateTest
import ConcordiumTests.KonsensusV1.Types

-- | Generate a random 'VoterPower'.
genFinalizerWeight :: Gen VoterPower
genFinalizerWeight = VoterPower <$> arbitrary

-- | Generate a baker ID.
genBakerId :: Gen BakerId
genBakerId = BakerId . AccountIndex <$> arbitrary

-- | Generate a 'QuorumMessage' for a particular block.
genQuorumMessageFor :: BlockHash -> Gen QuorumMessage
genQuorumMessageFor bh = do
    qmSignature <- genQuorumSignature
    qmFinalizerIndex <- genFinalizerIndex
    qmRound <- genRound
    qmEpoch <- genEpoch
    return QuorumMessage{qmBlock = bh, ..}

-- | Test for ensuring that when a
--  new 'QuorumMessage' is added to the 'QuorumMessages' type,
--  then the weight is being accumulated and signatures are aggregated.
propAddQuorumMessage :: SProtocolVersion pv -> Property
propAddQuorumMessage sProtocolVersion =
    forAll genQuorumMessage $ \qm0 ->
        forAll (genQuorumMessageFor (qmBlock qm0)) $ \qm1 ->
            (qm0 /= qm1) ==>
                forAll genFinalizerWeight $ \weight -> forAll genBakerId $ \bakerId0 -> forAll genBakerId $ \bakerId1 -> do
                    let verifiedQuorumMessage0 = VerifiedQuorumMessage qm0 weight bakerId0 $! Common.myBlockPointer sProtocolVersion 0 0
                        qsm' = addQuorumMessage verifiedQuorumMessage0 emptyQuorumMessages
                    assertEqual
                        "The quorum message should have been added"
                        (qsm' ^? smBakerIdToQuorumMessage . ix bakerId0)
                        (Just qm0)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0)))
                    assertEqual
                        "The finalizer weight, signature and finalizer index should be present"
                        (weight, qmSignature qm0, finalizerSet [qmFinalizerIndex qm0])
                        (fromJust $! qsm' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm0))
                    let verifiedQuorumMessage1 = VerifiedQuorumMessage qm1 weight bakerId1 $! Common.myBlockPointer sProtocolVersion 0 0
                        qsm'' = addQuorumMessage verifiedQuorumMessage1 qsm'
                    assertEqual
                        "The quorum message should have been added"
                        (qsm'' ^? smBakerIdToQuorumMessage . ix bakerId1)
                        (Just qm1)
                    assertBool
                        "The block hash can be looked up"
                        (isJust (qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1)))
                    assertEqual
                        "The finalizer weight, aggregated signature and finalizer indices should be present"
                        (2 * weight, qmSignature qm1 <> qmSignature qm0, finalizerSet [qmFinalizerIndex qm1, qmFinalizerIndex qm0])
                        (fromJust $! qsm'' ^? smBlockToWeightsAndSignatures . ix (qmBlock qm1))

-- | Test that checks that a 'QuorumCertificate' can be formed when
--  there are enough finalizers (weighted) who have signed off a round.
testMakeQuorumCertificate ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testMakeQuorumCertificate sProtocolVersion =
    describe "Quorum Certificate creation" $ do
        it "should not create a qc as there are not enough weight" $ do
            assertEqual
                "No quorum certificate should be created"
                Nothing
                (makeQuorumCertificate qcBlockPointer sd)
        it "should create a certificate when there is enough weight" $ do
            assertEqual
                "A quorum certificate should have been generated"
                (Just (QuorumCertificate qcBlockHash 1 0 (emptyQuorumSignature <> emptyQuorumSignature) (finalizerSet $ FinalizerIndex <$> [1, 2])))
                (makeQuorumCertificate qcBlockPointer sd')
        it "should not create a qc as there are no signatures present" $ do
            assertEqual
                "No quorum certificate should be created"
                Nothing
                (makeQuorumCertificate qcBlockPointer sdNoMessages)
  where
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 Common.sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (fromIntegral fIdx))
    blsPublicKey = Bls.derivePublicKey someBlsSecretKey
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    finalizers = FinalizationCommittee (Vec.fromList $ fi <$> [1, 2, 3]) 3
    bfs =
        BakersAndFinalizers
            { _bfBakersEx = FullBakersEx Vec.empty 0,
              _bfFinalizers = finalizers,
              _bfFinalizerHash = computeFinalizationCommitteeHash finalizers
            }
    -- A skov data not capable of forming a quorum certificate
    sd =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers bfs bfs bfs 1
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 1 1)
    -- A skov data capable of forming a quorum certificate
    sd' =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers bfs bfs bfs 1
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 1 1)
            & currentQuorumMessages %~ addQuorumMessage (verifiedQuorumMessage 2 1)
    sdNoMessages =
        dummyInitialSkovData
            & skovEpochBakers .~ EpochBakers bfs bfs bfs 1
    qcBlockHash = BlockHash minBound
    qcBlockPointer = Common.someBlockPointer sProtocolVersion qcBlockHash 1 0
    verifiedQuorumMessage finalizerIndex weight = VerifiedQuorumMessage (quorumMessage finalizerIndex) weight (fromIntegral finalizerIndex) $ Common.myBlockPointer sProtocolVersion 0 0
    quorumMessage finalizerIndex = QuorumMessage emptyQuorumSignature qcBlockHash (FinalizerIndex finalizerIndex) 0 0
    emptyQuorumSignature = QuorumSignature Bls.emptySignature

-- | Tests for receiving a quorum message.
--  In particular this test checks that the return codes are as expected
--  with respect to the received 'QuorumMessage'
testReceiveQuorumMessage ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testReceiveQuorumMessage sProtocolVersion =
    describe "Receive quorum message" $ do
        it "future epoch triggers catchup" $ receiveAndCheck sd messageFromFuture CatchupRequired
        it "obsolete round rejects" $ receiveAndCheck sd obsoleteMessage $ Rejected ObsoleteRound
        it "invalid finalizer rejects" $ receiveAndCheck sd invalidFinalizerMessage $ Rejected NotAFinalizer
        it "duplicate message" $ receiveAndCheck sd duplicateMessage $ Rejected Duplicate
        it "invalid signature" $ receiveAndCheck sd invalidSignatureMessage $ Rejected InvalidSignature
        it "double signing" $ receiveAndCheck sd doubleSigningMessage CatchupRequired
        it "unknown block" $ receiveAndCheck sd unknownBlockMessage CatchupRequired
        it "invalid block | dead" $ receiveAndCheck sd deadBlockMessage $ Rejected InvalidBlock
        it "round inconsistency" $ receiveAndCheck (sd' 0 1) inconsistentRoundsMessage $ Rejected InconsistentRounds
        it "epoch inconsistency" $ receiveAndCheck (sd' 1 1) inconsistentEpochsMessage $ Rejected InconsistentEpochs
        it "receives" $ receiveAndCheck (sd' 1 1) verifiableMessage $ Received $ VerifiedQuorumMessage verifiableMessage 1 2 $ liveBlockPointer 1 1
  where
    bh = BlockHash minBound
    emptyQuorumSignature = QuorumSignature Bls.emptySignature
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
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 Common.sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex (fromIntegral fIdx))
    blsPublicKey = Bls.derivePublicKey someBlsSecretKey
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    finalizers = FinalizationCommittee (Vec.fromList [fi 0, fi 1, fi 2, fi 3]) 4
    bakersAndFinalizers =
        BakersAndFinalizers
            (FullBakersEx Vec.empty 0)
            finalizers
            (computeFinalizationCommitteeHash finalizers)
    -- A skov data where
    -- - the current round and epoch is set to 1 (next payday is at epoch 2).
    -- - there are 3 finalizers 1, 2 and 3 each with a weight of 1 (total weight is 3).
    -- - one collected quorum message from the present finalizer index for round 1 and epoch 1.
    deadBlock = BlockHash $ Hash.hash "dead block"
    liveBlock = BlockHash $ Hash.hash "live block"
    finalizedBlock = BlockHash $ Hash.hash "finalized block"
    finalizedBlockPointer = Common.someBlockPointer sProtocolVersion finalizedBlock
    liveBlockPointer = Common.someBlockPointer sProtocolVersion liveBlock
    -- the round and epoch here is for making it easier to trigger the various cases
    -- with respect to the alive block (i.e. we set the focus block here).
    sd = sd' 1 1
    -- the round and epoch here is for making it easier to trigger the various cases
    -- with respect to the alive block (i.e. we set the focus block here).
    sd' r e =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round r
            & roundStatus . rsCurrentEpoch .~ e
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . nextPayday .~ 2
            & currentQuorumMessages %~ addQuorumMessage (VerifiedQuorumMessage duplicateMessage 1 1 $ Common.myBlockPointer sProtocolVersion (Round r) e)
            & blockTable . deadBlocks %~ insertDeadCache deadBlock
            & skovPendingTransactions . focusBlock .~ liveBlockPointer (Round r) e
            & lastFinalized .~ finalizedBlockPointer (Round 0) 1
    -- Run the 'receiveQuorumMessage' action.
    receiveAndCheck skovData qm expect = do
        resultCode <- runTestLLDB lldbWithGenesis $ receiveQuorumMessage qm skovData
        resultCode `shouldBe` expect

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $ do
            it "Adding a quorum message" $ propAddQuorumMessage spv
            testMakeQuorumCertificate spv
            testReceiveQuorumMessage spv
