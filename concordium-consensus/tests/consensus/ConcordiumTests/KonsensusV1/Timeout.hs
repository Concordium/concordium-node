{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus.Timeout' module.
module ConcordiumTests.KonsensusV1.Timeout (tests) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Ratio
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.DummyData as Dummy
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Genesis.Data as GD
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache)
import Concordium.Startup
import Concordium.Types
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.Transactions

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.TestMonad
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import ConcordiumTests.KonsensusV1.TreeStateTest hiding (tests)
import ConcordiumTests.KonsensusV1.Types hiding (tests)

-- |Test that 'updateCurrentTimeout' correctly calculates a new timeout given the current timeout and the
-- `timeoutIncrease` parameter.
testUpdateCurrentTimeout :: Duration -> Ratio Word64 -> Duration -> Assertion
testUpdateCurrentTimeout baseTimeout timeoutIncrease expectedTimeout = do
    let actualTimeout = updateCurrentTimeout timeoutIncrease baseTimeout
    assertEqual "Timeout duration should be correct" expectedTimeout actualTimeout

-- |Test the 'receiveTimeoutMessage' function which partially verifies
-- a 'TimeoutMessage'.
testReceiveTimeoutMessage :: Spec
testReceiveTimeoutMessage = describe "Receive timeout message" $ do
    it "rejects obsolete round" $ receiveAndCheck sd obsoleteRoundMessage $ Rejected ObsoleteRound
    it "rejects obsolete qc" $ receiveAndCheck sd obsoleteQCMessage $ Rejected ObsoleteQC
    it "initializes catch-up upon future epoch" $ receiveAndCheck sd futureEpochTM CatchupRequired
    it "rejects from a non finalizer" $ receiveAndCheck sd notAFinalizerQCMessage $ Rejected NotAFinalizer
    it "rejects on an invalid signature" $ receiveAndCheck sd invalidSignatureMessage $ Rejected InvalidSignature
    it "initializes catch-up upon a future round" $ receiveAndCheck sd futureRoundTM CatchupRequired
    it "rejects when the qc points to an old finalized block" $ receiveAndCheck sd obsoleteQCPointer $ Rejected ObsoleteQCPointer
    it "initializes catch-up when the qc pointer is unknown" $ receiveAndCheck sd unknownQCPointer CatchupRequired
    it "rejects when the qc points to a dead block" $ receiveAndCheck sd qcPointerIsDead $ Rejected DeadQCPointer
    it "initializes catch-up when qc pointer is pending" $ receiveAndCheck sd qcPointerIsPending CatchupRequired
    it "returns duplicate upon a duplicate timeout message" $ receiveAndCheck sd duplicateMessage Duplicate
    it "rejects double signing" $ receiveAndCheck sd doubleSignMessage $ Rejected DoubleSigning
    it "rejects when the bls signature is invalid" $ receiveAndCheck sd invalidBLSSignatureMessage $ Rejected InvalidBLSSignature
    it "received a valid timeout message" $
        receiveAndCheck sd validTimeoutMessage $
            Received $
                MkPartiallyVerifiedTimeoutMessage validTimeoutMessage finalizers
  where
    -- A valid timeout message that should pass the initial verification.
    validTimeoutMessage = mkTimeoutMessage $! mkTimeoutMessageBody 2 2 0 $ someQC (Round 1) 0
    -- A timeout message with an invalid BLS signature.
    invalidBLSSignatureMessage = mkTimeoutMessage $! TimeoutMessageBody (FinalizerIndex 2) (Round 2) 0 (someQC (Round 1) 0) $ TimeoutSignature Bls.emptySignature
    -- A message that will be rejected as double signing as the finalizer already have @duplicateMessage@
    -- in the tree state.
    doubleSignMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ someQC (Round 124) 0
    -- A message that is intended to return @Duplicate@, hence it is also insert into the
    -- tree state before running the test.
    duplicateMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ someQC (Round 123) 0
    -- A message where the qc pointer is pending
    qcPointerIsPending = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 qcWithPendingPointer
    -- A message where the qc pointer is pointing to a dead block
    qcPointerIsDead = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 qcWithDeadPointer
    -- A message where the qc pointer is unknown
    unknownQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 qcWithUnknownPointer
    -- A message where the qc pointer is to a block prior to the last finalized block.
    obsoleteQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 someQCPointingToAndOldFinalizedBlock
    -- A message where the epoch is in the future
    futureEpochTM = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 1 $ someQC (Round 1) 0
    -- A message where the round is in the future but the finalizer is present in the epoch.
    futureRoundTM = mkTimeoutMessage $! mkTimeoutMessageBody 1 3 0 $ someQC (Round 1) 0
    -- A message where the signature is invalid
    invalidSignatureMessage = TimeoutMessage (mkTimeoutMessageBody 1 2 0 (someQC (Round 1) 0)) (Sig.Signature "invalid signature")
    -- A message where the round is obsolete.
    obsoleteRoundMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 1 0 $ someQC (Round 0) 0
    -- A message where the qc is obsolete.
    obsoleteQCMessage = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ someQC (Round 0) 0
    -- A message from a non finalizer (42)
    notAFinalizerQCMessage = mkTimeoutMessage $! mkTimeoutMessageBody 42 2 0 $ someQC (Round 1) 0
    -- Some quorum certificate for the specified round and epoch
    -- The quorum certificate is signed off by finalizer 1 and 2.
    someQC r e = QuorumCertificate someBlockHash r e qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to a pending block
    qcWithPendingPointer = QuorumCertificate pendingBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to a dead block
    qcWithDeadPointer = QuorumCertificate deadBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- a qc pointing to an unknown block
    qcWithUnknownPointer = QuorumCertificate (BlockHash $ Hash.hash "Some unknown block") 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- A qc pointing to an old finalized block.
    someQCPointingToAndOldFinalizedBlock = QuorumCertificate someOldFinalizedBlockHash 1 0 qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- Some unsigned quorum certificate message
    someQCMessage finalizerIndex = QuorumMessage (QuorumSignature Bls.emptySignature) someBlockHash (FinalizerIndex finalizerIndex) 1 1
    -- A quorum certificate message signature for some quorum certificate message.
    someQCMessageSignature finalizerIndex = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex) genesisBlockHash) $ blsPrivateKey finalizerIndex
    -- A private bls key shared by the finalizers in this test.
    blsPrivateKey fidx = fst $ Dummy.randomBlsSecretKey $ mkStdGen (fromIntegral fidx)
    -- A public bls key shared by the finalizers in this test.
    blsPublicKey fidx = Bls.derivePublicKey (blsPrivateKey fidx)
    -- The aggregate signature for the quorum certificate signed off by finalizer 1 and 2.
    qcSignature = someQCMessageSignature 1 <> someQCMessageSignature 1
    --- A VRF public key shared by the finalizers in this test.
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    -- Keypair shared by the finalizers in this test.
    sigKeyPair fidx = fst $ Dummy.randomBlockKeyPair $ mkStdGen $ fromIntegral fidx
    -- Public key shared by the finalizers in this test.
    sigPublicKey fidx = Sig.verifyKey $ sigKeyPair fidx
    -- make a timeout message body suitable for signing.
    mkTimeoutMessageBody fidx r e qc = TimeoutMessageBody (FinalizerIndex fidx) (Round r) e qc $ validTimeoutSignature fidx r (qcRound qc) (qcEpoch qc)
    -- Create a valid timeout signature.
    validTimeoutSignature fidx r qr qe = signTimeoutSignatureMessage (TimeoutSignatureMessage genesisBlockHash (Round r) qr qe) (blsPrivateKey fidx)
    -- sign and create a timeout message.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlockHash $ sigKeyPair (fromIntegral $ theFinalizerIndex $ tmFinalizerIndex body)
    -- A block hash for a block marked as pending.
    pendingBlockHash = BlockHash $ Hash.hash "A pending block"
    -- A block hash for a block marked as dead.
    deadBlockHash = BlockHash $ Hash.hash "A dead block"
    -- Some genesis block hash.
    genesisBlockHash = BlockHash $ Hash.hash "My genesis block"
    -- Some other block hash
    someBlockHash = BlockHash $ Hash.hash "Some other block"
    -- A hash for an old finalized block.
    someOldFinalizedBlockHash = BlockHash $ Hash.hash "Some old finalized block"
    -- Some baked block for the provided round and epoch
    bakedBlock r e = BakedBlock r e 0 0 (someQC r e) Absent Absent dummyBlockNonce Vec.empty emptyTransactionOutcomesHashV1 (StateHashV0 $ Hash.hash "empty state hash")
    -- Some pending block with no meaningful content.
    -- It is inserted into the tree state before running tests.
    pendingBlock = MemBlockPending $ PendingBlock signedBlock $ timestampToUTCTime 0
    -- A block signed by finalizer 1.
    signedBlock = SignedBlock (bakedBlock 4 0) pendingBlockHash $ Sig.sign (sigKeyPair 1) "foo"
    -- Some dummy block pointer for the provided round and epoch
    someBlockPointer r e =
        BlockPointer
            { bpInfo =
                BlockMetadata
                    { bmHeight = 0,
                      bmReceiveTime = timestampToUTCTime 0,
                      bmArriveTime = timestampToUTCTime 0
                    },
              bpBlock = NormalBlock $ SignedBlock (bakedBlock r e) someBlockHash (Sig.sign (sigKeyPair 1) "foo"),
              bpState = dummyBlockState
            }
    -- FinalizerInfo for the finalizer index provided.
    -- All finalizers has the same keys attacched.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 (sigPublicKey fIdx) vrfPublicKey (blsPublicKey fIdx) (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- COnstruct the finalization committee
    finalizers = FinalizationCommittee (Vec.fromList [fi 0, fi 1, fi 2]) 3
    -- Construct a set of 0 bakers and 3 finalisers with indecies 1,2,3.
    bakersAndFinalizers = BakersAndFinalizers (FullBakers Vec.empty 0) finalizers
    -- A tree state where the following applies:
    -- - Current round is 2
    -- - Current epoch is 0
    -- - There is a finalization committee consisting of the finalizers with indecies [1,2,3]
    -- - There is a last finalized block for round 1, epoch 0.
    sd =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round 2
            & skovEpochBakers . currentEpoch .~ 0
            & genesisMetadata %~ (\existingGenesis -> existingGenesis{gmFirstGenesisHash = genesisBlockHash})
            & lastFinalized .~ someBlockPointer (Round 1) 0
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
            & blockTable . deadBlocks %~ insertDeadCache deadBlockHash
            & blockTable . liveMap %~ HM.insert pendingBlockHash pendingBlock
            & receivedTimeoutMessages .~ Present (TimeoutMessages 0 (Map.singleton (FinalizerIndex 1) duplicateMessage) Map.empty)
    -- A low level database which consists of a finalized block for height 0 otherwise empty.
    lldb =
        let myLLDB = lldbWithGenesis @'P6
        in  myLLDB{lldbBlockHashes = HM.singleton someOldFinalizedBlockHash $ BlockHeight 0}
    -- receive the timeout message in the provided tree state context and
    -- check that the result is as expected.
    receiveAndCheck skovData tm expect = do
        resultCode <- runTestLLDB lldb $ receiveTimeoutMessage tm skovData
        resultCode `shouldBe` expect

testExecuteTimeoutMessages :: Spec
testExecuteTimeoutMessages = describe "execute timeout messages" $ do
    it "rejects message with invalid qc signature (qc round is better than recorded highest qc)" $ execute invalidQCTimeoutMessage $ InvalidQC $ someInvalidQC 1 0
    it "accepts message where qc is ok (qc round is better than recorded highest qc)" $ execute validQCTimeoutMessage ExecutionSuccess
  where
    --    it "rejects message with invalid qc signature (qc round is already checked, but another epoch)" $ execute invalidEpochQCTimeoutMessage $ InvalidQCEpoch 0 (someQC 0 1)

    -- action that runs @executeTimeoutMessage@ on the provided
    -- timeout message and checks that it matches the expectation.
    execute timeoutMessage expect = runTestMonad @'P6 noBaker time genesisData $ do
        resultCode <- executeTimeoutMessage timeoutMessage
        liftIO $ expect @=? resultCode
    -- the finalizer with the provided finalizer index.
    -- Note that all finalizers use the same keys.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey (VRF.publicKey someVRFKeyPair) (Bls.derivePublicKey $ blsSk fIdx) (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- a valid timeout message with a valid qc.
    validQCTimeoutMessage = MkPartiallyVerifiedTimeoutMessage validTimeoutMessage finalizers
    -- a timeout message where the qc signature does not check out.
    invalidQCTimeoutMessage = MkPartiallyVerifiedTimeoutMessage (mkTimeoutMessage $! mkTimeoutMessageBody 2 1 0 (someInvalidQC 1 0)) finalizers
    -- the finalization committee.
    finalizers = FinalizationCommittee (Vec.fromList [fi 0, fi 1, fi 2]) 3
    -- the time that we run our test computation with respect to.
    time = timestampToUTCTime 1
    -- The @executeTimeoutMessage@ runs in a no baker context.
    noBaker = BakerContext Nothing
    -- the genesis block hash
    genesisBlockHash = GD.genesisBlockHash genesisData
    -- a timeout message body by the finalizer, round epoch and qc provided.
    mkTimeoutMessageBody fidx r e qc = TimeoutMessageBody (FinalizerIndex fidx) (Round r) e qc $ validTimeoutSignature fidx r (qcRound qc) (qcEpoch qc)
    -- a valid timeout signature.
    validTimeoutSignature fidx r qr qe = signTimeoutSignatureMessage (TimeoutSignatureMessage genesisBlockHash (Round r) qr qe) $ blsSk fidx
    -- A bls key for the finalizer.
    blsSk fidx = fst <$> Dummy.randomBlsSecretKey $ mkStdGen $ fromIntegral fidx
    -- a valid timeout message.
    validTimeoutMessage = mkTimeoutMessage $! mkTimeoutMessageBody 0 2 0 $ someQC (Round 1) 0
    -- some valid qc message by the provided finalizer.
    someQCMessage finalizerIndex = QuorumMessage (QuorumSignature Bls.emptySignature) someBlockHash (FinalizerIndex finalizerIndex) 1 0
    -- some valid qc message signature signed by the provide
    someQCMessageSignature finalizerIndex = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex) genesisBlockHash) (blsSk finalizerIndex)
    -- just a block hash for testing purposes.
    someBlockHash = BlockHash $ Hash.hash "a block hash"
    -- a valid signature.
    qcSignature = someQCMessageSignature 0 <> someQCMessageSignature 1 <> someQCMessageSignature 2
    -- a qc with a valid signature created by finalizers 0,1,2
    someQC r e = QuorumCertificate someBlockHash r e qcSignature $ finalizerSet [FinalizerIndex 0, FinalizerIndex 1, FinalizerIndex 2]
    -- a qc with the empty signature.
    someInvalidQC r e = QuorumCertificate someBlockHash r e (QuorumSignature Bls.emptySignature) $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2, FinalizerIndex 3]
    -- a signed timeout message with the provided body.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlockHash sigKeyPair
    -- the sig key pair that is used for all entities in this test.
    sigKeyPair = fst $ Dummy.randomBlockKeyPair $ mkStdGen 42
    -- the public key corresponding to the above sigKeyPair.
    sigPublicKey = Sig.verifyKey sigKeyPair
    -- the foundation account for the genesis data.
    foundationAcct =
        Dummy.createCustomAccount
            1_000_000_000_000
            (Dummy.deterministicKP 0)
            (Dummy.accountAddressFrom 0)
    -- the genesis data for composing the monad that the computations are run within.
    -- it consists of 3 finalizers with indecies 0,1,2
    (genesisData, _, _) =
        makeGenesisDataV1 @'P6
            (Timestamp 0)
            3
            3_600_000
            Dummy.dummyCryptographicParameters
            Dummy.dummyIdentityProviders
            Dummy.dummyArs
            [ foundationAcct
            ]
            Dummy.dummyKeyCollection
            Dummy.dummyChainParameters

tests :: Spec
tests = describe "KonsensusV1.Timeout" $ do
    testReceiveTimeoutMessage
    testExecuteTimeoutMessages
    it "Test updateCurrentTimeout" $ do
        testUpdateCurrentTimeout 10_000 (3 % 2) 15_000
        testUpdateCurrentTimeout 10_000 (4 % 3) 13_333
        testUpdateCurrentTimeout 10_000 (5 % 3) 16_666
        testUpdateCurrentTimeout 3_000 (4 % 3) 4_000
        testUpdateCurrentTimeout 80_000 (10 % 9) 88_888
        testUpdateCurrentTimeout 8_000 (8 % 7) 9_142
