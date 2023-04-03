{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus.Timeout' module.
module ConcordiumTests.KonsensusV1.Timeout (tests) where

import Data.Ratio
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform
import System.IO.Unsafe
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.TreeState (insertDeadCache)
import Concordium.Types
import Concordium.Types.Transactions
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map

import Concordium.KonsensusV1.Consensus.Timeout
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
    it "initializes catch-up upon future epoch" $ receiveAndCheck sd futureEpochTM $ CatchupRequired
    it "rejects from a non finalizer" $ receiveAndCheck sd notAFinalizerQCMessage $ Rejected NotAFinalizer
    it "rejects on an invalid signature" $ receiveAndCheck sd invalidSignatureMessage $ Rejected InvalidSignature
    it "initializes catch-up upon a future round" $ receiveAndCheck sd futureRoundTM $ CatchupRequired
    it "rejects when the qc points to an old finalized block" $ receiveAndCheck sd obsoleteQCPointer $ Rejected ObsoleteQCPointer
    it "initializes catch-up when the qc pointer is unknown" $ receiveAndCheck sd unknownQCPointer $ CatchupRequired
    it "rejects when the qc points to a dead block" $ receiveAndCheck sd qcPointerIsDead $ Rejected DeadQCPointer
    it "initializes catch-up when qc pointer is pending" $ receiveAndCheck sd qcPointerIsPending $ CatchupRequired
    it "returns duplicate upon a duplicate timeout message" $ receiveAndCheck sd duplicateMessage $ Duplicate
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
    qcPointerIsPending = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ qcWithPendingPointer
    -- A message where the qc pointer is pointing to a dead block
    qcPointerIsDead = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ qcWithDeadPointer
    -- A message where the qc pointer is unknown
    unknownQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ qcWithUnknownPointer
    -- A message where the qc pointer is to a block prior to the last finalized block.
    obsoleteQCPointer = mkTimeoutMessage $! mkTimeoutMessageBody 1 2 0 $ someQCPointingToAndOldFinalizedBlock
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
    someQCMessageSignature finalizerIndex = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex) genesisBlockHash) blsPrivateKey
    -- A private bls key shared by the finalizers in this test.
    blsPrivateKey = someBlsSecretKey
    -- A public bls key shared by the finalizers in this test.
    blsPublicKey = Bls.derivePublicKey blsPrivateKey
    -- The aggregate signature for the quorum certificate signed off by finalizer 1 and 2.
    qcSignature = someQCMessageSignature 1 <> someQCMessageSignature 1
    --- A VRF public key shared by the finalizers in this test.
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    -- Keypair shared by the finalizers in this test.
    sigKeyPair = unsafePerformIO Sig.newKeyPair
    -- Public key shared by the finalizers in this test.
    sigPublicKey = Sig.verifyKey sigKeyPair
    -- make a timeout message body suitable for signing.
    mkTimeoutMessageBody fidx r e qc = TimeoutMessageBody (FinalizerIndex fidx) (Round r) e qc $ validTimeoutSignature r (qcRound qc) (qcEpoch qc)
    -- Create a valid timeout signature.
    validTimeoutSignature r qr qe = signTimeoutSignatureMessage (TimeoutSignatureMessage genesisBlockHash (Round r) qr qe) blsPrivateKey
    -- sign and create a timeout message.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlockHash sigKeyPair
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
    -- A signed block.
    signedBlock = SignedBlock (bakedBlock 4 0) pendingBlockHash $ Sig.sign (unsafePerformIO Sig.newKeyPair) "foo"
    -- Some dummy block pointer for the provided round and epoch
    someBlockPointer r e =
        BlockPointer
            { bpInfo =
                BlockMetadata
                    { bmHeight = 0,
                      bmReceiveTime = timestampToUTCTime 0,
                      bmArriveTime = timestampToUTCTime 0
                    },
              bpBlock = NormalBlock $ SignedBlock (bakedBlock r e) someBlockHash (Sig.sign (unsafePerformIO Sig.newKeyPair) "foo"),
              bpState = dummyBlockState
            }
    -- FinalizerInfo for the finalizer index provided.
    -- All finalizers has the same keys attacched.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- COnstruct the finalization committee
    finalizers = FinalizationCommittee (Vec.fromList [fi 1, fi 2, fi 3]) 3
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

tests :: Spec
tests = describe "KonsensusV1.Timeout" $ do
    testReceiveTimeoutMessage
    it "Test updateCurrentTimeout" $ do
        testUpdateCurrentTimeout 10000 (3 % 2) 15000
        testUpdateCurrentTimeout 10000 (4 % 3) 13333
        testUpdateCurrentTimeout 10000 (5 % 3) 16666
        testUpdateCurrentTimeout 3000 (4 % 3) 4000
        testUpdateCurrentTimeout 80000 (10 % 9) 88888
        testUpdateCurrentTimeout 8000 (8 % 7) 9142
