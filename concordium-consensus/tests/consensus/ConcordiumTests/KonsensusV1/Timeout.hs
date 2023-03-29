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
import Concordium.Types
import Concordium.Types.Transactions

import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.TreeState.Implementation
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
    it "rejects from a non finalizer" $ receiveAndCheck sd notAFinalizerQCMessage $ Rejected ObsoleteQC
  where
    -- A message where the round is obsolete.
    obsoleteRoundMessage = mkTimeoutMessage $! TimeoutMessageBody (FinalizerIndex 0) (Round 0) 0 (someQC (Round 0) 0) (TimeoutSignature Bls.emptySignature)
    -- A message where the qc is obsolete.
    obsoleteQCMessage = mkTimeoutMessage $! TimeoutMessageBody (FinalizerIndex 0) (Round 1) 0 (someQC (Round 0) 0) (TimeoutSignature Bls.emptySignature)
    -- A message from a non finalizer
    notAFinalizerQCMessage = mkTimeoutMessage $! TimeoutMessageBody (FinalizerIndex 42) (Round 1) 0 (someQC (Round 0) 0) (TimeoutSignature Bls.emptySignature)
    -- Some quorum certificate for the specified round and epoch
    -- The quorum certificate is signed off by finalizer 1 and 2.
    someQC r e = QuorumCertificate someBlockHash r e qcSignature $ finalizerSet [FinalizerIndex 1, FinalizerIndex 2]
    -- Some unsigned quorum certificate message
    someQCMessage finalizerIndex = QuorumMessage (QuorumSignature Bls.emptySignature) someBlockHash (FinalizerIndex finalizerIndex) 1 1
    -- The signed quorum message from the provided finalizer
    someSignedQCMessage finalizerIndex = (someQCMessage finalizerIndex){qmSignature = someQCMessageSignature finalizerIndex}
    -- A quorum certificate message signature for some quorum certificate message.
    someQCMessageSignature finalizerIndex = signQuorumSignatureMessage (quorumSignatureMessageFor (someQCMessage finalizerIndex) genesisBlockHash) blsPrivateKey
    -- A private bls key shared by the finalizers in this test.
    blsPrivateKey = someBlsSecretKey
    -- A public bls key shared by the finalizers in this test.
    blsPublicKey = Bls.derivePublicKey blsPrivateKey
    -- A finalizer info with a weight of 1.
    finalizerInfo finalizerIndex = FinalizerInfo (FinalizerIndex finalizerIndex) 1 sigPublicKey
    -- The aggregate signature for the quorum certificate signed off by finalizer 1 and 2.
    qcSignature = someQCMessageSignature 1 <> someQCMessageSignature 1
    --- A VRF public key shared by the finalizers in this test.
    vrfPublicKey = VRF.publicKey someVRFKeyPair
    -- Keypair shared by the finalizers in this test.
    sigKeyPair = unsafePerformIO Sig.newKeyPair
    -- Public key shared by the finalizers in this test.
    sigPublicKey = Sig.verifyKey sigKeyPair
    -- make a timeout message by signing the provided body.
    mkTimeoutMessage body = signTimeoutMessage body genesisBlockHash sigKeyPair
    -- Some genesis block hash.
    genesisBlockHash = BlockHash $ Hash.hash "My genesis block"
    -- Some other block hash
    someBlockHash = BlockHash $ Hash.hash "Some other block"
    -- Some baked block for the provided round and epoch
    bakedBlock r e = BakedBlock r e 0 0 (someQC r e) Absent Absent dummyBlockNonce Vec.empty emptyTransactionOutcomesHashV1 (StateHashV0 $ Hash.hash "empty state hash")
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
    -- A tree state (sd') fixed to round 1 and epoch 0.
    sd = sd' 1 0
    -- FinalizerInfo for the finalizer index provided.
    -- All finalizers has the same keys attacched.
    fi :: Word32 -> FinalizerInfo
    fi fIdx = FinalizerInfo (FinalizerIndex fIdx) 1 sigPublicKey vrfPublicKey blsPublicKey (BakerId $ AccountIndex $ fromIntegral fIdx)
    -- Construct a set of 0 bakers and 3 finalisers with indecies 1,2,3.
    bakersAndFinalizers = BakersAndFinalizers (FullBakers Vec.empty 0) (FinalizationCommittee (Vec.fromList [fi 1, fi 2, fi 3]) 3)
    -- A tree state where the round and epoch is provided.
    -- It futher contains a set of finalizers for the current and previous epoch and
    -- a last finalized block for round 1.
    sd' r e =
        dummyInitialSkovData
            & roundStatus . rsCurrentRound .~ Round r
            & skovEpochBakers . currentEpoch .~ e
            & lastFinalized .~ someBlockPointer (Round 1) 0
            & skovEpochBakers . currentEpochBakers .~ bakersAndFinalizers
            & skovEpochBakers . previousEpochBakers .~ bakersAndFinalizers
    -- receive the timeout message in the provided tree state context and
    -- check that the result is as expected.
    receiveAndCheck skovData tm expect = do
        resultCode <- runTestLLDB (lldbWithGenesis @'P6) $ receiveTimeoutMessage tm skovData
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
