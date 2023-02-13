module ConcordiumTests.KonsensusV1.Types where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Test.Hspec
import Test.QuickCheck

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import qualified Data.FixedByteString as FBS

import Concordium.KonsensusV1.Types

-- |Generate a 'FinalizerSet'. The size parameter determines the size of the committee that
-- the finalizers are (nominally) sampled from.
genFinalizerSet :: Gen FinalizerSet
genFinalizerSet = sized $ \s -> FinalizerSet . fromInteger <$> chooseInteger (0, 2 ^ s)

-- |An arbitrarily-chosen 'Bls.SecretKey'.
someBlsSecretKey :: Bls.SecretKey
someBlsSecretKey = generateBlsSecretKeyFromSeed 123456

-- |Generate a 'Bls.Signature' by signing an arbitrary 10-byte string with the 'someBlsSecretKey'.
-- This should generate a representative sample of signatures for serialization purposes.
genBlsSignature :: Gen Bls.Signature
genBlsSignature = flip Bls.sign someBlsSecretKey . BS.pack <$> vector 10

-- |Generate a quorum signature.
genQuorumSignature :: Gen QuorumSignature
genQuorumSignature = QuorumSignature <$> genBlsSignature

-- |Generate a quorum certificate in a way that is suitable for testing serialization.
genQuorumCertificate :: Gen QuorumCertificate
genQuorumCertificate = do
    qcBlock <- BlockHash . Hash.Hash . FBS.pack <$> vector 32
    qcRound <- Round <$> arbitrary
    qcEpoch <- arbitrary
    qcAggregateSignature <- genQuorumSignature
    qcSignatories <- genFinalizerSet
    return QuorumCertificate{..}

-- |Generate a 'FinalizationEntry' suitable for testing serialization.
-- The result satisfies the invariants.
genFinalizationEntry :: Gen FinalizationEntry
genFinalizationEntry = do
    feFinalizedQuorumCertificate <- genQuorumCertificate
    preQC <- genQuorumCertificate
    feSuccessorProof <- BlockQuasiHash . Hash.Hash . FBS.pack <$> vector 32
    let succRound = qcRound feFinalizedQuorumCertificate + 1
    let feSuccessorQuorumCertificate =
            preQC
                { qcRound = succRound,
                  qcBlock = successorBlockHash (BlockHeader succRound (qcEpoch preQC) (qcBlock feFinalizedQuorumCertificate)) feSuccessorProof
                }
    return FinalizationEntry{..}

-- |Generate a 'FinalizerRounds' map. The number of entries is governed by the size parameter.
-- This satisfies the size invariant, but does guarantee that rounds have different sets of
-- finalizers.
genFinalizerRounds :: Gen FinalizerRounds
genFinalizerRounds =
    FinalizerRounds . Map.fromList
        <$> scale (min (fromIntegral (maxBound :: Word32))) (listOf genRoundFS)
  where
    genRoundFS = do
        r <- Round <$> arbitrary
        fs <- genFinalizerSet
        return (r, fs)

-- |Generate an arbitrary 'Round' and 'Epoch' tuple.
genRoundAndEpoch :: Maybe Epoch -> Gen (Round, Epoch)
genRoundAndEpoch me = do
    r <- Round <$> arbitrary
    case me of
        Nothing -> arbitrary >>= \e -> return (r, e)
        Just e -> return (r, e)

-- |Generate a timeout certificate.
-- Use the epoch if it is provided otherwise create an
-- arbitrary epoch.
genTimeoutCertificate :: Maybe Epoch -> Gen TimeoutCertificate
genTimeoutCertificate me = do
    (tcRound, tcEpoch) <- genRoundAndEpoch me
    tcFinalizerQCRounds <- genFinalizerRounds
    tcAggregateSignature <- TimeoutSignature <$> genBlsSignature
    return TimeoutCertificate{..}

-- |Generate a timeout message body.
genTimeoutMessageBody :: Gen TimeoutMessageBody
genTimeoutMessageBody = do
    tmFinalizerIndex <- FinalizerIndex <$> arbitrary
    tmQuorumCertificate <- genQuorumCertificate
    (tmRound, tmEpoch, tmTimeoutCertificate) <-
        oneof
            [ (return (qcRound tmQuorumCertificate + 1, qcEpoch tmQuorumCertificate, Absent)),
              ( do
                    r <- chooseBoundedIntegral (qcRound tmQuorumCertificate, maxBound - 1)
                    tc <- genTimeoutCertificate $ Just $! qcEpoch tmQuorumCertificate
                    return (r + 1, tcEpoch tc, Present tc{tcRound = r})
              )
            ]
    tmEpochFinalizationEntry <- oneof [return Absent, Present <$> genFinalizationEntry]
    tmAggregateSignature <- TimeoutSignature <$> genBlsSignature
    return TimeoutMessageBody{..}

-- |Generate a 'TimeoutMessage' signed by an arbitrarily-generated keypair.
genTimeoutMessage :: Gen TimeoutMessage
genTimeoutMessage = do
    body <- genTimeoutMessageBody
    kp <- genBlockKeyPair
    return $ signTimeoutMessage body kp

-- |Check that serialization followed by deserialization gives the identity.
serCheck :: (Eq a, Serialize a, Show a) => a -> Property
serCheck a = decode (encode a) === Right a

-- |Test that serializing then deserializing a finalizer set is the identity.
propSerializeFinalizerSet :: Property
propSerializeFinalizerSet = forAll genFinalizerSet serCheck

-- |Test that serializing then deserializing a quorum certificate is the identity.
propSerializeQuorumCertificate :: Property
propSerializeQuorumCertificate = forAll genQuorumCertificate serCheck

-- |Test that serializing then deserializing a finalization entry is the identity.
propSerializeFinalizationEntry :: Property
propSerializeFinalizationEntry = forAll genFinalizationEntry serCheck

-- |Test that serializing then deserializing a timeout certificate is the identity.
propSerializeTimeoutCertificate :: Property
propSerializeTimeoutCertificate = forAll (genTimeoutCertificate Nothing) serCheck

-- |Test that serializing then deserializing a timeout message body is the identity.
propSerializeTimeoutMessageBody :: Property
propSerializeTimeoutMessageBody = forAll genTimeoutMessageBody serCheck

-- |Test that serializing then deserializing a timeout message is the identity.
propSerializeTimeoutMessage :: Property
propSerializeTimeoutMessage = forAll genTimeoutMessage serCheck

-- |Check that a signing a timeout message produces a timeout message that verifies with the key.
propSignTimeoutMessagePositive :: Property
propSignTimeoutMessagePositive =
    forAll genTimeoutMessageBody $ \body ->
        forAll genBlockKeyPair $ \kp ->
            checkTimeoutMessageSignature (Sig.verifyKey kp) (signTimeoutMessage body kp)

-- |Check that a signing a timeout message produces a timeout message that does not verify with a
-- different key.
propSignTimeoutMessageDiffKey :: Property
propSignTimeoutMessageDiffKey =
    forAll genTimeoutMessageBody $ \body ->
        forAll genBlockKeyPair $ \kp1 ->
            forAll genBlockKeyPair $ \kp2 ->
                (kp1 /= kp2) ==>
                    not (checkTimeoutMessageSignature (Sig.verifyKey kp2) (signTimeoutMessage body kp1))

-- |Check that signing a timeout message and changing the body to something different produces a
-- timeout message that does not verify with the key.
propSignTimeoutMessageDiffBody :: Property
propSignTimeoutMessageDiffBody =
    forAll genTimeoutMessageBody $ \body1 ->
        forAll genTimeoutMessageBody $ \body2 ->
            (body1 /= body2) ==>
                forAll genBlockKeyPair $
                    \kp ->
                        not (checkTimeoutMessageSignature (Sig.verifyKey kp) (signTimeoutMessage body1 kp){tmBody = body2})

tests :: Spec
tests = describe "KonesnsusV2.Types" $ do
    it "FinalizerSet serialization" propSerializeFinalizerSet
    it "QuorumCertificate serialization" propSerializeQuorumCertificate
    it "FinalizationEntry serialization" propSerializeFinalizationEntry
    it "TimeoutCertificate serialization" propSerializeTimeoutCertificate
    it "TimeoutMessageBody serialization" propSerializeTimeoutMessageBody
    it "TimeoutMessage serialization" propSerializeTimeoutMessage
    it "TimeoutMessage signature check positive" propSignTimeoutMessagePositive
    it "TimeoutMessage signature check fails with different key" propSignTimeoutMessageDiffKey
    it "TimeoutMessage signature check fails with different body" propSignTimeoutMessageDiffBody
