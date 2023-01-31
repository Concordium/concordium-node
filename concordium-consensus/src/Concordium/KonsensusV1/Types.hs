{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.KonsensusV1.Types where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word
import Numeric.Natural

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types
import Concordium.Utils.Serialization
import Control.Monad

-- |A round number for consensus.
newtype Round = Round {theRound :: Word64}
    deriving (Eq, Ord, Show, Serialize, Num, Integral, Real, Enum, Bounded)

-- |The message that is signed by a finalizer to certify a block.
data QuorumSignatureMessage = QuorumSignatureMessage
    { -- |Hash of the genesis block.
      qsmGenesis :: !BlockHash,
      -- |Hash of the block being signed.
      qsmBlock :: !BlockHash
    }
    deriving (Eq, Show)

-- |Compute the byte representation of a 'QuorumSignatureMessage' that is actually signed.
-- TODO: check that this cannot collide with other signed messages.
quorumSignatureMessageBytes :: QuorumSignatureMessage -> BS.ByteString
quorumSignatureMessageBytes QuorumSignatureMessage{..} = runPut $ do
    putByteString "QUORUM."
    put qsmGenesis
    put qsmBlock

-- |Signature by a finalizer on a 'QuorumSignatureMessage', or an aggregation of signatures on a
-- common message.
newtype QuorumSignature = QuorumSignature {theQuorumSignature :: Bls.Signature}
    deriving (Eq, Ord, Show, Serialize, Semigroup, Monoid)

-- |Sign a 'QuorumSignatureMessage' with a baker's private key.
signQuorumSignatureMessage :: QuorumSignatureMessage -> BakerAggregationPrivateKey -> QuorumSignature
signQuorumSignatureMessage msg privKey =
    QuorumSignature $ Bls.sign (quorumSignatureMessageBytes msg) privKey

-- |Check the signature on a 'QuorumSignatureMessage' that is signed by a single party.
checkQuorumSignatureSingle ::
    QuorumSignatureMessage -> BakerAggregationVerifyKey -> QuorumSignature -> Bool
checkQuorumSignatureSingle msg pubKey =
    Bls.verify (quorumSignatureMessageBytes msg) pubKey
        . theQuorumSignature

-- |Check the signature on a 'QuorumSignatureMessage' that is signed by multiple parties.
checkQuorumSignature ::
    QuorumSignatureMessage -> [BakerAggregationVerifyKey] -> QuorumSignature -> Bool
checkQuorumSignature msg pubKeys =
    Bls.verifyAggregate (quorumSignatureMessageBytes msg) pubKeys
        . theQuorumSignature

-- |Index of a finalizer in a finalization committee.
newtype FinalizerIndex = FinalizerIndex {theFinalizerIndex :: Word32}
    deriving (Eq, Ord, Show, Enum, Bounded, Serialize)

-- |A set of 'FinalizerIndex'es.
-- This is represented as a bit vector, where the bit @i@ is set iff the finalizer index @i@ is
-- in the set.
newtype FinalizerSet = FinalizerSet {theFinalizerSet :: Natural}
    deriving (Eq)

instance Serialize FinalizerSet where
    put fs = do
        let (byteCount, putBytes) = unroll 0 (return ()) (theFinalizerSet fs)
        putWord32be byteCount
        putBytes
      where
        unroll bc cont 0 = (bc, cont)
        unroll bc cont n = unroll (bc + 1) (putWord8 (fromIntegral n) >> cont) (shiftR n 8)
    get = do
        byteCount <- getWord32be
        FinalizerSet <$> roll byteCount 0
      where
        roll 0 n = return n
        roll bc n = do
            b <- getWord8
            roll (bc - 1) (shiftL n 8 .|. fromIntegral b)

-- |Convert a 'FinalizerSet' to a list of 'FinalizerIndex'.
finalizerList :: FinalizerSet -> [FinalizerIndex]
finalizerList = unroll 0 . theFinalizerSet
  where
    unroll _ 0 = []
    unroll i x
        | testBit x 0 = FinalizerIndex i : r
        | otherwise = r
      where
        r = unroll (i + 1) (shiftR x 1)

data QuorumCertificate = QuorumCertificate
    { qcBlock :: !BlockHash,
      qcRound :: !Round,
      qcEpoch :: !Epoch,
      qcAggregateSignature :: !QuorumSignature,
      qcSignatories :: !FinalizerSet
    }

instance Serialize QuorumCertificate where
    put QuorumCertificate{..} = do
        put qcBlock
        put qcRound
        put qcEpoch
        put qcAggregateSignature
        put qcSignatories
    get = do
        qcBlock <- get
        qcRound <- get
        qcEpoch <- get
        qcAggregateSignature <- get
        qcSignatories <- get
        return QuorumCertificate{..}

-- |Check the signature in a quorum certificate.
checkQuorumCertificateSignature ::
    -- |Genesis block hash
    BlockHash ->
    (FinalizerSet -> [BakerAggregationVerifyKey]) ->
    QuorumCertificate ->
    Bool
checkQuorumCertificateSignature qsmGenesis toKeys QuorumCertificate{..} =
    checkQuorumSignature qsm (toKeys qcSignatories) qcAggregateSignature
  where
    qsm = QuorumSignatureMessage{qsmGenesis = qsmGenesis, qsmBlock = qcBlock}

-- |A Merkle proof that one block is the successor of another.
-- TODO: Define.
data SuccessorProof = SuccessorProof

instance Serialize SuccessorProof where
    put _ = return ()
    get = return SuccessorProof

-- |Compute the 'BlockHash' of a block that is the successor of another block
-- TODO: Define.
successorBlockHash ::
    -- |Block round
    Round ->
    -- |Block epoch
    Epoch ->
    -- |Predecessor block
    BlockHash ->
    SuccessorProof ->
    BlockHash
successorBlockHash = undefined

-- |A finalization entry that witnesses that a block has been finalized with quorum certificates
-- for two consecutive rounds. The finalization entry includes a proof that the blocks are in
-- consecutive rounds so that the entry can be validated without the second block.
--
-- The following invariants hold:
--
-- - @qcRound feSuccessorQuorumCertificate == qcRound feFinalizedQuorumCertificate + 1@
-- - @qcBlock feSuccessorQuorumCertificate == successorBlockHash (qcRound feSuccessorQuorumCertificate) (qcEpoch feSuccessorQuorumCertificate) (qcBlock feFinalizedQuorumCertificate) feSuccessorProof@
data FinalizationEntry = FinalizationEntry
    { feFinalizedQuorumCertificate :: !QuorumCertificate,
      feSuccessorQuorumCertificate :: !QuorumCertificate,
      feSuccessorProof :: !SuccessorProof
    }

instance Serialize FinalizationEntry where
    put FinalizationEntry{..} = do
        put feFinalizedQuorumCertificate
        let QuorumCertificate{..} = feSuccessorQuorumCertificate
        put qcEpoch
        put qcAggregateSignature
        put qcSignatories
        put feSuccessorProof
    get = do
        feFinalizedQuorumCertificate <- get
        qcEpoch <- get
        qcAggregateSignature <- get
        qcSignatories <- get
        feSuccessorProof <- get
        let sqcRound = qcRound feFinalizedQuorumCertificate + 1
        let feSuccessorQuorumCertificate =
                QuorumCertificate
                    { qcRound = sqcRound,
                      qcBlock =
                        successorBlockHash
                            sqcRound
                            qcEpoch
                            (qcBlock feFinalizedQuorumCertificate)
                            feSuccessorProof,
                      ..
                    }
        return FinalizationEntry{..}

data TimeoutSignatureMessage = TimeoutSignatureMessage
    { -- |Hash of the genesisBlock
      tsmGenesis :: !BlockHash,
      -- |Round number of the timed-out round
      tsmRound :: !Round,
      -- |Round number of the highest known valid quorum certificate
      tsmQCRound :: !Round
    }

-- |Compute the byte representation of a 'TimeoutSignatureMessage' that is actually signed.
-- TODO: check that this cannot collide with other signed messages.
timeoutSignatureMessageBytes :: TimeoutSignatureMessage -> BS.ByteString
timeoutSignatureMessageBytes TimeoutSignatureMessage{..} = runPut $ do
    putByteString "TIMEOUT."
    put tsmGenesis
    put tsmRound
    put tsmQCRound

-- |Signature by a finalizer on a 'TimeoutSignatureMessage', or an aggregation of signatures on a
-- common message.
newtype TimeoutSignature = TimeoutSignature {theTimeoutSignature :: Bls.Signature}
    deriving (Eq, Ord, Show, Serialize, Semigroup, Monoid)

-- |Sign a 'TimeoutSignatureMessage' with a Baker's private key.
signTimeoutSignatureMessage :: TimeoutSignatureMessage -> BakerAggregationPrivateKey -> TimeoutSignature
signTimeoutSignatureMessage msg privKey =
    TimeoutSignature $ Bls.sign (timeoutSignatureMessageBytes msg) privKey

-- |Check the signature on a 'TimeoutSignatureMessage' that is signed by a single party.
checkTimeoutSignatureSingle ::
    TimeoutSignatureMessage -> BakerAggregationVerifyKey -> TimeoutSignature -> Bool
checkTimeoutSignatureSingle msg pubKey =
    Bls.verify (timeoutSignatureMessageBytes msg) pubKey
        . theTimeoutSignature

-- |Data structure recording which finalizers have quorum certificates for which rounds.
--
-- Invariant: @Map.size theFinalizerRounds <= fromIntegral (maxBound :: Word32)@.
-- (This is trivially satisfied if each finalizer index occurs for at most one round.)
newtype FinalizerRounds = FinalizerRounds {theFinalizerRounds :: Map.Map Round FinalizerSet}

instance Serialize FinalizerRounds where
    put (FinalizerRounds fr) = do
        putWord32be $ fromIntegral $ Map.size fr
        putSafeSizedMapOf put put fr
    get = do
        count <- getWord32be
        FinalizerRounds <$> getSafeSizedMapOf count get get

finalizerRoundsList :: FinalizerRounds -> [(Round, FinalizerSet)]
finalizerRoundsList = Map.toAscList . theFinalizerRounds

data TimeoutCertificate = TimeoutCertificate
    { tcRound :: !Round,
      tcFinalizerQCRounds :: !FinalizerRounds,
      tcAggregateSignature :: !TimeoutSignature
    }

instance Serialize TimeoutCertificate where
    put TimeoutCertificate{..} = do
        put tcRound
        put tcFinalizerQCRounds
        put tcAggregateSignature
    get = do
        tcRound <- get
        tcFinalizerQCRounds <- get
        tcAggregateSignature <- get
        return TimeoutCertificate{..}

-- |Check the signature in a timeout certificate
checkTimeoutCertificateSignature ::
    -- |Genesis block hash
    BlockHash ->
    (FinalizerSet -> [BakerAggregationVerifyKey]) ->
    TimeoutCertificate ->
    Bool
checkTimeoutCertificateSignature tsmGenesis toKeys TimeoutCertificate{..} =
    verifyAggregateHybrid msgsKeys tcAggregateSignature
  where
    verifyAggregateHybrid = undefined -- TODO: Use implementation
    msgsKeys =
        [ (TimeoutSignatureMessage{tsmRound = tcRound, ..}, toKeys fs)
          | (tsmQCRound, fs) <- finalizerRoundsList tcFinalizerQCRounds
        ]

-- |The body of a timeout message.
--
-- The following invariants apply:
--
-- * @qcRound tmQuorumCertificate < tmRound@
-- * if @qcRound tmQuorumCertificate + 1 /= tmRound@ then @isJust tmTimeoutCertificate@
-- * if @tmTimeoutCertificate == Just tc@ then @tcRound tc + 1 == tmRound@
data TimeoutMessageBody = TimeoutMessageBody
    { -- |Index of the finalizer sending the timeout message.
      tmFinalizerIndex :: !FinalizerIndex,
      -- |Round number of the round being timed-out.
      tmRound :: !Round,
      -- |Highest quorum certificate known to the sender at the time of timeout.
      tmQuorumCertificate :: !QuorumCertificate,
      -- |A timeout certificate if the previous round timed out, or 'Nothing' otherwise.
      tmTimeoutCertificate :: !(Maybe TimeoutCertificate),
      -- |A epoch finalization entry for the epoch @qcEpoch tmQuorumCertificate@, if one is known.
      tmEpochFinalizationEntry :: !(Maybe FinalizationEntry),
      -- |A 'TimeoutSignature' from the sender for this round.
      tmAggregateSignature :: !TimeoutSignature
    }

instance Serialize TimeoutMessageBody where
    put TimeoutMessageBody{..} = do
        putWord8 tmFlags
        put tmFinalizerIndex
        put tmQuorumCertificate
        putTC
        putEFE
        put tmAggregateSignature
      where
        (tcFlag, putTC) = case tmTimeoutCertificate of
            Nothing -> (0, return ())
            Just tc -> (bit 0, put tc)
        (efeFlag, putEFE) = case tmEpochFinalizationEntry of
            Nothing -> (0, return ())
            Just efe -> (bit 1, put efe)
        tmFlags = tcFlag .|. efeFlag
    get = label "TimeoutMessageBody" $ do
        tmFlags <- getWord8
        unless (tmFlags .&. 0b11 == tmFlags) $
            fail $
                "deserialization encountered invalid flag byte (" ++ show tmFlags ++ ")"
        tmFinalizerIndex <- get
        tmQuorumCertificate <- get
        (tmRound, tmTimeoutCertificate) <-
            if testBit tmFlags 0
                then do
                    tc <- get
                    unless (qcRound tmQuorumCertificate <= tcRound tc) $
                        fail $
                            "failed check: quorum certificate round (" ++ show (qcRound tmQuorumCertificate) ++ ") <= timeout certificate round (" ++ show (tcRound tc) ++ ")"
                    return (tcRound tc + 1, Just tc)
                else return (qcRound tmQuorumCertificate + 1, Nothing)
        tmEpochFinalizationEntry <-
            if testBit tmFlags 1
                then do
                    Just <$> get
                else return Nothing
        tmAggregateSignature <- get
        return TimeoutMessageBody{..}

-- |The 'TimeoutSignatureMessage' associated with a 'TimeoutMessageBody'.
tmSignatureMessage ::
    -- |Genesis block hash
    BlockHash ->
    TimeoutMessageBody ->
    TimeoutSignatureMessage
tmSignatureMessage tsmGenesis tmb =
    TimeoutSignatureMessage
        { tsmRound = tmRound tmb,
          tsmQCRound = qcRound (tmQuorumCertificate tmb),
          tsmGenesis
        }

data TimeoutMessage = TimeoutMessage
    { tmBody :: !TimeoutMessageBody,
      tmSignature :: !BlockSig.Signature
    }

timeoutMessageBodySignatureBytes :: TimeoutMessageBody -> BS.ByteString
timeoutMessageBodySignatureBytes body = runPut $ do
    putByteString "TIMEOUTMESSAGE."
    put body

signTimeoutMessage :: TimeoutMessageBody -> BakerSignPrivateKey -> TimeoutMessage
signTimeoutMessage tmBody privKey = TimeoutMessage{..}
  where
    msg = timeoutMessageBodySignatureBytes tmBody
    tmSignature = BlockSig.sign privKey msg

checkTimeoutMessageSignature :: BakerSignVerifyKey -> TimeoutMessage -> Bool
checkTimeoutMessageSignature pubKey TimeoutMessage{..} =
    BlockSig.verify pubKey (timeoutMessageBodySignatureBytes tmBody) tmSignature
