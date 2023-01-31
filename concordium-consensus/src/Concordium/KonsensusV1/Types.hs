{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Concordium.KonsensusV1.Types where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Void
import Data.Word
import Numeric.Natural

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Utils.Serialization

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LFMBT

-- |A round number for consensus.
newtype Round = Round {theRound :: Word64}
    deriving (Eq, Ord, Show, Serialize, Num, Integral, Real, Enum, Bounded)

-- |A strict version of 'Maybe'. We deliberately avoid defining generalised serialization and
-- hashing instances, so that specific instances can be given as appropriate.
data Optionally a
    = Absent
    | Present !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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

-- |The serialization of a 'FinalizerSet' consists of a length (Word32, big-endian), followed by
-- that many bytes, the first of which (if any) must be non-zero. These bytes encode the bit-vector
-- in big-endian. This enforces that the serialization of a finalizer set is unique.
instance Serialize FinalizerSet where
    put fs = do
        let (byteCount, putBytes) = unroll 0 (return ()) (theFinalizerSet fs)
        putWord32be byteCount
        putBytes
      where
        unroll bc cont 0 = (bc, cont)
        unroll bc cont n = unroll (bc + 1) (putWord8 (fromIntegral n) >> cont) (shiftR n 8)
    get = label "FinalizerSet" $ do
        byteCount <- getWord32be
        FinalizerSet <$> roll1 byteCount
      where
        roll1 0 = return 0
        roll1 bc = do
            b <- getWord8
            when (b == 0) $ fail "unexpected 0 byte"
            roll (bc - 1) (fromIntegral b)
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
    deriving (Eq)

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

instance HashableTo Hash.Hash QuorumCertificate where
    getHash = Hash.hash . encode

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
type SuccessorProof = BlockQuasiHash

-- |Compute the 'BlockHash' of a block that is the successor of another block.
successorBlockHash ::
    -- |Block round
    Round ->
    -- |Block epoch
    Epoch ->
    -- |Predecessor block
    BlockHash ->
    SuccessorProof ->
    BlockHash
successorBlockHash bhRound bhEpoch bhParent = computeBlockHash bhh
  where
    bhh = getHash BlockHeader{..}

-- |A finalization entry that witnesses that a block has been finalized with quorum certificates
-- for two consecutive rounds. The finalization entry includes a proof that the blocks are in
-- consecutive rounds so that the entry can be validated without the second block.
--
-- The following invariants hold:
--
-- - @qcRound feSuccessorQuorumCertificate == qcRound feFinalizedQuorumCertificate + 1@
-- - @qcBlock feSuccessorQuorumCertificate == successorBlockHash (qcRound feSuccessorQuorumCertificate) (qcEpoch feSuccessorQuorumCertificate) (qcBlock feFinalizedQuorumCertificate) feSuccessorProof@
data FinalizationEntry = FinalizationEntry
    { -- |Quorum certificate for the finalized block.
      feFinalizedQuorumCertificate :: !QuorumCertificate,
      -- |Quorum certificate for the successor block.
      feSuccessorQuorumCertificate :: !QuorumCertificate,
      -- |Proof that establishes the successor block is the immediate successor of the finalized
      -- block (without further knowledge of the successor block beyond its hash).
      feSuccessorProof :: !SuccessorProof
    }
    deriving (Eq)

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

instance HashableTo Hash.Hash (Optionally FinalizationEntry) where
    getHash Absent = Hash.hash $ encode (0 :: Word8)
    getHash (Present fe) = Hash.hash $ runPut $ do
        putWord8 1
        put fe

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
    deriving (Eq)

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
    deriving (Eq)

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

instance HashableTo Hash.Hash (Optionally TimeoutCertificate) where
    getHash Absent = Hash.hash $ encode (0 :: Word8)
    getHash (Present tc) = Hash.hash $ runPut $ do
        putWord8 1
        put tc

-- |Check the signature in a timeout certificate
checkTimeoutCertificateSignature ::
    -- |Genesis block hash
    BlockHash ->
    (FinalizerSet -> [BakerAggregationVerifyKey]) ->
    TimeoutCertificate ->
    Bool
checkTimeoutCertificateSignature tsmGenesis toKeys TimeoutCertificate{..} =
    Bls.verifyAggregateHybrid msgsKeys (theTimeoutSignature tcAggregateSignature)
  where
    msgsKeys =
        [ (timeoutSignatureMessageBytes TimeoutSignatureMessage{tsmRound = tcRound, ..}, toKeys fs)
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
      tmTimeoutCertificate :: !(Optionally TimeoutCertificate),
      -- |A epoch finalization entry for the epoch @qcEpoch tmQuorumCertificate@, if one is known.
      tmEpochFinalizationEntry :: !(Optionally FinalizationEntry),
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
            Absent -> (0, return ())
            Present tc -> (bit 0, put tc)
        (efeFlag, putEFE) = case tmEpochFinalizationEntry of
            Absent -> (0, return ())
            Present efe -> (bit 1, put efe)
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
                    return (tcRound tc + 1, Present tc)
                else return (qcRound tmQuorumCertificate + 1, Absent)
        tmEpochFinalizationEntry <-
            if testBit tmFlags 1
                then do
                    Present <$> get
                else return Absent
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

-- |A timeout message including the sender's signature.
data TimeoutMessage = TimeoutMessage
    { tmBody :: !TimeoutMessageBody,
      tmSignature :: !BlockSig.Signature
    }

-- |Byte representation of a 'TimeoutMessageBody' used for signing the timeout message.
timeoutMessageBodySignatureBytes :: TimeoutMessageBody -> BS.ByteString
timeoutMessageBodySignatureBytes body = runPut $ do
    putByteString "TIMEOUTMESSAGE."
    put body

-- |Sign a timeout message.
signTimeoutMessage :: TimeoutMessageBody -> BakerSignPrivateKey -> TimeoutMessage
signTimeoutMessage tmBody privKey = TimeoutMessage{..}
  where
    msg = timeoutMessageBodySignatureBytes tmBody
    tmSignature = BlockSig.sign privKey msg

-- |Check the signature on a timeout message.
checkTimeoutMessageSignature :: BakerSignVerifyKey -> TimeoutMessage -> Bool
checkTimeoutMessageSignature pubKey TimeoutMessage{..} =
    BlockSig.verify pubKey (timeoutMessageBodySignatureBytes tmBody) tmSignature

-- |Projections for the data associated with a baked (i.e. non-genesis) block.
class BakedBlockData d where
    -- |Quorum certificate on the parent block.
    blockQuorumCertificate :: d -> QuorumCertificate

    -- |Parent block hash.
    blockParent :: d -> BlockHash
    blockParent = qcBlock . blockQuorumCertificate

    -- |'BakerId' of the baker of the block.
    blockBaker :: d -> BakerId

    -- |Signature verification key of the baker of the block.
    blockBakerKey :: d -> BakerSignVerifyKey

    -- |If the previous round timed-out, the timeout certificate for that round.
    blockTimeoutCertificate :: d -> Optionally TimeoutCertificate

    -- |If this block begins a new epoch, this is the finalization entry that finalizes the
    -- trigger block.
    blockEpochFinalizationEntry :: d -> Optionally FinalizationEntry

    -- |The 'BlockNonce' generated by the baker's VRF.
    blockNonce :: d -> BlockNonce

    -- |The baker's signature on the block.
    blockSignature :: d -> BlockSignature

instance BakedBlockData Void where
    blockQuorumCertificate = absurd
    blockBaker = absurd
    blockBakerKey = absurd
    blockTimeoutCertificate = absurd
    blockEpochFinalizationEntry = absurd
    blockNonce = absurd
    blockSignature = absurd

-- |Projections for the data associated with a block (including a genesis block).
class BlockData b where
    -- |An associated type that should be an instance of 'BakedBlockData'.
    -- This is returned by 'blockBakedData' for non-genesis blocks.
    type BakedBlockDataType b

    -- |Round number of the block.
    blockRound :: b -> Round

    -- |Epoch number of the block.
    blockEpoch :: b -> Epoch

    -- |Timestamp of the block.
    blockTimestamp :: b -> Timestamp

    -- |If the block is a baked (i.e. non-genesis) block, this returns the baked block data.
    -- If the block is a genesis block, this returns 'Absent'.
    blockBakedData :: b -> Optionally (BakedBlockDataType b)

    -- |The list of transactions in the block.
    blockTransactions :: b -> [BlockItem]

    -- |The hash of the block state after executing the block.
    blockStateHash :: b -> StateHash

data BakedBlock = BakedBlock
    { -- |Block round number.
      bbRound :: !Round,
      -- |Block epoch number.
      bbEpoch :: !Epoch,
      -- |Block nominal timestamp.
      bbTimestamp :: !Timestamp,
      -- |Block baker identity.
      bbBaker :: !BakerId,
      -- |Block baker signature verification key.
      bbBakerKey :: !BakerSignVerifyKey,
      -- |Quorum certificate of parent block.
      bbQuorumCertificate :: !QuorumCertificate,
      -- |Timeout certificate if the previous round timed-out.
      bbTimeoutCertificate :: !(Optionally TimeoutCertificate),
      -- |Epoch finalization entry if this is the first block in a new epoch.
      bbEpochFinalizationEntry :: !(Optionally FinalizationEntry),
      -- |Block nonce generated from the baker's VRF.
      bbNonce :: !BlockNonce,
      -- |Transactions in the block.
      bbTransactions :: ![BlockItem],
      -- |Hash of the transaction outcomes.
      bbTransactionOutcomesHash :: !TransactionOutcomesHash,
      -- |Hash of the block state.
      bbStateHash :: !StateHash
    }
    deriving (Eq)

data SignedBlock = SignedBlock
    { sbBlock :: !BakedBlock,
      sbHash :: !BlockHash,
      sbSignature :: !BlockSignature
    }

instance BakedBlockData SignedBlock where
    blockQuorumCertificate = bbQuorumCertificate . sbBlock
    blockBaker = bbBaker . sbBlock
    blockBakerKey = bbBakerKey . sbBlock
    blockTimeoutCertificate = bbTimeoutCertificate . sbBlock
    blockEpochFinalizationEntry = bbEpochFinalizationEntry . sbBlock
    blockNonce = bbNonce . sbBlock
    blockSignature = sbSignature

instance HashableTo BlockHash SignedBlock where
    getHash = sbHash

instance Monad m => MHashableTo m BlockHash SignedBlock

-- |The bytes that are signed by a block signature.
blockSignatureMessageBytes :: BlockHash -> BS.ByteString
blockSignatureMessageBytes = Hash.hashToByteString . blockHash

-- |Verify that a block is correctly signed by the baker key present in the block.
-- (This does not authenticate that the key corresponds to a genuine baker.)
verifyBlockSignature :: (BakedBlockData b, HashableTo BlockHash b) => b -> Bool
verifyBlockSignature b =
    BlockSig.verify
        (blockBakerKey b)
        (blockSignatureMessageBytes $ getHash b)
        (blockSignature b)

-- |Sign a block hash as a baker.
signBlockHash :: BakerSignPrivateKey -> BlockHash -> BlockSignature
signBlockHash privKey bh = BlockSig.sign privKey (blockSignatureMessageBytes bh)

-- |Message used to generate the block nonce with the VRF.
blockNonceMessage :: LeadershipElectionNonce -> Round -> BS.ByteString
blockNonceMessage leNonce rnd = runPut $ do
    putByteString "NONCE"
    put leNonce
    put rnd

-- |Generate the block nonce.
computeBlockNonce :: LeadershipElectionNonce -> Round -> BakerElectionPrivateKey -> BlockNonce
computeBlockNonce leNonce rnd key = VRF.prove key (blockNonceMessage leNonce rnd)

-- |Verify a block nonce.
verifyBlockNonce :: LeadershipElectionNonce -> Round -> BakerElectionVerifyKey -> BlockNonce -> Bool
verifyBlockNonce leNonce rnd verifKey = VRF.verify verifKey (blockNonceMessage leNonce rnd)

-- |The 'BlockHeader' consists of basic information about the block that is used in the generation
-- of the block hash. This data is near the root of the Merkle tree constructing the block hash.
data BlockHeader = BlockHeader
    { bhRound :: !Round,
      bhEpoch :: !Epoch,
      bhParent :: !BlockHash
    }
    deriving (Eq)

-- |The block header for a 'BakedBlock'.
bbBlockHeader :: BakedBlock -> BlockHeader
bbBlockHeader BakedBlock{..} =
    BlockHeader
        { bhRound = bbRound,
          bhEpoch = bbEpoch,
          bhParent = qcBlock bbQuorumCertificate
        }

-- |The hash of a 'BlockHeader'. This is combined with the 'BlockQuasiHash' to produce a
-- 'BlockHash'.
newtype BlockHeaderHash = BlockHeaderHash {theBlockHeaderHash :: Hash.Hash}
    deriving (Eq, Ord, Show, Serialize)

instance HashableTo BlockHeaderHash BlockHeader where
    getHash BlockHeader{..} = BlockHeaderHash $ Hash.hash $ runPut $ do
        put bhRound
        put bhEpoch
        put bhParent

instance HashableTo BlockHeaderHash BakedBlock where
    getHash = getHash . bbBlockHeader

-- |Hash of a block's contents. This is combined with the 'BlockHeaderHash' to produce a
-- 'BlockHash'.
newtype BlockQuasiHash = BlockQuasiHash {theBlockQuasiHash :: Hash.Hash}
    deriving (Eq, Ord, Show, Serialize)

-- |Compute the hash from a list of transactions.
-- TODO: determine if this hashing scheme is appropriate.
computeTransactionsHash :: [BlockItem] -> Hash.Hash
computeTransactionsHash bis = LFMBT.hashAsLFMBT (Hash.hash "") (v0TransactionHash . getHash <$> bis)

instance HashableTo BlockQuasiHash BakedBlock where
    getHash BakedBlock{..} = BlockQuasiHash $ Hash.hashOfHashes metaHash dataHash
      where
        metaHash = Hash.hashOfHashes bakerInfoHash certificatesHash
          where
            bakerInfoHash = Hash.hashOfHashes timestampBakerHash keyNonceHash
              where
                timestampBakerHash = Hash.hash $ runPut $ do
                    put bbTimestamp
                    put bbBaker
                keyNonceHash = Hash.hashOfHashes bakerKeyHash nonceHash
                  where
                    bakerKeyHash = Hash.hash $ encode bbBakerKey
                    nonceHash = Hash.hash $ encode bbNonce
            certificatesHash = Hash.hashOfHashes qcHash timeoutFinalizationHash
              where
                qcHash = getHash bbQuorumCertificate
                timeoutFinalizationHash = Hash.hashOfHashes timeoutHash finalizationHash
                  where
                    timeoutHash = getHash bbTimeoutCertificate
                    finalizationHash = getHash bbEpochFinalizationEntry
        dataHash = Hash.hashOfHashes transactionsAndOutcomesHash stateHash
          where
            transactionsAndOutcomesHash = Hash.hashOfHashes transactionsHash outcomesHash
              where
                transactionsHash = computeTransactionsHash bbTransactions
                outcomesHash = tohGet bbTransactionOutcomesHash
            stateHash = v0StateHash bbStateHash

-- |Compute the block hash from the header hash and quasi-hash.
computeBlockHash :: BlockHeaderHash -> BlockQuasiHash -> BlockHash
computeBlockHash bhh bqh =
    BlockHash $
        Hash.hashOfHashes
            (theBlockHeaderHash bhh)
            (theBlockQuasiHash bqh)