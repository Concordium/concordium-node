{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Concordium.Types where

import GHC.Generics
import qualified Data.Map as Map
import Data.Word
import Data.ByteString
import Data.ByteString.Builder
import Data.Serialize.Put
import Data.Serialize

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.DummyVRF as VRF

type Slot = Word64
type BlockHash = ByteString
type BakerId = Word64
type BlockProof = VRF.Proof
type BlockSignature = Sig.Signature
type BlockNonce = (VRF.Hash, VRF.Proof)
type BlockData = ByteString
type BlockHeight = Word64

type LeadershipElectionNonce = ByteString
type BakerSignVerifyKey = Sig.VerifyKey
type BakerSignPrivateKey = Sig.SignKey
type BakerElectionVerifyKey = VRF.PublicKey
type BakerElectionPrivateKey = VRF.PrivateKey
type LotteryPower = Double
type ElectionDifficulty = Double

type VoterId = Word64
type VoterVerificationKey = Sig.VerifyKey
type VoterSignKey = Sig.SignKey
-- Using a floating point number for voter power may be a bad idea.
type VoterPower = Double


data Block = Block {
    blockSlot :: Slot,
    blockPointer :: BlockHash,
    blockBaker :: BakerId,
    blockProof :: BlockProof,
    blockNonce :: BlockNonce,
    blockLastFinalized :: BlockHash,
    blockData :: BlockData,
    blockSignature :: BlockSignature
}

serializeBlockBody ::
    Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> BlockData
    -> Put
serializeBlockBody sl bpt bid bpf bn lfpt bdata = do
    put sl
    put bpt
    put bid
    put bpf
    put bn
    put lfpt
    put bdata

serializeBlock :: Block -> Put
serializeBlock (Block {..}) = do
    serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData
    put blockSignature

deserializeBlock :: Get Block
deserializeBlock = do
    blockSlot <- get
    blockPointer <- get
    blockBaker <- get
    blockProof <- get
    blockNonce <- get
    blockLastFinalized <- get
    blockData <- get
    blockSignature <- get
    return (Block {..})

signBlock :: 
    Sig.SignKey
    -> Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> BlockData
    -> Block
signBlock ks blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData = Block {..}
    where
        blockSignature = Sig.sign ks $ runPut $
            serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData

verifyBlockSignature :: Sig.VerifyKey -> Block -> Bool
verifyBlockSignature kv Block{..} = Sig.verify kv bs blockSignature
    where
        bs = runPut $ serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData

hashBlock :: Block -> BlockHash
hashBlock = Hash.hashLazy . runPutLazy . serializeBlock

data FinalizationProof = FinalizationProof

emptyFinalizationProof :: FinalizationProof
emptyFinalizationProof = FinalizationProof


data FinalizationRecord = FinalizationRecord {
    finalizationIndex :: BlockHeight,
    finalizationBlockPointer :: BlockHash,
    finalizationProof :: FinalizationProof,
    finalizationDelay :: Word32
}

data BakerInfo = BakerInfo {
    bakerElectionVerifyKey :: BakerElectionVerifyKey,
    bakerSignatureVerifyKey :: BakerSignVerifyKey,
    bakerLotteryPower :: LotteryPower
} deriving (Eq, Generic)
instance Serialize BakerInfo where

data BirkParameters = BirkParameters {
    birkLeadershipElectionNonce :: LeadershipElectionNonce,
    birkElectionDifficulty :: ElectionDifficulty,
    birkBakers :: Map.Map BakerId BakerInfo
} deriving (Eq, Generic)
instance Serialize BirkParameters where

birkBaker :: BakerId -> BirkParameters -> Maybe BakerInfo
birkBaker bid bps = Map.lookup bid (birkBakers bps)


data VoterInfo = VoterInfo {
    voterVerificationKey :: VoterVerificationKey,
    voterPower :: VoterPower
} deriving (Eq, Generic)
instance Serialize VoterInfo where

data FinalizationParameters = FinalizationParameters (Map.Map VoterId VoterInfo)
    deriving (Eq, Generic)
instance Serialize FinalizationParameters where

-- | Time in seconds since the epoch
type Timestamp = Word64
-- | Time duration in seconds
type Duration = Word64

data GenesisData = GenesisData {
    genesisTime :: Timestamp,
    genesisSlotDuration :: Duration,
    genesisBirkParameters :: BirkParameters,
    genesisFinalizationParameters :: FinalizationParameters
} deriving (Generic)

instance Serialize GenesisData where

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = Block {
    blockSlot = 0,
    blockPointer = empty,
    blockBaker = 0,
    blockProof = VRF.emptyProof,
    blockNonce = (VRF.emptyHash, VRF.emptyProof),
    blockLastFinalized = empty,
    blockData = runPut $ put genData,
    blockSignature = Sig.emptySignature
}