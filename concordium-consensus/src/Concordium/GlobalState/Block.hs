{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Concordium.GlobalState.Block(
    BlockFinalizationData,
    module Concordium.GlobalState.Block
) where

import Control.Monad
import Data.Kind
import Data.Time
import Data.Serialize
import qualified Data.ByteString as ByteString

import Concordium.Common.Version
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlockSignature as Sig

import Concordium.GlobalState.Finalization

instance HashableTo BlockHash BakedBlock where
    getHash bb = generateBlockHash (blockSlot bb) (blockPointer bb) (blockBaker bb) (blockBakerKey bb) (blockProof bb) (blockNonce bb) (blockFinalizationData bb) (blockTransactions bb) (blockStateHash bb) (blockTransactionOutcomesHash bb)

generateBlockHash :: Slot         -- ^Block slot (must be non-zero)
    -> BlockHash                  -- ^Hash of parent block
    -> BakerId                    -- ^Identifier of block baker
    -> BakerSignVerifyKey         -- ^Claimed Baker public Key
    -> BlockProof                 -- ^Block proof
    -> BlockNonce                 -- ^Block nonce
    -> BlockFinalizationData      -- ^Finalization data
    -> [BlockItem]                -- ^Payload of the block.
    -> StateHash                  -- ^Statehash of the block.
    -> TransactionOutcomesHash     -- ^TransactionOutcomesHash of block.
    -> BlockHash
generateBlockHash slot parent baker bakerKey proof bnonce finData transactions stateHash transactionOutcomesHash
    = BlockHash topHash
    where
        topHash = Hash.hashOfHashes transactionOutcomes h1
        transactionOutcomes = v0TransactionOutcomesHash transactionOutcomesHash
        statehash = v0StateHash stateHash
        h1 = Hash.hashOfHashes statehash h2
        h2 = Hash.hashOfHashes h3 h4
        h3 = Hash.hashLazy . runPutLazy $ put finData
        h4 = Hash.hashLazy . runPutLazy $ do
            put slot
            put parent
            put baker
            put bakerKey
            put proof
            put bnonce
            putWord64be (fromIntegral (length transactions))
            mapM_ putBlockItemV0 transactions

instance HashableTo BlockHash (Block pv) where
    getHash (GenesisBlock genCore) = _gcCurrentHash genCore
    getHash (NormalBlock bb) = getHash bb

-- * Block type classes

-- |Accessors for the metadata of a baked (i.e. non-genesis) block.
class BlockMetadata d where
    -- |The hash of the parent block
    blockPointer :: d -> BlockHash
    -- |The identifier of the block's baker
    blockBaker :: d -> BakerId
    -- |The public Signing key the block claims it was signed with
    blockBakerKey :: d -> BakerSignVerifyKey
    -- |The proof that the baker was entitled to bake this block
    blockProof :: d -> BlockProof
    -- |The block nonce
    blockNonce :: d -> BlockNonce
    -- |A finalization proof, where given
    blockFinalizationData :: d -> BlockFinalizationData

-- |For a type @b@ representing a block, the type @BlockFieldType b@
-- represents the block metadata associated with a block.  Typically,
-- @BlockFieldType b@ is an instance of 'BlockMetadata'.
type family BlockFieldType (b :: Type) :: Type

-- |The 'BlockData' class provides an interface for the pure data associated
-- with a block.
class (BlockMetadata (BlockFieldType b)) => BlockData b where
    -- |The slot number of the block (0 for genesis block)
    blockSlot :: b -> Slot
    -- |The fields of a block, if it was baked; @Nothing@ for the genesis block.
    blockFields :: b -> Maybe (BlockFieldType b)
    -- |The transactions in a block in the variant they are currently stored in the block.
    blockTransactions :: b -> [BlockItem]
    -- |The hash of the TransactionOutcomes resulting from executing this block
    blockTransactionOutcomesHash :: b -> TransactionOutcomesHash
    -- |The hash of the state after executing this block
    blockStateHash :: b -> StateHash
    -- |The signature of the block, if it was baked; @Nothing@ for the genesis block.
    blockSignature :: b -> Maybe BlockSignature
    -- |Determine if the block is signed by the key given in the block
    -- (always 'True' for genesis block)
    verifyBlockSignature :: b -> Bool

class (BlockMetadata b, BlockData b, HashableTo BlockHash b, Show b) => BlockPendingData b where
    -- |Time at which the block was received
    blockReceiveTime :: b -> UTCTime

-- |Defines the block version serialization associated with a protocol version.
blockVersion :: SProtocolVersion pv -> Version
blockVersion SP1 = 2
blockVersion SP2 = 2
blockVersion SP3 = 2
blockVersion SP4 = 2
blockVersion SP5 = 2
{-# INLINE blockVersion #-}

-- |Type class that supports serialization of a block.
-- TODO: Eventually, the block will probably be sufficient to determine
-- the protocol version.
class (IsProtocolVersion pv) => EncodeBlock pv b where
    -- |Serialize a block. The serialization format version may depend
    -- on the protocol version, and should correspond to the result
    -- of 'blockVersion' for the given protocol version.
    putBlock :: SProtocolVersion pv -> Putter b
    -- |Serialize a block with a version header.
    putVersionedBlock :: SProtocolVersion pv -> Putter b
    putVersionedBlock pv b = putVersion (blockVersion pv) >> putBlock pv b

-- |Metadata that is supplied in a call to 'getBlock' or 'getVersionedBlock'.
type family DecodeBlockMetadata b

class (IsProtocolVersion pv) => DecodeBlock pv b where
    -- |Deserialize a block in the format version determined by
    -- the protocol version.
    getBlock :: SProtocolVersion pv -> DecodeBlockMetadata b -> Get b
    -- |Deserialize a block with a version header.
    -- The default implementation checks that the version is correct
    -- and calls getBlock.
    getVersionedBlock :: SProtocolVersion pv -> DecodeBlockMetadata b -> Get b
    getVersionedBlock pv md = do
        v <- getVersion
        unless (v == blockVersion pv) $ fail $
            "Unexpected block version (expected " ++ show (blockVersion pv) ++ " but saw " ++ show v ++ ")"
        getBlock pv md

-- * Block types

-- |The fields of a baked block.
data BlockFields = BlockFields {
    -- |The 'BlockHash' of the parent block
    bfBlockPointer :: !BlockHash,
    -- |The identity of the block baker
    bfBlockBaker :: !BakerId,
    -- |The public Signing key the block claims it was signed with
    bfBlockBakerKey :: !BakerSignVerifyKey,
    -- |The proof that the baker was entitled to bake this block
    bfBlockProof :: !BlockProof,
    -- |The block nonce
    bfBlockNonce :: !BlockNonce,
    -- |The 'BlockHash' of the last finalized block when the block was baked
    bfBlockFinalizationData :: !BlockFinalizationData
} deriving (Show)

instance BlockMetadata BlockFields where
    blockPointer = bfBlockPointer
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker
    {-# INLINE blockBaker #-}
    blockBakerKey = bfBlockBakerKey
    {-# INLINE blockBakerKey #-}
    blockProof = bfBlockProof
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce
    {-# INLINE blockNonce #-}
    blockFinalizationData = bfBlockFinalizationData
    {-# INLINE blockFinalizationData #-}

-- |A baked (i.e. non-genesis) block.
--
-- The type parameter @t@ is the type of the transaction
-- in the block.
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockMetadata
-- * BlockData
data BakedBlock = BakedBlock {
    -- |Slot number (must be >0)
    bbSlot :: !Slot,
    -- |Block fields
    bbFields :: !BlockFields,
    -- |Block transactions
    bbTransactions :: ![BlockItem],
    -- |Block State Hash
    bbStateHash :: !StateHash,
    -- |Block TransactionOutcomes Hash
    bbTransactionOutcomesHash :: !TransactionOutcomesHash,
    -- |Block signature
    -- With the way the abstractions are currently set up it is
    -- necessary that this is a lazy field. Specifically signing
    -- the block relies on this.
    bbSignature :: BlockSignature
} deriving (Show)

type instance BlockFieldType BakedBlock = BlockFields

instance BlockMetadata BakedBlock where
    blockPointer = bfBlockPointer . bbFields
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker . bbFields
    {-# INLINE blockBaker #-}
    blockBakerKey = bfBlockBakerKey . bbFields
    {-# INLINE blockBakerKey #-}
    blockProof = bfBlockProof . bbFields
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce . bbFields
    {-# INLINE blockNonce #-}
    blockFinalizationData = bfBlockFinalizationData . bbFields
    {-# INLINE blockFinalizationData #-}

instance BlockData BakedBlock where
    blockSlot = bbSlot
    {-# INLINE blockSlot #-}
    blockFields = Just . bbFields
    {-# INLINE blockFields #-}
    blockStateHash = bbStateHash
    {-# INLINE blockStateHash #-}
    blockTransactionOutcomesHash = bbTransactionOutcomesHash
    {-# INLINE blockTransactionOutcomesHash #-}
    blockTransactions = bbTransactions
    blockSignature = Just . bbSignature
    verifyBlockSignature b = Sig.verify (bfBlockBakerKey (bbFields b)) (Hash.hashToByteString (blockHash (getHash b))) (bbSignature b)

-- |Serialize a normal (non-genesis) block in V2 format.
putBakedBlockV2 :: BakedBlock -> Put
putBakedBlockV2 b = do
        put (blockSlot b)
        put (blockPointer b)
        put (blockBaker b)
        put (blockBakerKey b)
        put (blockProof b)
        put (blockNonce b)
        put (blockFinalizationData b)
        put (blockStateHash b)
        put (blockTransactionOutcomesHash b)
        putWord64be (fromIntegral (length (blockTransactions b)))
        mapM_ putBlockItemV0 (blockTransactions b)
        put (bbSignature b)

instance (IsProtocolVersion pv) => EncodeBlock pv BakedBlock where
    putBlock _ = putBakedBlockV2

-- |Deserialized a normal (non-genesis) block according to the V1/V2 format,
-- except for the initial slot number, which is provided as a parameter.
-- The arrival time for the transactions is also provided as a parameter.
getBakedBlockAtSlot :: SProtocolVersion pv -> Slot -> TransactionTime -> Get BakedBlock
getBakedBlockAtSlot spv sl arrivalTime = do
        bfBlockPointer <- get
        bfBlockBaker <- get
        bfBlockBakerKey <- get
        bfBlockProof <- get
        bfBlockNonce <- get
        bfBlockFinalizationData <- get
        bbStateHash <- get
        bbTransactionOutcomesHash <- get
        bbTransactions <- getListOf (getBlockItemV0 spv arrivalTime)
        bbSignature <- get
        return BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..}

-- |Deserialized a normal (non-genesis) block according to the V1/V2 format.
-- The arrival time for the transactions is provided as a parameter.
getBakedBlock :: SProtocolVersion pv -> TransactionTime -> Get BakedBlock
getBakedBlock spv arrivalTime = do
        sl <- get
        if sl == genesisSlot then
            fail "Unexpected genesis block"
        else
            getBakedBlockAtSlot spv sl arrivalTime

type instance DecodeBlockMetadata BakedBlock = TransactionTime

instance forall pv. (IsProtocolVersion pv) => DecodeBlock pv BakedBlock where
    getBlock _ = getBakedBlock (protocolVersion @pv)

-- |Representation of a block
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockData
data Block (pv :: ProtocolVersion)
    = GenesisBlock !GenesisConfiguration
    -- ^A genesis block with the given configuration.
    | NormalBlock !BakedBlock
    -- ^A baked (i.e. non-genesis) block

deriving instance Show (GenesisData pv) => Show (Block pv)

type instance BlockFieldType (Block pv) = BlockFieldType BakedBlock

instance BlockData (Block pv) where
    blockSlot GenesisBlock{} = 0
    blockSlot (NormalBlock bb) = blockSlot bb

    blockFields GenesisBlock{} = Nothing
    blockFields (NormalBlock bb) = blockFields bb

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock bb) = blockTransactions bb

    -- move into gendata?
    blockTransactionOutcomesHash GenesisBlock{} = getHash emptyTransactionOutcomes
    blockTransactionOutcomesHash (NormalBlock bb) = blockTransactionOutcomesHash bb

    -- FIXME: replace stub, and move into gendata 
    blockStateHash GenesisBlock{} = StateHashV0 minBound
    blockStateHash (NormalBlock bb) = blockStateHash bb

    blockSignature GenesisBlock{} = Nothing
    blockSignature (NormalBlock bb) = blockSignature bb

    verifyBlockSignature GenesisBlock{} = True
    verifyBlockSignature (NormalBlock bb) = verifyBlockSignature bb

    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}

instance (IsProtocolVersion pv) => EncodeBlock pv (Block pv) where
    putBlock _ (GenesisBlock gd) = put genesisSlot >> (putGenesisConfiguration gd)
    putBlock spv (NormalBlock bb) = putBlock spv bb

-- |A baked block, pre-hashed with its arrival time.
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockMetadata
-- * BlockData
-- * BlockPendingData
-- * HashableTo BlockHash
data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !BakedBlock,
    pbReceiveTime :: !UTCTime
}

type instance BlockFieldType PendingBlock = BlockFieldType BakedBlock

instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

instance Show PendingBlock where
    show pb = show (pbHash pb) ++ " (" ++ show (blockBaker $ bbFields $ pbBlock pb) ++ ")"

instance BlockMetadata PendingBlock where
    blockPointer = bfBlockPointer . bbFields . pbBlock
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker . bbFields . pbBlock
    {-# INLINE blockBaker #-}
    blockBakerKey = bfBlockBakerKey . bbFields . pbBlock
    {-# INLINE blockBakerKey #-}
    blockProof = bfBlockProof . bbFields . pbBlock
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce . bbFields . pbBlock
    {-# INLINE blockNonce #-}
    blockFinalizationData = bfBlockFinalizationData . bbFields . pbBlock
    {-# INLINE blockFinalizationData #-}

instance BlockData PendingBlock where
    blockSlot = blockSlot . pbBlock
    blockFields = blockFields . pbBlock
    blockTransactions = blockTransactions . pbBlock
    blockStateHash = blockStateHash . pbBlock
    blockTransactionOutcomesHash = blockTransactionOutcomesHash . pbBlock
    blockSignature = blockSignature . pbBlock
    verifyBlockSignature = verifyBlockSignature . pbBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE blockStateHash #-}
    {-# INLINE blockTransactionOutcomesHash #-}

instance BlockPendingData PendingBlock where
    blockReceiveTime = pbReceiveTime

instance HashableTo BlockHash PendingBlock where
    getHash = pbHash

makePendingBlock :: BakedBlock -> UTCTime -> PendingBlock
makePendingBlock pbBlock pbReceiveTime = PendingBlock{pbHash = getHash pbBlock,..}

-- |Generate a baked block.
signBlock :: BakerSignPrivateKey           -- ^Key for signing the new block
    -> Slot                       -- ^Block slot (must be non-zero)
    -> BlockHash                  -- ^Hash of parent block
    -> BakerId                    -- ^Identifier of block baker
    -- -> BakerSignVerifyKey         -- ^Claimed Baker public Key
    -> BlockProof                 -- ^Block proof
    -> BlockNonce                 -- ^Block nonce
    -> BlockFinalizationData      -- ^Finalization data
    -> [BlockItem]                -- ^Payload of the block.
    -> StateHash                  -- ^Statehash of the block.
    -> TransactionOutcomesHash     -- ^TransactionOutcomesHash of block.
    -> BakedBlock
signBlock key slot parent baker proof bnonce finData transactions stateHash transactionOutcomesHash
    | slot == 0 = error "Only the genesis block may have slot 0"
    | otherwise = do
        -- Generate hash on the unsigned block, and sign the hash
        let sig = Sig.sign key (Hash.hashToByteString (blockHash preBlockHash))
        preBlock $! sig
    where
        bakerKey = Sig.verifyKey key
        preBlock = BakedBlock slot (BlockFields parent baker bakerKey proof bnonce finData) transactions stateHash transactionOutcomesHash
        preBlockHash = generateBlockHash slot parent baker bakerKey proof bnonce finData transactions stateHash transactionOutcomesHash

type instance DecodeBlockMetadata PendingBlock = UTCTime

instance (IsProtocolVersion pv) => DecodeBlock pv PendingBlock where
    getBlock spv recTime = label "PendingBlock" $ do
        bb <- getBlock spv (utcTimeToTransactionTime recTime)
        return $! makePendingBlock bb recTime

-- |Deserialize a pending block with a version tag.
deserializeExactVersionedPendingBlock :: IsProtocolVersion pv => SProtocolVersion pv -> ByteString.ByteString -> UTCTime -> Either String PendingBlock
deserializeExactVersionedPendingBlock spv blockBS rectime =
    case runGet (getVersionedBlock spv rectime) blockBS of
        Left err -> Left $ "Block deserialization failed: " ++ err
        Right block1 -> Right block1
