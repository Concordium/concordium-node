{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Block(
    BlockFinalizationData,
    module Concordium.GlobalState.Block
) where

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

-- FIXME: temporary import to stub hashes for gensis
import Data.FixedByteString as FBS

hashGenesisData :: GenesisData -> Hash
hashGenesisData genData = Hash.hashLazy . runPutLazy $ put genesisSlot >> put genData

instance HashableTo BlockHash BakedBlock where
    -- FIXME: Hash of a block should be independent of serialization version.
    -- This will be fixed as part of block hashing revision.
    getHash b = BlockHashV0 . Hash.hashLazy . runPutLazy $ blockBodyV0 b >> put (bbSignature b)

instance HashableTo BlockHash Block where
    getHash (GenesisBlock genData) = BlockHashV0 (hashGenesisData genData)
    getHash (NormalBlock bb) = getHash bb

-- * Block type classes

-- |Accessors for the metadata of a baked (i.e. non-genesis) block.
class BlockMetadata d where
    -- |The hash of the parent block
    blockPointer :: d -> BlockHash
    -- |The identifier of the block's baker
    blockBaker :: d -> BakerId
    -- |The public Signing key the block claims it was signed with
    blockClaimedKey :: d -> Sig.SignKey
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
    blockSignature :: b -> Maybe BlockSignature
    -- |Determine if the block is signed by the given key
    -- (always 'True' for genesis block)
    verifyBlockSignature :: Sig.VerifyKey -> b -> Bool
    -- |Provides a pure serialization of the block according to V0 format.
    --
    -- This means that if some IO is needed for serializing the block (as
    -- in the case of the Persistent blocks), it will not be done and we
    -- would just serialize the hashes of the transactions. This is useful in order
    -- to get the serialized version of the block that we want to write into the disk.
    -- 
    -- FIXME: Having this method here is likely not what we want, but I'm keeping it
    -- right now since it is most consistent for the moment and changing it will require
    -- changing some other abstractions.
    putBlockV0 :: b -> Put

class (BlockMetadata b, BlockData b, HashableTo BlockHash b, Show b) => BlockPendingData b where
    -- |Time at which the block was received
    blockReceiveTime :: b -> UTCTime

-- * Block types

-- |The fields of a baked block.
data BlockFields = BlockFields {
    -- |The 'BlockHash' of the parent block
    bfBlockPointer :: !BlockHash,
    -- |The identity of the block baker
    bfBlockBaker :: !BakerId,
    -- |The public Signing key the block claims it was signed with
    bfBlockClaimedKey :: !Sig.SignKey,
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
    blockClaimedKey = bfBlockClaimedKey
    {-# INLINE blockClaimedKey #-}
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
    blockClaimedKey = bfBlockClaimedKey . bbFields
    {-# INLINE blockClaimedKey #-}
    blockProof = bfBlockProof . bbFields
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce . bbFields
    {-# INLINE blockNonce #-}
    blockFinalizationData = bfBlockFinalizationData . bbFields
    {-# INLINE blockFinalizationData #-}

blockBodyV0 :: (BlockMetadata b, BlockData b) => b -> Put
blockBodyV0 b = do
        put (blockSlot b)
        put (blockPointer b)
        put (blockBaker b)
        put (blockClaimedKey b)
        put (blockProof b)
        put (blockNonce b)
        put (blockFinalizationData b)
        putWord64be (fromIntegral (length (blockTransactions b)))
        mapM_ putBlockItemV0 $ blockTransactions b

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
    -- FIXME: Signature verification should be independent of serialization format
    -- of blocks, this will be fixed as part of block hashing revision.
    verifyBlockSignature key b = Sig.verify key (runPut (blockBodyV0 b)) (bbSignature b)
    {-# INLINE putBlockV0 #-}
    putBlockV0 = putBakedBlockV0

-- |Serialize a normal (non-genesis) block according to the V0 format.
--
-- NB: This function does not record the version directly, use
-- 'putVersionedBlockV0' if that is needed.
putBakedBlockV0 :: BakedBlock -> Put
putBakedBlockV0 b = blockBodyV0 b >> put (bbSignature b)

-- |Representation of a block
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockData
data Block
    = GenesisBlock !GenesisData
    -- ^A genesis block
    | NormalBlock !BakedBlock
    -- ^A baked (i.e. non-genesis) block
    deriving (Show)

type instance BlockFieldType Block = BlockFieldType BakedBlock

instance BlockData Block where
    blockSlot GenesisBlock{} = 0
    blockSlot (NormalBlock bb) = blockSlot bb

    blockFields GenesisBlock{} = Nothing
    blockFields (NormalBlock bb) = blockFields bb

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock bb) = blockTransactions bb

    -- FIXME: move into gendata
    blockTransactionOutcomesHash GenesisBlock{} = TransactionOutcomesHashV0 (Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word)))))
    blockTransactionOutcomesHash (NormalBlock bb) = blockTransactionOutcomesHash bb

    -- FIXME: move into gendata 
    blockStateHash GenesisBlock{} = StateHashV0 (Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word)))))
    blockStateHash (NormalBlock bb) = blockStateHash bb

    blockSignature GenesisBlock{} = Nothing
    blockSignature (NormalBlock bb) = blockSignature bb

    verifyBlockSignature _ GenesisBlock{} = True
    verifyBlockSignature key (NormalBlock bb) = verifyBlockSignature key bb

    putBlockV0 (GenesisBlock gd) = put genesisSlot <> put gd
    putBlockV0 (NormalBlock bb) = putBakedBlockV0 bb
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE putBlockV0 #-}

-- |Serialize a block according to V0 format, also prepending the version.
putVersionedBlockV0 :: BlockData b => b -> Put
putVersionedBlockV0 b = putVersion 0 <> putBlockV0 b

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
    blockClaimedKey = bfBlockClaimedKey . bbFields . pbBlock
    {-# INLINE blockClaimedKey #-}
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
    verifyBlockSignature key = verifyBlockSignature key . pbBlock
    putBlockV0 = putBlockV0 . pbBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE blockStateHash #-}
    {-# INLINE blockTransactionOutcomesHash #-}
    {-# INLINE putBlockV0 #-}

instance BlockPendingData PendingBlock where
    blockReceiveTime = pbReceiveTime

instance HashableTo BlockHash PendingBlock where
    getHash = pbHash

-- |Deserialize a versioned block. Read the version and decide how to parse the
-- remaining data based on the version.
--
-- Currently only supports version 0
getExactVersionedBlock :: TransactionTime -> Get Block
getExactVersionedBlock time = do
    version <- get :: Get Version
    case version of
      0 -> getBlockV0 time
      n -> fail $ "Unsupported block version: " ++ (show n)

-- |Deserialize a block according to the version 0 format.
--
-- NB: This does not check transaction signatures.
getBlockV0 :: TransactionTime -> Get Block
getBlockV0 arrivalTime = do
    sl <- get
    if sl == 0 then GenesisBlock <$> get
    else do
        bfBlockPointer <- get
        bfBlockBaker <- get
        bfBlockClaimedKey <- get
        bfBlockProof <- get
        bfBlockNonce <- get
        bfBlockFinalizationData <- get
        bbTransactions <- getListOf (getBlockItemV0 arrivalTime)
        bbStateHash <- get
        bbTransactionOutcomesHash <- get
        bbSignature <- get
        return $ NormalBlock (BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..})

makePendingBlock :: BakedBlock -> UTCTime -> PendingBlock
makePendingBlock pbBlock pbReceiveTime = PendingBlock{pbHash = getHash pbBlock,..}

-- |Generate a baked block.
signBlock :: BakerSignPrivateKey           -- ^Key for signing the new block
    -> Slot                       -- ^Block slot (must be non-zero)
    -> BlockHash                  -- ^Hash of parent block
    -> BakerId                    -- ^Identifier of block baker
    -> Sig.SignKey                -- ^Claimed Baker Signing Key
    -> BlockProof                 -- ^Block proof
    -> BlockNonce                 -- ^Block nonce
    -> BlockFinalizationData      -- ^Finalization data
    -> [BlockItem]                -- ^Payload of the block.
    -> StateHash                  -- ^Statehash of the block.
    -> TransactionOutcomesHash     -- ^TransactionOutcomesHash of block.
    -> BakedBlock
signBlock key slot parent baker claimedKey proof bnonce finData transactions stateHash transactionOutcomesHash
    | slot == 0 = error "Only the genesis block may have slot 0"
    | otherwise = do
        -- FIXME: Signature of a block should be independent of body format serialization.
        -- This will be fixed as part of block hashing revision.
        let sig = Sig.sign key . runPut $ blockBodyV0 (preBlock undefined)
        preBlock $! sig
    where
        preBlock = BakedBlock slot (BlockFields parent baker claimedKey proof bnonce finData) transactions stateHash transactionOutcomesHash

deserializeExactVersionedPendingBlock :: ByteString.ByteString -> UTCTime -> Either String PendingBlock
deserializeExactVersionedPendingBlock blockBS rectime =
    case runGet (getExactVersionedBlock (utcTimeToTransactionTime rectime)) blockBS of
        Left err -> Left $ "Block deserialization failed: " ++ err
        Right (GenesisBlock {}) -> Left $ "Block deserialization failed: unexpected genesis block"
        Right (NormalBlock block0) -> Right $! makePendingBlock block0 rectime

deserializePendingBlockV0 :: ByteString.ByteString -> UTCTime -> Either String PendingBlock
deserializePendingBlockV0 blockBS rectime =
    case runGet (getBlockV0 (utcTimeToTransactionTime rectime)) blockBS of
        Left err -> Left $ "Block deserialization failed: " ++ err
        Right (GenesisBlock {}) -> Left $ "Block deserialization failed: unexpected genesis block"
        Right (NormalBlock block0) -> Right $! makePendingBlock block0 rectime
