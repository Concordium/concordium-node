{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, RecordWildCards, TypeFamilies, FlexibleContexts, TypeSynonymInstances, FunctionalDependencies, DerivingVia, ScopedTypeVariables, FlexibleInstances, UndecidableInstances #-}
module Concordium.GlobalState.Block where

import Data.Time
import Data.Serialize

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Classes

-- | Serialize the block by first converting all the transactions to memory transactions
fullBody :: forall m t. (Convert (BakedBlock Transaction) (BakedBlock t) m) =>
           BakedBlock t -> m Put
fullBody b = do
  bx <- toMemoryRepr b :: m (BakedBlock Transaction)
  return $ blockBody bx

hashGenesisData :: GenesisData -> Hash
hashGenesisData genData = Hash.hashLazy . runPutLazy $ put genesisSlot >> put genData

instance HashableTo BlockHash (BakedBlock Transaction) where
    getHash b = Hash.hashLazy . runPutLazy $ blockBody b >> put (bbSignature b)

instance HashableTo BlockHash (Block Transaction) where
    getHash (GenesisBlock genData) = hashGenesisData genData
    getHash (NormalBlock bb) = getHash bb

-- * Block type classes

-- |Accessors for the metadata of a baked (i.e. non-genesis) block.
class BlockMetadata d where
    -- |The hash of the parent block
    blockPointer :: d -> BlockHash
    -- |The identifier of the block's baker
    blockBaker :: d -> BakerId
    -- |The proof that the baker was entitled to bake this block
    blockProof :: d -> BlockProof
    -- |The block nonce
    blockNonce :: d -> BlockNonce
    -- |The hash of the last finalized block when this block was baked
    blockLastFinalized :: d -> BlockHash

-- |For a type @b@ representing a block, the type @BlockFieldType b@
-- represents the block metadata associated with a block.  Typically,
-- @BlockFieldType b@ is an instance of 'BlockMetadata'.
type family BlockFieldType (b :: *) :: *

-- |The type of the transactions in the block.
--
-- The block datatypes are parametrized by a type variable @t@ that indicates
-- its Transaction type. By default given a Type @Block... t@, its
-- @BlockTransactionType@ is automatically defined as @t@.
--
-- As GlobalStateTypes provides the block types associated with a given monad
-- @m@, we can get the TransactionType associated with that monad using the expression
-- @BlockTransactionType (BlockPointer m)@.
type family BlockTransactionType (b :: *) :: *

-- |The 'BlockData' class provides an interface for the pure data associated
-- with a block.
class (BlockMetadata (BlockFieldType b)) => BlockData b where
    -- |The slot number of the block (0 for genesis block)
    blockSlot :: b -> Slot
    -- |The fields of a block, if it was baked; @Nothing@ for the genesis block.
    blockFields :: b -> Maybe (BlockFieldType b)
    -- |The transactions in a block in the variant they are currently stored in the block.
    blockTransactions :: b -> [BlockTransactionType b]
    blockSignature :: b -> Maybe BlockSignature
    -- |Provides a pure serialization of the block.
    --
    -- This means that if some IO is needed for serializing the block (as
    -- in the case of the Persistent blocks), it will not be done and we
    -- would just serialize the hashes of the transactions. This is useful in order
    -- to get the serialized version of the block that we want to write into the disk.
    blockBody :: b -> Put

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
    -- |The proof that the baker was entitled to bake this block
    bfBlockProof :: !BlockProof,
    -- |The block nonce
    bfBlockNonce :: !BlockNonce,
    -- |The 'BlockHash' of the last finalized block when the block was baked
    bfBlockLastFinalized :: !BlockHash
} deriving (Show)

instance BlockMetadata BlockFields where
    blockPointer = bfBlockPointer
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker
    {-# INLINE blockBaker #-}
    blockProof = bfBlockProof
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce
    {-# INLINE blockNonce #-}
    blockLastFinalized = bfBlockLastFinalized
    {-# INLINE blockLastFinalized #-}

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
data BakedBlock t = BakedBlock {
    -- |Slot number (must be >0)
    bbSlot :: !Slot,
    -- |Block fields
    bbFields :: !BlockFields,
    -- |Block transactions
    bbTransactions :: ![t],
    -- |Block signature
    bbSignature :: BlockSignature
} deriving (Show)

type instance BlockFieldType (BakedBlock t) = BlockFields
type instance BlockTransactionType (BakedBlock t) = t

type BroadcastableBlock = BakedBlock Transaction

instance BlockMetadata (BakedBlock t) where
    blockPointer = bfBlockPointer . bbFields
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker . bbFields
    {-# INLINE blockBaker #-}
    blockProof = bfBlockProof . bbFields
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce . bbFields
    {-# INLINE blockNonce #-}
    blockLastFinalized = bfBlockLastFinalized . bbFields
    {-# INLINE blockLastFinalized #-}

instance (ToPut t) => BlockData (BakedBlock t) where
    blockSlot = bbSlot
    {-# INLINE blockSlot #-}
    blockFields = Just . bbFields
    {-# INLINE blockFields #-}
    blockTransactions = bbTransactions
    blockSignature = Just . bbSignature
    blockBody b = do
        put (blockSlot b)
        put (blockPointer b)
        put (blockBaker b)
        put (blockProof b)
        put (blockNonce b)
        put (blockLastFinalized b)
        putListOf toPut $ blockTransactions b

instance Convert Transaction t m => Convert (BakedBlock Transaction) (BakedBlock t) m where
  toMemoryRepr BakedBlock{..} = do
    newTxs <- mapM toMemoryRepr bbTransactions
    return $ BakedBlock{bbTransactions = newTxs,..}
  fromMemoryRepr BakedBlock{..} = do
    newTxs <- mapM fromMemoryRepr bbTransactions
    return $ BakedBlock{bbTransactions = newTxs,..}

instance (Monad m, Convert (BakedBlock Transaction) (BakedBlock t) m) =>
    HashableTo (m BlockHash) (BakedBlock t) where
  getHash b = do
    bx <- toMemoryRepr b :: m (BakedBlock Transaction)
    return $ Hash.hashLazy . runPutLazy $ blockBody bx >> put (bbSignature b)

-- |Representation of a block
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockData
data Block t
    = GenesisBlock !GenesisData
    -- ^A genesis block
    | NormalBlock !(BakedBlock t)
    -- ^A baked (i.e. non-genesis) block
    deriving (Show)

type instance BlockFieldType (Block t) = BlockFieldType (BakedBlock t)
type instance BlockTransactionType (Block t) = t

instance (ToPut t) => BlockData (Block t) where
    blockSlot GenesisBlock{} = 0
    blockSlot (NormalBlock bb) = blockSlot bb

    blockFields GenesisBlock{} = Nothing
    blockFields (NormalBlock bb) = blockFields bb

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock bb) = blockTransactions bb

    blockSignature GenesisBlock{} = Nothing
    blockSignature (NormalBlock bb) = blockSignature bb

    blockBody (GenesisBlock gd) = put genesisSlot >> put gd
    blockBody (NormalBlock bb) = blockBody bb

    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE blockBody #-}

instance (Monad m, Convert (BakedBlock Transaction) (BakedBlock t) m) =>
         HashableTo (m BlockHash) (Block t) where
    getHash (GenesisBlock genData) =
      return $! hashGenesisData genData
    getHash (NormalBlock bb) = getHash bb

-- |A baked block, pre-hashed with its arrival time.
--
-- All instances of this type will implement automatically:
--
-- * BlockFieldType & BlockTransactionType
-- * BlockMetadata
-- * BlockData
-- * BlockPendingData
-- * HashableTo BlockHash
data PendingBlock t = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !(BakedBlock t),
    pbReceiveTime :: !UTCTime
}

type instance BlockFieldType (PendingBlock t) = BlockFieldType (BakedBlock t)
type instance BlockTransactionType (PendingBlock t) = t

instance Eq (PendingBlock t) where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

instance Show (PendingBlock t) where
    show pb = show (pbHash pb) ++ " (" ++ show (blockBaker $ bbFields $ pbBlock pb) ++ ")"

instance BlockMetadata (PendingBlock t) where
    blockPointer = bfBlockPointer . bbFields . pbBlock
    {-# INLINE blockPointer #-}
    blockBaker = bfBlockBaker . bbFields . pbBlock
    {-# INLINE blockBaker #-}
    blockProof = bfBlockProof . bbFields . pbBlock
    {-# INLINE blockProof #-}
    blockNonce = bfBlockNonce . bbFields . pbBlock
    {-# INLINE blockNonce #-}
    blockLastFinalized = bfBlockLastFinalized . bbFields . pbBlock
    {-# INLINE blockLastFinalized #-}

instance (ToPut t) => BlockData (PendingBlock t) where
    blockSlot = blockSlot . pbBlock
    blockFields = blockFields . pbBlock
    blockTransactions = blockTransactions . pbBlock
    blockSignature = blockSignature . pbBlock
    blockBody = blockBody . pbBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE blockBody #-}

instance (ToPut t) => BlockPendingData (PendingBlock t) where
    blockReceiveTime = pbReceiveTime

instance HashableTo BlockHash (PendingBlock t) where
  getHash = pbHash
