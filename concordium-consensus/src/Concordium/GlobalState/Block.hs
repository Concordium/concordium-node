{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies, FlexibleContexts, TypeSynonymInstances, FunctionalDependencies #-}

module Concordium.GlobalState.Block where

import Data.Time.Clock
import Data.Serialize

import qualified Concordium.Crypto.BlockSignature as Sig
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.HashableTo

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

-- |The 'BlockData' class provides an interface for the data associated
-- with a block.
class BlockMetadata (BlockFieldType b) => BlockData b where
    -- |The slot number of the block (0 for genesis block)
    blockSlot :: b -> Slot
    -- |The fields of a block, if it was baked; @Nothing@ for the genesis block.
    blockFields :: b -> Maybe (BlockFieldType b)
    -- |The list of transactions in the block (empty for genesis block)
    blockTransactions :: b -> [Transaction]
    -- |Determine if the block is signed by the given key
    -- (always 'True' for genesis block)
    verifyBlockSignature :: Sig.VerifyKey -> b -> Bool
    -- |Serialize the block.
    putBlock :: b -> Put

class (BlockMetadata b, BlockData b, HashableTo BlockHash b, Show b) => BlockPendingData b where
    -- |Time at which the block was received
    blockReceiveTime :: b -> UTCTime

-- |Serialize the body of a baked (non-genesis) block.
-- The block must be a baked block since its type is an
-- instance of 'BlockMetadata'; its slot number must
-- therefore be non-zero.
blockBody :: (BlockMetadata b, BlockData b) => b -> Put
{-# INLINE blockBody #-}
blockBody b = do
        put (blockSlot b)
        put (blockPointer b)
        put (blockBaker b)
        put (blockProof b)
        put (blockNonce b)
        put (blockLastFinalized b)
        put (map trBareTransaction $ blockTransactions b)


class (Eq bp, Show bp, BlockData bp) => BlockPointerData bs bp | bp -> bs where
    -- |Hash of the block
    bpHash :: bp -> BlockHash
    -- |Pointer to the parent (circular reference for genesis block)
    bpParent :: bp -> bp
    -- |Pointer to the last finalized block (circular for genesis)
    bpLastFinalized :: bp -> bp
    -- |Height of the block in the tree
    bpHeight :: bp -> BlockHeight
    -- |The handle for accessing the state (of accounts, contracts, etc.) at the end of the block.
    bpState :: bp -> bs
    -- |Time at which the block was first received
    bpReceiveTime :: bp -> UTCTime
    -- |Time at which the block was first considered part of the tree (validated)
    bpArriveTime :: bp -> UTCTime
    -- |Number of transactions in a block
    bpTransactionCount :: bp -> Int
    -- |Energy cost of all transactions in the block.
    bpTransactionsEnergyCost :: bp -> Energy
    -- |Size of the transaction data in bytes.
    bpTransactionsSize :: bp -> Int
