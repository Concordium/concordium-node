{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies, FlexibleContexts, TypeSynonymInstances #-}
module Concordium.GlobalState.Basic.Block where

import Data.Serialize.Put
import Data.Serialize
import Data.Time.Clock

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.Block

-- * Block implementations

-- |A list of transactions.
newtype BlockTransactions = BlockTransactions {transactionList :: [Transaction]} deriving (Show)

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
data BakedBlock = BakedBlock {
    -- |Slot number (must be >0)
    bbSlot :: !Slot,
    -- |Block fields
    bbFields :: !BlockFields,
    -- |Block transactions
    bbTransactions :: !BlockTransactions,
    -- |Block signature
    bbSignature :: BlockSignature
} deriving (Show)

type instance BlockFieldType BakedBlock = BlockFields

instance BlockMetadata BakedBlock where
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

instance BlockData BakedBlock where
    blockSlot = bbSlot
    {-# INLINE blockSlot #-}
    blockFields = Just . bbFields
    {-# INLINE blockFields #-}
    blockTransactions = transactionList . bbTransactions
    {-# INLINE blockTransactions #-}
    verifyBlockSignature key b = Sig.verify key (runPut $ blockBody b) (bbSignature b)
    {-# INLINE verifyBlockSignature #-}
    putBlock = putBlock . NormalBlock
    {-# INLINE putBlock #-}

instance HashableTo BlockHash BakedBlock where
    getHash = getHash . NormalBlock

-- |Generate a baked block.
signBlock ::
    BakerSignPrivateKey -- ^Key for signing the new block
    -> Slot             -- ^Block slot (must be non-zero)
    -> BlockHash        -- ^Hash of parent block
    -> BakerId          -- ^Identifier of block baker
    -> BlockProof       -- ^Block proof
    -> BlockNonce       -- ^Block nonce
    -> BlockHash        -- ^Hash of last finalized block
    -> [Transaction]    -- ^List of transactions
    -> BakedBlock
signBlock key slot parent baker proof bnonce lastFin transactions
    | slot == 0 = error "Only the genesis block may have slot 0"
    | otherwise = block
    where
        trs = BlockTransactions transactions
        preBlock = BakedBlock slot (BlockFields parent baker proof bnonce lastFin) trs
        sig = Sig.sign key $ runPut $ blockBody (preBlock undefined)
        block = preBlock sig

-- |Representation of a block
data Block
    = GenesisBlock !GenesisData
    -- ^A genesis block
    | NormalBlock !BakedBlock
    -- ^A baked (i.e. non-genesis) block
    deriving (Show)

type instance BlockFieldType Block = BlockFields

instance BlockData Block where
    blockSlot GenesisBlock{} = 0
    blockSlot (NormalBlock bb) = blockSlot bb

    blockFields GenesisBlock{} = Nothing
    blockFields (NormalBlock bb) = blockFields bb

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock bb) = blockTransactions bb

    verifyBlockSignature _ GenesisBlock{} = True
    verifyBlockSignature key (NormalBlock bb) = verifyBlockSignature key bb

    putBlock (GenesisBlock genData) = put genesisSlot >> put genData
    putBlock (NormalBlock bb) = blockBody bb >> put (bbSignature bb)

    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE verifyBlockSignature #-}
    {-# INLINE putBlock #-}

getBlock :: TransactionTime -> Get Block
getBlock arrivalTime = do
  sl <- get
  if sl == 0 then do
    genData <- get
    return (GenesisBlock genData)
  else do
    bfBlockPointer <- get
    bfBlockBaker <- get
    bfBlockProof <- get
    bfBlockNonce <- get
    bfBlockLastFinalized <- get
    bbTransactions <- BlockTransactions <$> getListOf (getVerifiedTransaction arrivalTime)
    bbSignature <- get
    return $ NormalBlock (BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..})

instance HashableTo BlockHash Block where
    getHash = Hash.hashLazy . runPutLazy . putBlock

-- |A baked block, pre-hashed with its arrival time.
data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !BakedBlock,
    pbReceiveTime :: !UTCTime
}
instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

instance HashableTo Hash.Hash PendingBlock where
    getHash = pbHash

type instance BlockFieldType PendingBlock = BlockFields

instance BlockData PendingBlock where
    blockSlot = blockSlot . pbBlock
    blockFields = blockFields . pbBlock
    blockTransactions = blockTransactions . pbBlock
    verifyBlockSignature key = verifyBlockSignature key . pbBlock
    putBlock = putBlock . pbBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE verifyBlockSignature #-}
    {-# INLINE putBlock #-}

instance Show PendingBlock where
    show pb = show (pbHash pb) ++ " (" ++ show (blockBaker $ bbFields $ pbBlock pb) ++ ")"

instance BlockMetadata PendingBlock where
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

instance BlockPendingData PendingBlock where
    blockReceiveTime = pbReceiveTime

makePendingBlock :: BakedBlock -> UTCTime -> PendingBlock
makePendingBlock pbBlock pbReceiveTime = PendingBlock{pbHash = getHash pbBlock,..}

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = GenesisBlock genData
