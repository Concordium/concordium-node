{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}

module Concordium.GlobalState.Block where

import Data.Serialize.Put
import Data.Serialize
import Data.Time.Clock


import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.GlobalState.Transactions
import Concordium.Types.HashableTo

newtype BlockTransactions = BlockTransactions {transactionList :: [Transaction]} deriving (Show)

-- |The fields of a baked block.
data BlockFields = BlockFields {
    -- |The 'BlockHash' of the parent block
    blockPointer :: !BlockHash,
    -- |The identity of the block baker
    blockBaker :: !BakerId,
    -- |The proof that the baker was entitled to bake this block
    blockProof :: !BlockProof,
    -- |The block nonce
    blockNonce :: !BlockNonce,
    -- |The 'BlockHash' of the last finalized block when the block was baked
    blockLastFinalized :: !BlockHash
} deriving (Show)

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

blockBody :: BakedBlock -> Put
blockBody BakedBlock{bbFields=BlockFields{..},..} = do
        put bbSlot
        put blockPointer
        put blockBaker
        put blockProof
        put blockNonce
        put blockLastFinalized
        put (transactionList bbTransactions)

class BlockData b where
    -- |The slot number of the block (0 for genesis block)
    blockSlot :: b -> Slot
    -- |The fields of a block, if it was baked; @Nothing@ for the genesis block.
    blockFields :: b -> Maybe BlockFields
    -- |The list of transactions in the block (empty for genesis block)
    blockTransactions :: b -> [Transaction]
    -- |Determine if the block is signed by the given key
    -- (always 'True' for genesis block)
    verifyBlockSignature :: Sig.VerifyKey -> b -> Bool

instance BlockData BakedBlock where
    blockSlot = bbSlot
    blockFields = Just . bbFields
    blockTransactions = transactionList . bbTransactions
    verifyBlockSignature key b = Sig.verify key (runPut $ blockBody b) (bbSignature b)

instance HashableTo Hash.Hash BakedBlock where
    getHash = getHash . NormalBlock

data Block
    = GenesisBlock !GenesisData
    | NormalBlock !BakedBlock
    deriving (Show)

instance BlockData Block where
    blockSlot GenesisBlock{} = 0
    blockSlot (NormalBlock bb) = blockSlot bb

    blockFields GenesisBlock{} = Nothing
    blockFields (NormalBlock bb) = blockFields bb

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock bb) = blockTransactions bb

    verifyBlockSignature _ GenesisBlock{} = True
    verifyBlockSignature key (NormalBlock bb) = verifyBlockSignature key bb

instance Serialize Block where
    put (GenesisBlock genData) = put genesisSlot >> put genData
    put (NormalBlock bb) = blockBody bb >> put (bbSignature bb)
    get = do
        sl <- get
        if sl == 0 then do
            genData <- get
            return (GenesisBlock genData)
        else do
            blockPointer <- get
            blockBaker <- get
            blockProof <- get
            blockNonce <- get
            blockLastFinalized <- get
            bbTransactions <- BlockTransactions <$> get
            bbSignature <- get
            return $ NormalBlock (BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..})

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

instance HashableTo Hash.Hash Block where
    getHash = Hash.hashLazy . runPutLazy . put

data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !BakedBlock,
    pbReceiveTime :: !UTCTime
}
instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

instance HashableTo Hash.Hash PendingBlock where
    getHash = pbHash

instance BlockData PendingBlock where
    blockSlot = blockSlot . pbBlock
    blockFields = blockFields . pbBlock
    blockTransactions = blockTransactions . pbBlock
    verifyBlockSignature key = verifyBlockSignature key . pbBlock

instance Show PendingBlock where
    show = show . pbHash

makePendingBlock :: BakedBlock -> UTCTime -> PendingBlock
makePendingBlock pbBlock pbReceiveTime = PendingBlock{pbHash = getHash pbBlock,..}

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = GenesisBlock genData
