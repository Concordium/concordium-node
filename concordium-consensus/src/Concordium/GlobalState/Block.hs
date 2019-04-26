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

newtype BlockTransactions = BlockTransactions {transactionList :: [Transaction]}

class BlockData b where
    -- |The slot number of the block (0 for genesis block)
    blockSlot :: b -> Slot
    -- |The 'BlockHash' of the parent block (undefined for genesis block)
    blockPointer :: b -> BlockHash
    -- |The identity of the block baker (undefined for genesis block)
    blockBaker :: b -> BakerId
    -- |The proof that the baker was entitled (undefined for genesis block)
    blockProof :: b -> BlockProof
    -- |The block nonce (undefined for genesis block)
    blockNonce :: b -> BlockNonce
    -- |The 'BlockHash' of the last finalized block when the block was baked
    -- (undefined for genesis block)
    blockLastFinalized :: b -> BlockHash
    -- |The list of transactions in the block (empty for genesis block)
    blockTransactions :: b -> [Transaction]
    -- |Determine if the block is signed by the given key
    -- (always 'True' for genesis block)
    verifyBlockSignature :: Sig.VerifyKey -> b -> Bool

data Block
    = GenesisBlock Slot GenesisData
    | NormalBlock Slot BlockHash BakerId BlockProof BlockNonce BlockHash BlockTransactions BlockSignature

instance BlockData Block where
    blockSlot (GenesisBlock slot _) = slot
    blockSlot (NormalBlock slot _ _ _ _ _ _ _) = slot

    blockPointer (NormalBlock _ parent _ _ _ _ _ _) = parent
    blockPointer GenesisBlock{} = error "Genesis block has no block pointer"

    blockBaker (NormalBlock _ _ baker _ _ _ _ _) = baker
    blockBaker GenesisBlock{} = error "Genesis block has no baker"

    blockProof (NormalBlock _ _ _ proof _ _ _ _) = proof
    blockProof GenesisBlock{} = error "Genesis block has no block proof"

    blockNonce (NormalBlock _ _ _ _ bnonce _ _ _) = bnonce
    blockNonce GenesisBlock{} = error "Genesis block has no block nonce"

    blockLastFinalized (NormalBlock _ _ _ _ _ lastFin _ _) = lastFin
    blockLastFinalized GenesisBlock{} = error "Genesis block has no last finalized pointer"

    blockTransactions GenesisBlock{} = []
    blockTransactions (NormalBlock _ _ _ _ _ _ (BlockTransactions transactions) _) = transactions

    verifyBlockSignature _ GenesisBlock{} = True
    verifyBlockSignature key b@(NormalBlock _ _ _ _ _ _ _ sig) = Sig.verify key bs sig
        where
            bs = runPut $ blockBody b


blockBody :: Block -> Put
blockBody (GenesisBlock slot genData) = put slot >> put genData
blockBody (NormalBlock slot parent baker proof bnonce lastFin transactions _) = do
        put slot
        put parent
        put baker
        put proof
        put bnonce
        put lastFin
        put (transactionList transactions)

instance Serialize Block where
    put b@GenesisBlock{} = blockBody b
    put b@(NormalBlock _ _ _ _ _ _ _ sig) = blockBody b >> put sig
    get = do
        sl <- get
        if sl == 0 then do
            genData <- get
            return (GenesisBlock sl genData)
        else do
            parent <- get
            baker <- get
            proof <- get
            bnonce <- get
            lastFin <- get
            transactions <- BlockTransactions <$> get
            sig <- get
            return $ NormalBlock sl parent baker proof bnonce lastFin transactions sig

signBlock ::
    BakerSignPrivateKey
    -> Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> [Transaction]
    -> Block
signBlock key slot parent baker proof bnonce lastFin transactions
    | slot == 0 = error "Only the genesis block may have slot 0"
    | otherwise = block
    where
        trs = BlockTransactions transactions
        preBlock = NormalBlock slot parent baker proof bnonce lastFin trs
        sig = Sig.sign key $ runPut $ blockBody (preBlock undefined)
        block = preBlock sig

instance HashableTo Hash.Hash Block where
    getHash = Hash.hashLazy . runPutLazy . put

data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !Block,
    pbReceiveTime :: !UTCTime
}
instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

instance HashableTo Hash.Hash PendingBlock where
    getHash = pbHash

instance BlockData PendingBlock where
    blockSlot = blockSlot . pbBlock
    blockPointer = blockPointer . pbBlock
    blockBaker = blockBaker . pbBlock
    blockProof = blockProof . pbBlock
    blockNonce = blockNonce . pbBlock
    blockLastFinalized = blockLastFinalized . pbBlock
    blockTransactions = blockTransactions . pbBlock
    verifyBlockSignature key = verifyBlockSignature key . pbBlock

instance Show PendingBlock where
    show = show . pbHash

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = GenesisBlock 0 genData
