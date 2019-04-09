{-# LANGUAGE MultiParamTypeClasses #-}

module Concordium.GlobalState.Block where

import Data.ByteString
import Data.Serialize.Put
import Data.Serialize
import Data.Time.Clock
import Data.Hashable hiding (unhashed, hashed)

import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Types
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.HashableTo
import Concordium.GlobalState.BlockState

type BlockHash = Hash.Hash
type BlockProof = VRF.Proof
type BlockSignature = Sig.Signature
-- TODO: The hash is redundant; should be removed
type BlockNonce = (VRF.Hash, VRF.Proof)
type BlockData = ByteString

newtype BlockTransactions = BlockTransactions {transactionList :: [HashedTransaction]}

data Block
    = GenesisBlock Slot GenesisData
    | NormalBlock Slot BlockHash BakerId BlockProof BlockNonce BlockHash BlockTransactions BlockSignature

blockSlot :: Block -> Slot
blockSlot (GenesisBlock slot _) = slot
blockSlot (NormalBlock slot _ _ _ _ _ _ _) = slot

blockPointer :: Block -> BlockHash
blockPointer (NormalBlock _ parent _ _ _ _ _ _) = parent
blockPointer GenesisBlock{} = error "Genesis block has no block pointer"

blockBaker :: Block -> BakerId
blockBaker (NormalBlock _ _ baker _ _ _ _ _) = baker
blockBaker GenesisBlock{} = error "Genesis block has no baker"

blockProof :: Block -> BlockProof
blockProof (NormalBlock _ _ _ proof _ _ _ _) = proof
blockProof GenesisBlock{} = error "Genesis block has no block proof"

blockNonce :: Block -> BlockNonce
blockNonce (NormalBlock _ _ _ _ bnonce _ _ _) = bnonce
blockNonce GenesisBlock{} = error "Genesis block has no block nonce"

blockLastFinalized :: Block -> BlockHash
blockLastFinalized (NormalBlock _ _ _ _ _ lastFin _ _) = lastFin
blockLastFinalized GenesisBlock{} = error "Genesis block has no last finalized pointer"

blockTransactions :: Block -> [HashedTransaction]
blockTransactions GenesisBlock{} = []
blockTransactions (NormalBlock _ _ _ _ _ _ (BlockTransactions transactions) _) = transactions

blockBody :: Block -> Put
blockBody (GenesisBlock slot genData) = put slot >> put genData
blockBody (NormalBlock slot parent baker proof bnonce lastFin transactions _) = do
        put slot
        put parent
        put baker
        put proof
        put bnonce
        put lastFin
        put (unhashed <$> transactionList transactions)

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
            transactions <- BlockTransactions . fmap makeHashed <$> get
            sig <- get
            return $ NormalBlock sl parent baker proof bnonce lastFin transactions sig

verifyBlockSignature :: Sig.VerifyKey -> Block -> Bool
verifyBlockSignature _ GenesisBlock{} = True
verifyBlockSignature key b@(NormalBlock _ _ _ _ _ _ _ sig) = Sig.verify key bs sig
    where
        bs = runPut $ blockBody b

signBlock ::
    BakerSignPrivateKey
    -> Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> [HashedTransaction]
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

data BlockPointer = BlockPointer {
    bpHash :: !BlockHash,
    bpBlock :: !Block,
    bpParent :: BlockPointer,
    bpLastFinalized :: BlockPointer,
    bpHeight :: !BlockHeight,
    bpState :: !BlockState,
    -- |Time at which the block was first received
    bpReceiveTime :: UTCTime,
    -- |Time at which the block was first considered part of the tree (validated)
    bpArriveTime :: UTCTime,
    -- |Number of transactions in a block
    bpTransactionCount :: Int
}

instance Eq BlockPointer where
    bp1 == bp2 = bpHash bp1 == bpHash bp2

instance Ord BlockPointer where
    compare bp1 bp2 = compare (bpHash bp1) (bpHash bp2)

instance Hashable BlockPointer where
    hashWithSalt s = hashWithSalt s . bpHash
    hash = hash . bpHash

instance Show BlockPointer where
    show = show . bpHash

instance HashableTo Hash.Hash BlockPointer where
    getHash = bpHash