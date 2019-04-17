{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}

module Concordium.GlobalState.Block where

import Control.Exception
import Data.Serialize.Put
import Data.Serialize
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Hashable hiding (unhashed, hashed)

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.GlobalState.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.BlockState

type BlockHash = Hash.Hash
type BlockProof = VRF.Proof
type BlockSignature = Sig.Signature
-- TODO: The hash is redundant; should be removed
type BlockNonce = (VRF.Hash, VRF.Proof)

newtype BlockTransactions = BlockTransactions {transactionList :: [HashedTransaction]}

class BlockData b where
    blockSlot :: b -> Slot
    blockPointer :: b -> BlockHash
    blockBaker :: b -> BakerId
    blockProof :: b -> BlockProof
    blockNonce :: b -> BlockNonce
    blockLastFinalized :: b -> BlockHash
    blockTransactions :: b -> [HashedTransaction]
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

instance BlockData PendingBlock where
    blockSlot = blockSlot . pbBlock
    blockPointer = blockPointer . pbBlock
    blockBaker = blockBaker . pbBlock
    blockProof = blockProof . pbBlock
    blockNonce = blockNonce . pbBlock
    blockLastFinalized = blockLastFinalized . pbBlock
    blockTransactions = blockTransactions . pbBlock
    verifyBlockSignature key = verifyBlockSignature key . pbBlock

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

instance BlockData BlockPointer where
    blockSlot = blockSlot . bpBlock
    blockPointer = blockPointer . bpBlock
    blockBaker = blockBaker . bpBlock
    blockProof = blockProof . bpBlock
    blockNonce = blockNonce . bpBlock
    blockLastFinalized = blockLastFinalized . bpBlock
    blockTransactions = blockTransactions . bpBlock
    verifyBlockSignature key = verifyBlockSignature key . bpBlock

-- |Make a 'BlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBlockPointer ::
    PendingBlock        -- ^Pending block
    -> BlockPointer     -- ^Parent block pointer
    -> BlockPointer     -- ^Last finalized block pointer
    -> BlockState       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> BlockPointer
makeBlockPointer pb@PendingBlock{..} bpParent bpLastFinalized bpState bpArriveTime
        = assert (getHash bpParent == blockPointer pb) $
            assert (getHash bpLastFinalized == blockLastFinalized pb) $
                BlockPointer {
                    bpHash = getHash pb,
                    bpBlock = pbBlock,
                    bpHeight = bpHeight bpParent + 1,
                    bpReceiveTime = pbReceiveTime,
                    bpTransactionCount = length (blockTransactions pb),
                    ..}

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = GenesisBlock 0 genData

makeGenesisBlockPointer :: GenesisData -> BlockState -> BlockPointer
makeGenesisBlockPointer genData bpState = theBlockPointer
    where
        theBlockPointer = BlockPointer {..}
        bpBlock = makeGenesisBlock genData
        bpHash = getHash bpBlock
        bpParent = theBlockPointer
        bpLastFinalized = theBlockPointer
        bpHeight = 0
        bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        bpArriveTime = bpReceiveTime
        bpTransactionCount = 0
