{-# LANGUAGE
        RecordWildCards,
        MultiParamTypeClasses,
        TypeFamilies,
        FlexibleInstances
        #-}
module Concordium.GlobalState.Basic.BlockPointer where

import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as List
import Control.Exception

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.Basic.Block
import qualified Concordium.Types.Transactions as Transactions


data BasicBlockPointer s = BasicBlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: BasicBlockPointer s,
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: BasicBlockPointer s,
    -- |Height of the block in the tree
    _bpHeight :: !BlockHeight,
    -- |The handle for accessing the state (of accounts, contracts, etc.) after execution of the block.
    _bpState :: !s,
    -- |Time at which the block was first received
    _bpReceiveTime :: UTCTime,
    -- |Time at which the block was first considered part of the tree (validated)
    _bpArriveTime :: UTCTime,
    -- |Number of transactions in a block
    _bpTransactionCount :: !Int,
    -- |Energy cost of all transactions in the block.
    _bpTransactionsEnergyCost :: !Energy,
    -- |Size of the transaction data in bytes.
    _bpTransactionsSize :: !Int
}

instance Eq (BasicBlockPointer s) where
    bp1 == bp2 = _bpHash bp1 == _bpHash bp2

instance Ord (BasicBlockPointer s) where
    compare bp1 bp2 = compare (_bpHash bp1) (_bpHash bp2)

instance Hashable (BasicBlockPointer s) where
    hashWithSalt s = hashWithSalt s . _bpHash
    hash = hash . _bpHash

instance Show (BasicBlockPointer s) where
    show = show . _bpHash

instance HashableTo Hash.Hash (BasicBlockPointer s) where
    getHash = _bpHash

type instance BlockFieldType (BasicBlockPointer s) = BlockFields

instance BlockData (BasicBlockPointer s) where
    blockSlot = blockSlot . _bpBlock
    blockFields = blockFields . _bpBlock
    blockTransactions = blockTransactions . _bpBlock
    verifyBlockSignature key = verifyBlockSignature key . _bpBlock
    putBlock = putBlock . _bpBlock
    {-# INLINE blockSlot #-}
    {-# INLINE blockFields #-}
    {-# INLINE blockTransactions #-}
    {-# INLINE verifyBlockSignature #-}
    {-# INLINE putBlock #-}


-- |Make a 'BasicBlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBasicBlockPointer ::
    PendingBlock        -- ^Pending block
    -> BasicBlockPointer s    -- ^Parent block pointer
    -> BasicBlockPointer s    -- ^Last finalized block pointer
    -> s       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> Energy           -- ^Energy cost of all transactions in the block
    -> BasicBlockPointer s
makeBasicBlockPointer pb _bpParent _bpLastFinalized _bpState _bpArriveTime _bpTransactionsEnergyCost
        = assert (getHash _bpParent == blockPointer bf) $
            assert (getHash _bpLastFinalized == blockLastFinalized bf) $
                BasicBlockPointer {
                    _bpHash = getHash pb,
                    _bpBlock = NormalBlock (pbBlock pb),
                    _bpHeight = _bpHeight _bpParent + 1,
                    _bpReceiveTime = pbReceiveTime pb,
                    ..}
    where
        bf = bbFields $ pbBlock pb
        (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.trSize tx + csize)) (0, 0) (blockTransactions pb)


makeGenesisBlockPointer :: GenesisData -> s -> BasicBlockPointer s
makeGenesisBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BasicBlockPointer {..}
        _bpBlock = makeGenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = theBlockPointer
        _bpLastFinalized = theBlockPointer
        _bpHeight = 0
        _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0
        _bpTransactionsEnergyCost = 0
        _bpTransactionsSize = 0


instance BlockPointerData s (BasicBlockPointer s) where
    bpHash = _bpHash
    bpParent = _bpParent
    bpLastFinalized = _bpLastFinalized
    bpHeight = _bpHeight
    bpState = _bpState
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount
    bpTransactionsEnergyCost = _bpTransactionsEnergyCost
    bpTransactionsSize = _bpTransactionsSize
