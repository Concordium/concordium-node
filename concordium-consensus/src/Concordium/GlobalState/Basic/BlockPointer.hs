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
    -- |Information about the block, e.g., height, transactions, ...
    _bpInfo :: !BasicBlockPointerData,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: BasicBlockPointer s,
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: BasicBlockPointer s,
      -- |The block itself
    _bpBlock :: !Block,
      -- |The handle for accessing the state (of accounts, contracts, etc.) after execution of the block.
    _bpState :: !s
}

instance Eq (BasicBlockPointer s) where
    bp1 == bp2 = _bpInfo bp1 == _bpInfo bp2

instance Ord (BasicBlockPointer s) where
    compare bp1 bp2 = compare (_bpInfo bp1) (_bpInfo bp2)

instance Hashable (BasicBlockPointer s) where
    hashWithSalt s = hashWithSalt s . _bpInfo
    hash = hash . _bpInfo

instance Show (BasicBlockPointer s) where
    show = show . _bpInfo

instance HashableTo Hash.Hash (BasicBlockPointer s) where
    getHash = getHash . _bpInfo

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
                    _bpInfo = BasicBlockPointerData{
                      _bpHash = getHash pb,
                      _bpHeight = bpHeight _bpParent + 1,
                      _bpReceiveTime = pbReceiveTime pb,
                      ..},
                      _bpBlock = NormalBlock (pbBlock pb),
                    ..}
    where
        bf = bbFields $ pbBlock pb
        (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.trSize tx + csize)) (0, 0) (blockTransactions pb)


makeGenesisBlockPointer :: GenesisData -> s -> BasicBlockPointer s
makeGenesisBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BasicBlockPointer {_bpInfo=BasicBlockPointerData{..},..}
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


instance BlockPointerData (BasicBlockPointer s) where
    bpHash = _bpHash . _bpInfo
    bpHeight = _bpHeight . _bpInfo
    bpReceiveTime = _bpReceiveTime . _bpInfo
    bpArriveTime = _bpArriveTime . _bpInfo
    bpTransactionCount = _bpTransactionCount . _bpInfo
    bpTransactionsEnergyCost = _bpTransactionsEnergyCost . _bpInfo
    bpTransactionsSize = _bpTransactionsSize . _bpInfo
