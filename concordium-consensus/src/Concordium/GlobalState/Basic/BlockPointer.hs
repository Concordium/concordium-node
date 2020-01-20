{-# LANGUAGE
        RecordWildCards,
        MultiParamTypeClasses,
        TypeFamilies,
        FlexibleInstances,
        RecursiveDo
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
import System.Mem.Weak

data BasicBlockPointer s = BasicBlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: Weak (BasicBlockPointer s),
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: Weak (BasicBlockPointer s),
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

makeBlockPointer ::
    Block        -- ^Pending block
    -> BlockHeight
    -> Weak (BasicBlockPointer s)    -- ^Parent block pointer
    -> Weak (BasicBlockPointer s)    -- ^Last finalized block pointer
    -> s       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> UTCTime          -- ^Receive time
    -> Maybe Energy           -- ^Energy cost of all transactions in the block
    -> BasicBlockPointer s
makeBlockPointer b _bpHeight _bpParent _bpLastFinalized _bpState _bpArriveTime _bpReceiveTime ene =
  BasicBlockPointer {
    _bpHash = getHash b,
    _bpBlock = b,
    ..}
 where (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.trSize tx + csize)) (0, 0) (blockTransactions b)
       _bpTransactionsEnergyCost = case ene of
         Just v -> v
         Nothing -> List.foldl' (\(en) tx -> Transactions.transactionGasAmount tx + en) 0 (blockTransactions b)

makeGenesisBlockPointer :: GenesisData -> s -> IO (BasicBlockPointer s)
makeGenesisBlockPointer genData state = mdo
  let tm = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
  bp <- mkWeakPtr bp Nothing >>= (\parent ->
         mkWeakPtr bp Nothing >>= (\lfin ->
           return $ makeBlockPointer (makeGenesisBlock genData) 0 parent lfin state tm tm (Just 0)))
  return bp

makeBlockPointerFromPendingBlock ::
    PendingBlock        -- ^Pending block
    -> BasicBlockPointer s    -- ^Parent block pointer
    -> BasicBlockPointer s    -- ^Last finalized block pointer
    -> s       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> Energy           -- ^Energy cost of all transactions in the block
    -> IO (BasicBlockPointer s)
makeBlockPointerFromPendingBlock pb parent lfin st arr ene = do
  parentW <- mkWeakPtr parent Nothing
  lfinW <- mkWeakPtr lfin Nothing
  return $ assert (getHash parent == blockPointer bf) $
    assert (getHash lfin == blockLastFinalized bf) $
    makeBlockPointer (NormalBlock (pbBlock pb)) (bpHeight parent + 1) parentW lfinW st arr (pbReceiveTime pb) (Just ene)
 where bf = bbFields $ pbBlock pb

makeBlockPointerFromBlock :: Block -> s -> BlockHeight -> IO (BasicBlockPointer s)
makeBlockPointerFromBlock b s bh = do
  parentW <- mkWeakPtr undefined Nothing
  finalize parentW
  lfinW <- mkWeakPtr undefined Nothing
  finalize lfinW
  tm <- getCurrentTime
  return $ makeBlockPointer b bh parentW lfinW s tm tm Nothing

instance BlockPointerData (BasicBlockPointer s) where
    bpHash = _bpHash
    bpParent p = (\(Just x) -> return x) =<< (deRefWeak $ _bpParent p) --FIXME: This should read from the disk
    bpLastFinalized p = (\(Just x) -> return x) =<< (deRefWeak $ _bpLastFinalized p) --FIXME: This should read from the disk
    bpHeight = _bpHeight
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount
    bpTransactionsEnergyCost = _bpTransactionsEnergyCost
    bpTransactionsSize = _bpTransactionsSize
