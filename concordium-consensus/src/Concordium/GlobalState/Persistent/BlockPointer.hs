{-# LANGUAGE
        RecordWildCards,
        MultiParamTypeClasses,
        TypeFamilies,
        FlexibleInstances,
        RecursiveDo
        #-}
-- |An implementation of a BlockPointer that doesn't retain the parent or last finalized block so that they can be written into the disk and dropped from the memory.

module Concordium.GlobalState.Persistent.BlockPointer where

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Block
import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Transactions as Transactions
import Control.Exception
import Data.Hashable (Hashable, hashWithSalt, hash)
import qualified Data.List as List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import System.Mem.Weak

-- |A Block Pointer that doesn't retain the values for its Parent and Last finalized block
data PersistentBlockPointer s = PersistentBlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: Weak (PersistentBlockPointer s),
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: Weak (PersistentBlockPointer s),
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

instance Eq (PersistentBlockPointer s) where
    bp1 == bp2 = _bpHash bp1 == _bpHash bp2

instance Ord (PersistentBlockPointer s) where
    compare bp1 bp2 = compare (_bpHash bp1) (_bpHash bp2)

instance Hashable (PersistentBlockPointer s) where
    hashWithSalt s = hashWithSalt s . _bpHash
    hash = hash . _bpHash

instance Show (PersistentBlockPointer s) where
    show = show . _bpHash

instance HashableTo Hash.Hash (PersistentBlockPointer s) where
    getHash = _bpHash

type instance BlockFieldType (PersistentBlockPointer s) = BlockFields

instance BlockData (PersistentBlockPointer s) where
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

-- | Creates a block pointer using a Block. This version already consumes the `Weak` pointers
-- so it should not be used directly. Instead, `makeBlockPointerFromPendingBlock` or `makeBlockPointerFromBlock` should be used.
makeBlockPointer ::
    Block                                -- ^Pending block
    -> BlockHeight                        -- ^Height of the block
    -> Weak (PersistentBlockPointer s)    -- ^Parent block pointer
    -> Weak (PersistentBlockPointer s)    -- ^Last finalized block pointer
    -> s                                  -- ^Block state
    -> UTCTime                            -- ^Block arrival time
    -> UTCTime                            -- ^Receive time
    -> Maybe Energy                       -- ^Energy cost of all transactions in the block.
                                         --  If `Nothing` it will be computed in this function.
    -> PersistentBlockPointer s
makeBlockPointer b _bpHeight _bpParent _bpLastFinalized _bpState _bpArriveTime _bpReceiveTime ene =
  PersistentBlockPointer {
    _bpHash = getHash b,
    _bpBlock = b,
    ..}
 where (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.trSize tx + csize)) (0, 0) (blockTransactions b)
       _bpTransactionsEnergyCost = fromMaybe (List.foldl' (\en tx -> Transactions.transactionGasAmount tx + en) 0 (blockTransactions b)) ene

-- |Creates the genesis block pointer that has circular references to itself.
makeGenesisBlockPointer :: GenesisData -> s -> IO (PersistentBlockPointer s)
makeGenesisBlockPointer genData state = mdo
  let tm = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
  bp <- mkWeakPtr bp Nothing >>= (\parent ->
         mkWeakPtr bp Nothing >>= (\lfin ->
           return $ makeBlockPointer (makeGenesisBlock genData) 0 parent lfin state tm tm (Just 0)))
  return bp

-- |Creates a Block Pointer using a pending block
makeBlockPointerFromPendingBlock ::
    PendingBlock                  -- ^Pending block
    -> PersistentBlockPointer s    -- ^Parent block
    -> PersistentBlockPointer s    -- ^Last finalized block
    -> s                           -- ^Block state
    -> UTCTime                     -- ^Block arrival time
    -> Energy                      -- ^Energy cost of all transactions in the block
    -> IO (PersistentBlockPointer s)
makeBlockPointerFromPendingBlock pb parent lfin st arr ene = do
  parentW <- mkWeakPtr parent Nothing
  lfinW <- mkWeakPtr lfin Nothing
  return $ assert (getHash parent == blockPointer bf) $
    assert (getHash lfin == blockLastFinalized bf) $
    makeBlockPointer (NormalBlock (pbBlock pb)) (bpHeight parent + 1) parentW lfinW st arr (pbReceiveTime pb) (Just ene)
 where bf = bbFields $ pbBlock pb

-- |Creates a Block Pointer from a Block using a state and a height. This version results into an unlinked Block Pointer and is
-- intended to be used when we deserialize a specific block from the disk as we don't want to deserialize its parent or last finalized block.
makeBlockPointerFromBlock :: Block -> s -> BlockHeight -> IO (PersistentBlockPointer s)
makeBlockPointerFromBlock b s bh = do
  parentW <- mkWeakPtr undefined Nothing
  finalize parentW
  lfinW <- mkWeakPtr undefined Nothing
  finalize lfinW
  tm <- getCurrentTime
  return $ makeBlockPointer b bh parentW lfinW s tm tm Nothing

instance BlockPointerData (PersistentBlockPointer s) where
    bpHash = _bpHash
    bpHeight = _bpHeight
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount
    bpTransactionsEnergyCost = _bpTransactionsEnergyCost
    bpTransactionsSize = _bpTransactionsSize
