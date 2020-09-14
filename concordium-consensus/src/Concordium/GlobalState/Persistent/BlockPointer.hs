{-# LANGUAGE ScopedTypeVariables #-}
-- |An implementation of a BlockPointer that doesn't retain the parent or last finalized block so that they can be written into the disk and dropped from the memory.

module Concordium.GlobalState.Persistent.BlockPointer where

import Concordium.GlobalState.Block
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Transactions as Transactions
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Time.Clock
import System.Mem.Weak
import Data.Serialize
import Concordium.Logger

type PersistentBlockPointer ati = BlockPointer ati Weak

-- |Create an empty weak pointer
--
-- Creating a pointer that points to `undefined` with no finalizers and finalizing it
-- immediately, results in an empty pointer that always return `Nothing`
-- when dereferenced.
emptyWeak :: IO (Weak a)
emptyWeak = do
  pointer <- mkWeakPtr undefined Nothing
  finalize pointer
  return pointer

-- |Creates a persistent block pointer with the provided block and metadata. Should not be called directly.
makePersistentBlockPointer :: (Monad m) => Block                               -- ^Pending block
                           -> Maybe BlockHash                    -- ^Precomputed hash of this block. If not provided, it will be computed in-place.
                           -> BlockHeight                        -- ^Height of the block
                           -> Weak (PersistentBlockPointer ati bs)    -- ^Parent block pointer
                           -> Weak (PersistentBlockPointer ati bs)    -- ^Last finalized block pointer
                           -> BlockHash                          -- ^Last finalized block hash
                           -> bs                                 -- ^Block state
                           -> ati                                -- ^Account index table for this block
                           -> UTCTime                            -- ^Block arrival time
                           -> UTCTime                            -- ^Receive time
                           -> Maybe Int                          -- ^Transaction count (only non available if we are upgrading a pending block)
                           -> Maybe Int                          -- ^Transction size (only non available if we are upgrading a pending block)
                           -> Energy                       -- ^Energy cost of all transactions in the block. If not provided, it will be computed in-place.
                           -> m (PersistentBlockPointer ati bs)
makePersistentBlockPointer b hs _bpHeight _bpParent _bpLastFinalized _bpLastFinalizedHash _bpState _bpATI _bpArriveTime _bpReceiveTime txcount txsize _bpTransactionsEnergyCost = do
  let _bpHash = maybe (getHash b) id hs
  return $ BlockPointer {
      _bpInfo = BasicBlockPointerData{..},
      _bpBlock = b,
      ..}
 where (_bpTransactionCount, _bpTransactionsSize) =
         case (txcount, txsize) of
           (Just x, Just y) -> (x, y)
           _ -> List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.biSize tx + csize)) (0, 0) (blockTransactions b)

-- |Creates the genesis persistent block pointer that has empty references to parent and last finalized.
--
-- The genesis block used to have circular references but doing recursive circular references in a recursive
-- do block with Weak pointers results in the pointer being deallocated immediately so instead of doing that
-- we will just put empty pointers there. In any case, when reading those values, if the block is the genesis
-- block we don't even deref these pointers so they can be empty.
makeGenesisPersistentBlockPointer :: (MonadIO m) =>
                                    GenesisData
                                  -> bs
                                  -> ati
                                  -> m (PersistentBlockPointer ati bs)
makeGenesisPersistentBlockPointer genData _bpState _bpATI = liftIO $ do
  let _bpReceiveTime = timestampToUTCTime (genesisTime genData)
      b = GenesisBlock genData
      _bpHash = BlockHashV0 . Hash.hashLazy . runPutLazy $ put genesisSlot <> put genData
  _bpParent <- emptyWeak
  _bpLastFinalized <- emptyWeak
  return $ BlockPointer {
      _bpInfo = BasicBlockPointerData{
          _bpHeight = 0,
          _bpTransactionsEnergyCost = 0,
          _bpTransactionCount = 0,
          _bpTransactionsSize = 0,
          _bpArriveTime = _bpReceiveTime,
          _bpLastFinalizedHash = _bpHash,
          ..},
      _bpBlock = b,
      ..}

-- |Converts a Pending Block into a PersistentBlockPointer
makePersistentBlockPointerFromPendingBlock :: forall m ati bs. (MonadLogger m, MonadIO m) =>
                                   PendingBlock      -- ^Pending block
                                 -> PersistentBlockPointer ati bs  -- ^Parent block
                                 -> PersistentBlockPointer ati bs  -- ^Last finalized block
                                 -> bs                             -- ^Block state
                                 -> ati                         -- ^Account transaction index table for this block
                                 -> UTCTime                     -- ^Block arrival time
                                 -> Energy                      -- ^Energy cost of all transactions in the block
                                 -> m (PersistentBlockPointer ati bs)
makePersistentBlockPointerFromPendingBlock pb parent lfin st ati arr ene = do
  (parentW, lfinW) <- liftIO $ do
    parentW <- mkWeakPtr parent Nothing
    lfinW <- mkWeakPtr lfin Nothing
    return (parentW, lfinW)
  let block = pbBlock pb
      bf = bbFields block
  if getHash parent == blockPointer bf
    then
    makePersistentBlockPointer (NormalBlock block) (Just $ getHash pb) (bpHeight parent + 1) parentW lfinW (bpHash lfin) st ati arr (pbReceiveTime pb) Nothing Nothing ene
    else do
    logErrorAndThrow GlobalState $ "The hash of the given parent block (" ++ show (getHash parent :: BlockHash) ++ ") and the hash in the block metadata (" ++ show (blockPointer bf) ++ ") don't match"

-- | Create an unlinked persistent block pointer
makeBlockPointerFromPersistentBlock :: (MonadIO m) =>
                                      Block                      -- ^Block deserialized as retrieved from the disk
                                    -> bs                        -- ^Block state
                                    -> ati                       -- ^Account index table for this block
                                    -> BasicBlockPointerData     -- ^Block info
                                    -> m (PersistentBlockPointer ati bs)
makeBlockPointerFromPersistentBlock _bpBlock _bpState _bpATI _bpInfo = liftIO $ do
  _bpParent <- emptyWeak
  _bpLastFinalized <- emptyWeak
  return $ BlockPointer {..}
