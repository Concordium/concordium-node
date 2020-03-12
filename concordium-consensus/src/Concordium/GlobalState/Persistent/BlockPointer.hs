{-# LANGUAGE
        MultiParamTypeClasses,
        TypeFamilies,
        FlexibleInstances,
        FlexibleContexts,
        ScopedTypeVariables,
        RecordWildCards
        #-}
-- |An implementation of a BlockPointer that doesn't retain the parent or last finalized block so that they can be written into the disk and dropped from the memory.

module Concordium.GlobalState.Persistent.BlockPointer where

import Concordium.GlobalState.Block
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Block
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.PersistentTransactions
import qualified Concordium.Types.Transactions as T
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Time.Clock
import Concordium.GlobalState.Classes
import Data.Time.Clock.POSIX
import System.Mem.Weak
import Data.Serialize

type PersistentBlockPointer ati s = BlockPointer ati PersistentTransaction Weak s

-- |Creates a persistent block pointer with the provided block and metadata. Should not be called directly.
makePersistentBlockPointer :: forall m s ati. (Convert T.Transaction PersistentTransaction m) =>
                             PersistentBlock                    -- ^Pending block
                           -> Maybe BlockHash                    -- ^Precomputed hash of this block. If not provided, it will be computed in-place.
                           -> BlockHeight                        -- ^Height of the block
                           -> Weak (PersistentBlockPointer ati s)    -- ^Parent block pointer
                           -> Weak (PersistentBlockPointer ati s)    -- ^Last finalized block pointer
                           -> s                                  -- ^Block state
                           -> ati                                -- ^Account index table for this block
                           -> UTCTime                            -- ^Block arrival time
                           -> UTCTime                            -- ^Receive time
                           -> Maybe Int                          -- ^Transaction count (only non available if we are upgrading a pending block)
                           -> Maybe Int                          -- ^Transction size (only non available if we are upgrading a pending block)
                           -> Energy                       -- ^Energy cost of all transactions in the block. If not provided, it will be computed in-place.
                           -> m (PersistentBlockPointer ati s)
makePersistentBlockPointer b _bpHash _bpHeight _bpParent _bpLastFinalized _bpState _bpATI _bpArriveTime _bpReceiveTime txcount txsize _bpTransactionsEnergyCost = do
  _bpHash <- maybe (getHash b) return _bpHash
  return $ BlockPointer {
      _bpInfo = BasicBlockPointerData{..},
      _bpBlock = b,
      ..}
 where (_bpTransactionCount, _bpTransactionsSize) = case (txcount, txsize) of
         (Just txcnt, Just txsz) -> (txcnt, txsz)
         _ -> List.foldl' (\(clen, csize) tx -> (clen + 1, ptrSize tx + csize)) (0, 0) (blockTransactions b)

-- |Creates the genesis persistent block pointer that has empty references to parent and last finalized.
--
-- The genesis block used to have circular references but doing recursive circular references in a recursive
-- do block with Weak pointers results in the pointer being deallocated immediately so instead of doing that
-- we will just put empty pointers there. In any case, when reading those values, if the block is the genesis
-- block we don't even deref these pointers so they can be empty.
makeGenesisPersistentBlockPointer :: (MonadIO m) =>
                                    GenesisData
                                  -> s
                                  -> ati
                                  -> m (PersistentBlockPointer ati s)
makeGenesisPersistentBlockPointer genData _bpState _bpATI = do
  let _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
      b = GenesisBlock genData
      _bpHash = Hash.hashLazy . runPutLazy $ put genesisSlot >> put genData
  _bpParent <- liftIO emptyWeak
  _bpLastFinalized <- liftIO emptyWeak
  return $ BlockPointer {
      _bpInfo = BasicBlockPointerData{
          _bpHeight = 0,
          _bpTransactionsEnergyCost = 0,
          _bpTransactionCount = 0,
          _bpTransactionsSize = 0,
          _bpArriveTime = _bpReceiveTime,
          ..},
      _bpBlock = b,
      ..}

-- |Converts a Pending Block into a PersistentBlockPointer
makePersistentBlockPointerFromPendingBlock :: (MonadIO m,
                                     BlockDataMonad PersistentBlock m) =>
                                   PersistentPendingBlock      -- ^Pending block
                                 -> PersistentBlockPointer ati s    -- ^Parent block
                                 -> PersistentBlockPointer ati s    -- ^Last finalized block
                                 -> s                           -- ^Block state
                                 -> ati                         -- ^Account transaction index table for this block
                                 -> UTCTime                     -- ^Block arrival time
                                 -> Energy                      -- ^Energy cost of all transactions in the block
                                 -> m (PersistentBlockPointer ati s)
makePersistentBlockPointerFromPendingBlock pb parent lfin st ati arr ene = do
  parentW <- liftIO $ mkWeakPtr parent Nothing
  lfinW <- liftIO $ mkWeakPtr lfin Nothing
  let block = pbBlock pb
      bf = bbFields block
  assert (getHash parent == blockPointer bf) $
    assert (getHash lfin == blockLastFinalized bf) $
    makePersistentBlockPointer (NormalBlock block) (Just $ getHash pb) (bpHeight parent + 1) parentW lfinW st ati arr (pbReceiveTime pb) Nothing Nothing ene

-- | Create an unlinked persistent block pointer
makeBlockPointerFromPersistentBlock :: (MonadIO m, Convert T.Transaction PersistentTransaction m) =>
                                      PersistentBlock            -- ^Block deserialized as retrieved from the disk
                                    -> s                         -- ^Block state
                                    -> ati                       -- ^Account index table for this block
                                    -> BlockHash                 -- ^Hash of this block
                                    -> BlockHeight               -- ^Height of the block
                                    -> Int                       -- ^Transaction count
                                    -> Int                       -- ^Transaction size
                                    -> Energy                    -- ^Transaction execution cost
                                    -> m (PersistentBlockPointer ati s)
makeBlockPointerFromPersistentBlock b s ati hs bh txcount txsize ene = do
  parentW <- liftIO emptyWeak
  lfinW <- liftIO emptyWeak
  tm <- liftIO getCurrentTime
  makePersistentBlockPointer b (Just hs) bh parentW lfinW s ati tm tm (Just txcount) (Just txsize) ene
