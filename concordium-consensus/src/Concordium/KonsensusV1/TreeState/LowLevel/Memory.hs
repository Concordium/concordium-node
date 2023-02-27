{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module provides a simple in-memory version of the low-level tree state.
-- This is intended for testing purposes, for instance, where the overhead of creating an LMDB
-- database is excessive.
module Concordium.KonsensusV1.TreeState.LowLevel.Memory where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |A low-level tree state database. This manages the storage and indexing of blocks and
-- transactions, as well as recording persisted state of the consensus in the form of the latest
-- finalization entry and current round status.
data LowLevelDB pv = LowLevelDB
    { -- |Index of blocks by hash.
      lldbBlockHashes :: !(HM.HashMap BlockHash BlockHeight),
      -- |Table of blocks by height.
      lldbBlocks :: !(Map.Map BlockHeight (StoredBlock pv)),
      -- |Table of transactions by hash.
      lldbTransactions :: !(HM.HashMap TransactionHash FinalizedTransactionStatus),
      -- |The last finalization entry (if any).
      lldbLatestFinalizationEntry :: !(Maybe FinalizationEntry),
      -- |The current round status.
      lldbRoundStatus :: !RoundStatus
    }

-- |An initial 'LowLevelDB' with the supplied genesis block and round status, but otherwise with
-- no blocks, no transactions and no finalization entry.
-- The genesis block should have height 0; this is not checked.
initialLowLevelDB :: StoredBlock pv -> RoundStatus -> LowLevelDB pv
initialLowLevelDB genBlock roundStatus =
    LowLevelDB
        { lldbBlockHashes = HM.singleton (getHash genBlock) 0,
          lldbBlocks = Map.singleton 0 genBlock,
          lldbTransactions = HM.empty,
          lldbLatestFinalizationEntry = Nothing,
          lldbRoundStatus = roundStatus
        }

-- |The class 'HasMemoryLLDB' is implemented by a context in which a 'LowLevelDB' state is
-- maintained in an 'IORef'. This provides access to the low-level database when the monad implements
-- @MonadReader r@ and @MonadIO@.
class HasMemoryLLDB pv r | r -> pv where
    theMemoryLLDB :: r -> IORef (LowLevelDB pv)

-- |Helper for reading the low level DB.
readLLDB :: (MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => m (LowLevelDB pv)
readLLDB = liftIO . readIORef =<< asks theMemoryLLDB

-- |Helper for updating the low level DB.
withLLDB :: (MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => (LowLevelDB pv -> (LowLevelDB pv, a)) -> m a
withLLDB f = do
    ref <- asks theMemoryLLDB
    liftIO $ atomicModifyIORef' ref f

-- |A newtype wrapper that provides an instance of 'MonadTreeStateStore' where the underlying monad
-- provides a context for accessing the low-level state. That is, it implements @MonadIO@ and
-- @MonadReader r@ for @r@ with @HasMemoryLLDB pv r@.
newtype MemoryLLDBM (pv :: ProtocolVersion) m a = MemoryLLDBM {runMemoryLLDBM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadReader r m => MonadReader r (MemoryLLDBM pv m)

instance IsProtocolVersion pv => MonadProtocolVersion (MemoryLLDBM pv m) where
    type MPV (MemoryLLDBM pv m) = pv

instance (IsProtocolVersion pv, MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => MonadTreeStateStore (MemoryLLDBM pv m) where
    lookupBlock bh =
        readLLDB <&> \db -> do
            height <- HM.lookup bh $ lldbBlockHashes db
            Map.lookup height $ lldbBlocks db
    memberBlock = fmap isJust . lookupBlock
    lookupFirstBlock =
        readLLDB <&> fmap snd . Map.lookupMin . lldbBlocks
    lookupLastBlock =
        readLLDB <&> fmap snd . Map.lookupMax . lldbBlocks
    lookupBlockByHeight h =
        readLLDB <&> Map.lookup h . lldbBlocks
    lookupTransaction th =
        readLLDB <&> HM.lookup th . lldbTransactions
    memberTransaction = fmap isJust . lookupTransaction
    writeBlocks blocks fe =
        withLLDB $ (,()) . updateFinEntry . flip (foldl' insertBlock) blocks
      where
        updateFinEntry db = db{lldbLatestFinalizationEntry = Just fe}
        insertBlock db@LowLevelDB{..} sb =
            db
                { lldbBlocks = Map.insert height sb lldbBlocks,
                  lldbBlockHashes = HM.insert (getHash sb) height lldbBlockHashes,
                  lldbTransactions = foldl' insertTx lldbTransactions (zip (blockTransactions sb) [0 ..])
                }
          where
            height = bmHeight (stbInfo sb)
            insertTx txs (tx, ti) = HM.insert (getHash tx) (FinalizedTransactionStatus height ti) txs
    lookupLatestFinalizationEntry =
        readLLDB <&> lldbLatestFinalizationEntry
    lookupCurrentRoundStatus =
        readLLDB <&> lldbRoundStatus
    writeCurrentRoundStatus rs =
        withLLDB $ \db -> (db{lldbRoundStatus = rs}, ())
    rollBackBlocksUntil predicate =
        lookupLastBlock >>= \case
            Nothing -> return (Right 0)
            Just sb -> do
                ok <- predicate sb
                if ok
                    then return (Right 0)
                    else do
                        withLLDB $ \db -> (db{lldbLatestFinalizationEntry = Nothing}, ())
                        Right <$> roll sb 0
      where
        roll !sb !ctr = do
            -- Delete the block from the database and
            -- its associated transactions.
            withLLDB $ \db@LowLevelDB{..} ->
                ( db
                    { lldbBlocks = Map.delete (bmHeight (stbInfo sb)) lldbBlocks,
                      lldbBlockHashes = HM.delete (getHash sb) lldbBlockHashes,
                      lldbTransactions = foldl' deleteTx lldbTransactions (blockTransactions sb)
                    },
                  ()
                )
            -- Get the new last block and continue rolling if
            -- such one exist, otherwise return how many blocks
            -- we have rolled back.
            lookupLastBlock >>= \case
                Nothing -> return ctr
                Just !sb' -> do
                    -- Stop if we're at the block we wish to roll
                    -- roll back to otherwise continue.
                    predicate sb' >>= \case
                        True -> return ctr
                        False -> roll sb' $! ctr + 1
        deleteTx txs tx = HM.delete (getHash tx) txs
