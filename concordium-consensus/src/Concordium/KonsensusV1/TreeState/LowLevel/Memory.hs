{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImportQualifiedPost #-}

-- |This module provides a simple in-memory version of the low-level tree state.
module Concordium.KonsensusV1.TreeState.LowLevel.Memory where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Map.Strict as Map

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

data LowLevelDB pv = LowLevelDB
    { lldbBlockHashes :: !(HM.HashMap BlockHash BlockHeight),
      lldbBlocks :: !(Map.Map BlockHeight (StoredBlock pv)),
      lldbTransactions :: !(HM.HashMap TransactionHash FinalizedTransactionStatus),
      lldbLatestFinalizationEntry :: !(Maybe FinalizationEntry),
      lldbRoundStatus :: !RoundStatus
    }

class HasMemoryLLDB pv r | r -> pv where
    theMemoryLLDB :: r -> IORef (LowLevelDB pv)

readLLDB :: (MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => m (LowLevelDB pv)
readLLDB = liftIO . readIORef =<< asks theMemoryLLDB

withLLDB :: (MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => (LowLevelDB pv -> (LowLevelDB pv, a)) -> m a
withLLDB f = do
    ref <- asks theMemoryLLDB
    liftIO $ atomicModifyIORef' ref f

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
    lookupFirstBlock =
        readLLDB <&> fmap snd . Map.lookupMin . lldbBlocks
    lookupLastBlock =
        readLLDB <&> fmap snd . Map.lookupMax . lldbBlocks
    lookupBlockByHeight h =
        readLLDB <&> Map.lookup h . lldbBlocks
    lookupTransaction th =
        readLLDB <&> HM.lookup th . lldbTransactions
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
            Nothing -> return (Right False)
            Just sb -> do
                ok <- predicate sb
                if ok
                    then return (Right False)
                    else do
                        withLLDB $ \db -> (db{lldbLatestFinalizationEntry = Nothing}, ())
                        roll sb
                        return (Right True)
      where
        roll sb = do
            withLLDB $ \db@LowLevelDB{..} ->
                ( db
                    { lldbBlocks = Map.delete (bmHeight (stbInfo sb)) lldbBlocks,
                      lldbBlockHashes = HM.delete (getHash sb) lldbBlockHashes,
                      lldbTransactions = foldl' deleteTx lldbTransactions (blockTransactions sb)
                    },
                  ()
                )
            lookupLastBlock >>= \case
                Nothing -> return ()
                Just sb' -> do
                    ok <- predicate sb'
                    unless ok $ roll sb'
        deleteTx txs tx = HM.delete (getHash tx) txs
