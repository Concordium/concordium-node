{-# LANGUAGE FlexibleContexts, NumericUnderscores, ScopedTypeVariables, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, StandaloneDeriving, DerivingVia, RecordWildCards, LambdaCase, TypeApplications #-}
-- |This module provides an abstraction over the operations done in the LMDB database that serves as a backend for storing blocks and finalization records.

module Concordium.GlobalState.Persistent.LMDB (
  DatabaseHandlers(..)
  , storeEnv
  , blockStore
  , finalizationRecordStore
  , transactionStatusStore
  , databaseHandlers
  , initializeDatabase
  , finalizedByHeightStore
  , LMDBStoreType (..)
  , LMDBStoreMonad (..)
  , LMDBQueryMonad (..)
  , putOrResize
  ) where

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.GlobalState.Types
import Concordium.Types
import qualified Concordium.Types.Transactions as T
import Concordium.Crypto.SHA256
import Concordium.Types.HashableTo
import Control.Exception (handleJust)
import Control.Monad.IO.Class
import Data.ByteString
import Data.Maybe(isJust)
import Data.Serialize as S (put, runPut, Put)
import Database.LMDB.Raw
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Directory
import qualified Data.HashMap.Strict as HM

-- |Values used by the LMDBStoreMonad to manage the database.
-- Sometimes we only want read access 
data DatabaseHandlers = DatabaseHandlers {
    _limits :: !Limits,
    _storeEnv :: !(Environment ReadWrite),
    _blockStore :: !(Database BlockHash ByteString),
    _finalizationRecordStore :: !(Database FinalizationIndex FinalizationRecord),
    _transactionStatusStore :: !(Database TransactionHash T.TransactionStatus),
    _finalizedByHeightStore :: !(Database BlockHeight BlockHash)
}
makeLenses ''DatabaseHandlers

blockStoreName, finalizationRecordStoreName, transactionStatusStoreName, finalizedByHeightStoreName :: String
blockStoreName = "blocks"
finalizationRecordStoreName = "finalization"
finalizedByHeightStoreName = "finalizedByHeight"
transactionStatusStoreName = "transactionstatus"

-- NB: The @ati@ is stored in an external database if chosen to.

-- |Initialize database handlers in ReadWrite mode.
-- This simply loads the references and does not initialize the databases.
databaseHandlers ::  RuntimeParameters -> IO DatabaseHandlers
databaseHandlers RuntimeParameters{..} = do
  let _limits = defaultLimits { mapSize = 2^(26 :: Int), maxDatabases = 4 } -- 64MB
  _storeEnv <- openEnvironment @ ReadWrite rpTreeStateDir _limits
  _blockStore <- transaction _storeEnv (getDatabase @ ReadWrite (Just blockStoreName))
  _finalizationRecordStore <- transaction _storeEnv (getDatabase @ ReadWrite (Just finalizationRecordStoreName))
  _finalizedByHeightStore <- transaction _storeEnv (getDatabase @ ReadWrite (Just finalizedByHeightStoreName))
  _transactionStatusStore <- transaction _storeEnv (getDatabase @ ReadWrite (Just transactionStatusStoreName))
  return DatabaseHandlers{..}

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initializeDatabase :: PersistentBlockPointer ati bs -> S.Put -> RuntimeParameters -> IO DatabaseHandlers
initializeDatabase gb serState rp@RuntimeParameters{..} = do
  -- The initial mapsize needs to be high enough to allocate the genesis block and its finalization record or
  -- initialization would fail. It also needs to be a multiple of the OS page size. We considered keeping 4096 as a typical
  -- OS page size and setting the initial mapsize to 64MB which is very unlikely to be reached just by the genesis block.
  createDirectoryIfMissing False rpTreeStateDir
  handlers@DatabaseHandlers{..} <- databaseHandlers rp
  let gbh = getHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  transaction _storeEnv $ do
    L.put _blockStore gbh (Just $ runPut (S.put (0 :: FinalizationIndex) <>
                                          S.put (_bpInfo gb) <>
                                          putBlock gb <>
                                          serState))
    L.put _finalizedByHeightStore 0 (Just gbh)
    L.put _finalizationRecordStore 0 (Just gbfin)
  return handlers

resizeDatabaseHandlers :: DatabaseHandlers -> Int -> IO DatabaseHandlers
resizeDatabaseHandlers dbh size = do
  let step = 2^(26 :: Int)
      delta = size + (step - size `mod` step)
      lim = dbh ^. limits
      newSize = mapSize lim + delta
      _limits = lim { mapSize = newSize }
      _storeEnv = dbh ^. storeEnv
  resizeEnvironment _storeEnv newSize
  _blockStore <- transaction _storeEnv (getDatabase (Just blockStoreName) :: Transaction ReadWrite (Database BlockHash ByteString))
  _finalizationRecordStore <- transaction _storeEnv (getDatabase (Just finalizationRecordStoreName) :: Transaction ReadWrite (Database FinalizationIndex FinalizationRecord))
  _transactionStatusStore <- transaction _storeEnv (getDatabase (Just transactionStatusStoreName) :: Transaction ReadWrite (Database TransactionHash T.TransactionStatus))
  _finalizedByHeightStore <- transaction _storeEnv (getDatabase (Just finalizedByHeightStoreName) :: Transaction ReadWrite (Database BlockHeight BlockHash))
  return DatabaseHandlers {..}

-- |For now the database supports four stores: blocks, finalization records, transaction statuses,
-- and hashes of finalized blocks indexed by height. This type abstracts access to those four stores.
--
-- * finalization records can be written separately.
-- * Blocks are written together with adding an index to the by-height store. This is written in a single database transaction.
-- * Transaction statuses can be written either individually, each in its own database transaction, or in one transaction.
--   The latter is preferrable for performance reasons.
data LMDBStoreType = Block !BlockHash !BlockHeight !ByteString
                   -- ^The Blockhash, block height and the serialized form of the block
                   | Finalization !FinalizationIndex !FinalizationRecord
                   -- ^The finalization index and the associated finalization record
                   | TxStatus !TransactionHash !T.TransactionStatus
                   -- ^Write a single transaction record.
                   | TxStatuses ![(TransactionHash, T.TransactionStatus)]
                   -- ^Write all transaction records in a single database transaction.
                   deriving (Show)

lmdbStoreTypeSize :: LMDBStoreType -> Int
lmdbStoreTypeSize (Block _ _ v) = digestSize + 8 + Data.ByteString.length v + 8 + digestSize
lmdbStoreTypeSize (Finalization _ v) = let FinalizationProof (vs, _)  = finalizationProof v in
  -- key + finIndex + finBlockPointer + finProof (list of Word32s + BlsSignature.signatureSize) + finDelay
  digestSize + 64 + digestSize + (32 * Prelude.length vs) + 48 + 64
lmdbStoreTypeSize (TxStatus _ t) = digestSize + 8 + case t of
  T.Committed _ res -> HM.size res * (digestSize + 8)
  T.Finalized{} -> digestSize + 8
  _ -> 0
lmdbStoreTypeSize (TxStatuses ss) = Prelude.length ss * (2 * digestSize + 16)

-- |Depending on the variant of the provided tuple, this function will perform a `put` transaction in the
-- correct database.
putInProperDB :: LMDBStoreType -> DatabaseHandlers -> IO ()
putInProperDB (Block bHash bHeight value) dbh = do
  let env = dbh ^. storeEnv
  transaction env $ do
    L.put (dbh ^. finalizedByHeightStore) bHeight (Just bHash)
    L.put (dbh ^. blockStore) bHash (Just value)
putInProperDB (Finalization key value) dbh =  do
  let env = dbh ^. storeEnv
  transaction env $ L.put (dbh ^. finalizationRecordStore) key (Just value)
putInProperDB (TxStatus key value) dbh =  do
  let env = dbh ^. storeEnv
  transaction env $ L.put (dbh ^. transactionStatusStore) key (Just value)
putInProperDB (TxStatuses statuses) dbh = do
  let env = dbh ^. storeEnv
  let putter (key, value) = L.put (dbh ^. transactionStatusStore) key (Just value)
  transaction env $ mapM_ putter statuses
  

-- |Provided default function that tries to perform an insertion in a given database of a given value,
-- altering the environment if needed when the database grows.
putOrResize :: MonadIO m => DatabaseHandlers -> LMDBStoreType -> m DatabaseHandlers
putOrResize dbh tup = liftIO $ handleJust selectDBFullError handleResize tryResizeDB
    where tryResizeDB = dbh <$ putInProperDB tup dbh
          -- only handle the db full error and propagate other exceptions.
          selectDBFullError = \case (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
                                    _ -> Nothing
          -- Resize the database handlers, and try to add again in case the size estimate
          -- given by lmdbStoreTypeSize is off.
          handleResize () = do
            dbh' <- resizeDatabaseHandlers dbh (lmdbStoreTypeSize tup)
            putOrResize dbh' tup


class (MonadIO m) => LMDBQueryMonad m where
   readBlock :: BlockHash -> m (Maybe (BlockPointerType m))
   readFinalizationRecord :: FinalizationIndex -> m (Maybe FinalizationRecord)
   readTransactionStatus :: TransactionHash -> m (Maybe T.TransactionStatus)
   -- |Read a finalized block at a given height, if it exists.
   readFinalizedBlockAtHeight :: BlockHeight -> m (Maybe (BlockPointerType m))

   -- |Check if the given key is in the on-disk transaction table. At the moment
   -- this has a default implementation in terms of 'readTransactionStatus', but
   -- this could be replaced with a more efficient variant which does not load
   -- the data from memory.
   memberTransactionTable :: TransactionHash -> m Bool
   memberTransactionTable = fmap isJust . readTransactionStatus

-- |Monad to abstract over the operations for writing to a LMDB database.
-- It provides functions for reading and writing Blocks and FinalizationRecords.
-- The databases should be indexed by the @BlockHash@ and the @FinalizationIndex@ in each case.
class (MonadIO m) => LMDBStoreMonad m where
  -- |Write a block that was finalized by the given finalization record.
  -- The finalization index is stored with the block.
  writeBlock :: BlockPointerType m -> FinalizationRecord -> m ()

  writeFinalizationRecord :: FinalizationRecord -> m ()

  writeTransactionStatus :: TransactionHash -> T.TransactionStatus -> m ()

  writeTransactionStatuses :: [(TransactionHash, T.TransactionStatus)] -> m ()
