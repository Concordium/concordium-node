{-# LANGUAGE FlexibleContexts, NumericUnderscores, ScopedTypeVariables, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, StandaloneDeriving, DerivingVia, RecordWildCards #-}
-- |This module provides an abstraction over the operations done in the LMDB database that serves as a backend for storing blocks and finalization records.

module Concordium.GlobalState.Persistent.LMDB (
  DatabaseHandlers (..)
  , storeEnv
  , storeLock
  , blockStore
  , finalizationRecordStore
  , initialDatabaseHandlers
  , LMDBStoreType (..)
  , LMDBStoreMonad (..)
  , putOrResize
  , readTransaction
  ) where

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString
import Data.Serialize as S (put, runPut, Put)
import Database.LMDB.Raw
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Directory
import Debug.Trace
import Control.Concurrent.ReadWriteLock (RWLock)
import qualified Control.Concurrent.ReadWriteLock as RWL

-- |Values used by the LMDBStoreMonad to manage the database
data DatabaseHandlers bs = DatabaseHandlers {
    _limits :: Limits,
    _storeLock :: RWLock,
    _storeEnv :: Environment ReadWrite,
    _blockStore :: Database BlockHash ByteString,
    _finalizationRecordStore :: Database FinalizationIndex FinalizationRecord
}
makeLenses ''DatabaseHandlers

blockStoreName :: String
blockStoreName = "blocks"
finalizationRecordStoreName :: String
finalizationRecordStoreName = "finalization"

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initialDatabaseHandlers :: PersistentBlockPointer bs -> S.Put -> RuntimeParameters -> IO (DatabaseHandlers bs)
initialDatabaseHandlers gb serState RuntimeParameters{..} = liftIO $ do
  let _limits = defaultLimits { mapSize = 16 * 4096, maxDatabases = 2 } -- 64MB
  createDirectoryIfMissing False rpTreeStateDir
  _storeEnv <- openEnvironment rpTreeStateDir _limits
  _blockStore <- transaction _storeEnv
    (getDatabase (Just "blocks") :: L.Transaction ReadWrite (Database BlockHash ByteString))
  _finalizationRecordStore <- transaction _storeEnv
    (getDatabase (Just "finalization") :: L.Transaction ReadWrite (Database FinalizationIndex FinalizationRecord))
  let gbh = getHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  transaction _storeEnv $ L.put _blockStore (getHash gb) (Just $ runPut (putBlock gb <> serState <> S.put (BlockHeight 0)))
  transaction _storeEnv $ L.put _finalizationRecordStore 0 (Just gbfin)
  _storeLock <- RWL.new
  return $ DatabaseHandlers {..}

readTransaction l e t = RWL.withRead l (transaction e t)

resizeDatabaseHandlers :: DatabaseHandlers bs -> IO (DatabaseHandlers bs)
resizeDatabaseHandlers dbh = do
  let _limits = dbh ^. limits
      newSize = mapSize _limits + 16 * 4096
      dbB = dbh ^. blockStore
      dbF = dbh ^. finalizationRecordStore
      _storeEnv = dbh ^. storeEnv
      _storeLock = dbh ^. storeLock
  closeDatabase dbB
  closeDatabase dbF
  RWL.withWrite _storeLock (resizeEnvironment _storeEnv newSize)
  _blockStore <- readTransaction _storeLock _storeEnv (getDatabase (Just blockStoreName) :: Transaction ReadWrite (Database BlockHash ByteString))
  _finalizationRecordStore <- readTransaction _storeLock _storeEnv (getDatabase (Just finalizationRecordStoreName) :: Transaction ReadWrite (Database FinalizationIndex FinalizationRecord))
  return DatabaseHandlers {..}

-- |For now the database only supports two stores: blocks and finalization records.
-- In order to abstract the database access, this datatype was created.
-- When implementing `putOrResize` a tuple will need to be created and `putInProperDB` will choose the correct database.
data LMDBStoreType = Block (BlockHash, ByteString) -- ^The Blockhash and the serialized form of the block
               | Finalization (FinalizationIndex, FinalizationRecord) -- ^The finalization index and the associated finalization record
               deriving (Show)

putInProperDB :: LMDBStoreType -> DatabaseHandlers bs -> IO ()
putInProperDB (Block (key, value)) dbh = do
  let env = dbh ^. storeEnv
      l = dbh ^. storeLock
  readTransaction l env $ L.put (dbh ^. blockStore) key (Just value)
putInProperDB (Finalization (key, value)) dbh =  do
  let env = dbh ^. storeEnv
      l = dbh ^. storeLock
  readTransaction l env $ L.put (dbh ^. finalizationRecordStore) key (Just value)

-- |Provided default function that tries to perform an insertion in a given database of a given value,
-- altering the environment if needed when the database grows.
putOrResize :: MonadIO m => DatabaseHandlers bs -> LMDBStoreType -> m (DatabaseHandlers bs)
putOrResize dbh tup = liftIO $ catch (do
                                         putInProperDB tup dbh
                                         return dbh)
                                    (\(e :: LMDB_Error) -> case e of
                                        LMDB_Error _ _ (Right MDB_MAP_FULL) -> do

                                          traceM . ("Before " ++) =<< do
                                            let L.Env e = dbh ^. storeEnv
                                            show . me_mapsize <$> mdb_env_info e

                                          dbh' <- resizeDatabaseHandlers dbh

                                          traceM . ("After " ++) =<< do
                                            let L.Env e = dbh ^. storeEnv
                                            show . me_mapsize <$> mdb_env_info e

                                          putOrResize dbh' tup
                                        _ -> error $ show e)


-- |Monad to abstract over the operations for reading and writing from a LMDB database. It provides functions for reading and writing Blocks and FinalizationRecords.
-- The databases should be indexed by the @BlockHash@ and the @FinalizationIndex@ in each case.
class (MonadIO m) => LMDBStoreMonad m where
   writeBlock :: BlockPointer m -> m ()

   readBlock :: BlockHash -> m (Maybe (BlockPointer m))

   writeFinalizationRecord :: FinalizationRecord -> m ()

   readFinalizationRecord :: FinalizationIndex -> m (Maybe FinalizationRecord)
