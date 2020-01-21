{-# LANGUAGE FlexibleContexts, NumericUnderscores, ScopedTypeVariables, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, StandaloneDeriving, DerivingVia, RecordWildCards #-}
-- |This module provides an abstraction over the operations done in the LMDB database that serves as a backend for storing blocks and finalization records.

module Concordium.GlobalState.Persistent.LMDB where

import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString
import Data.Serialize as S (Serialize, put, runPut)
import Database.LMDB.Raw
import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Directory

-- |Values used by the LMDBStoreMonad to manage the database
data DatabaseHandlers bs = DatabaseHandlers {
    _limits :: Limits,
    _storeEnv :: Environment ReadWrite,
    _blockStore :: Database BlockHash ByteString,
    _finalizationRecordStore :: Database FinalizationIndex FinalizationRecord
}
makeLenses ''DatabaseHandlers

-- |A standard location for the database
defaultLocation :: IO FilePath
defaultLocation = do
  dir <- (++ "/treestate") <$> getCurrentDirectory
  createDirectoryIfMissing False dir
  return dir

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initialDatabaseHandlers :: PersistentBlockPointer bs -> ByteString -> IO (DatabaseHandlers bs)
initialDatabaseHandlers gb serState = liftIO $ do
  let _limits = defaultLimits { mapSize = 64_000_000, maxDatabases = 2 } -- 64MB
  dir <- defaultLocation
  _storeEnv <- openEnvironment dir _limits
  _blockStore <- transaction _storeEnv $
    (getDatabase (Just "blocks") :: L.Transaction ReadWrite (Database BlockHash ByteString))
  _finalizationRecordStore <- transaction _storeEnv $
    (getDatabase (Just "finalization") :: L.Transaction ReadWrite (Database FinalizationIndex FinalizationRecord))
  let gbh = getHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  transaction _storeEnv $ L.put _blockStore (getHash gb) (Just $ runPut (putBlock gb >> S.put serState >> S.put (BlockHeight 0)))
  transaction _storeEnv $ L.put _finalizationRecordStore 0 (Just gbfin)
  return $ DatabaseHandlers {..}

-- |Monad to abstract over the operations for reading and writing from a LMDB database. It provides functions for reading and writing Blocks and FinalizationRecords.
-- The databases should be indexed by the @BlockHash@ and the @FinalizationIndex@ in each case.
class (MonadIO m) => LMDBStoreMonad m where
   -- |Provided default function that tries to perform an insertion in a given database of a given value,
   -- altering the environment if needed when the database grows.
   putOrResize :: (Serialize k, Serialize v) => Limits -> String -> Environment ReadWrite -> Database k v -> k -> v -> m (Limits, Environment ReadWrite, Database k v)
   putOrResize lim name env db k v = liftIO $ catch (do
                                              transaction env $ L.put db k (Just v)
                                              return $ (lim, env, db))
                                    (\(e :: LMDB_Error) -> case e of
                                        LMDB_Error _ _ (Right MDB_MAP_FULL) -> do
                                          dir <- defaultLocation
                                          let lim' = lim { mapSize = mapSize lim + 64_000_000 }
                                          env' <- openEnvironment dir lim'
                                          db' <- transaction env' $ (getDatabase (Just name) :: Transaction ReadWrite (Database k v))
                                          transaction env' $ L.put db' k (Just v)
                                          return $ (lim', env', db')
                                        _ -> error $ show e
                                    )

   writeBlock :: (BlockPointer m) -> m ()

   readBlock :: BlockHash -> m (Maybe (BlockPointer m))

   writeFinalizationRecord :: FinalizationRecord -> m ()

   readFinalizationRecord :: FinalizationIndex -> m (Maybe FinalizationRecord)
