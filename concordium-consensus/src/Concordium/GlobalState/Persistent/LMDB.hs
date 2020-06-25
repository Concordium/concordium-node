{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- |This module provides an abstraction over the operations done in the LMDB database that serves as a backend for storing blocks and finalization records.

module Concordium.GlobalState.Persistent.LMDB (
  DatabaseHandlers(..)
  , HasDatabaseHandlers(..)
  , FinalizedTransactionStatus(..)
  , finalizedToTransactionStatus
  , storeEnv
  , blockStore
  , finalizationRecordStore
  , transactionStatusStore
  , databaseHandlers
  , initializeDatabase
  , closeDatabase
  , finalizedByHeightStore
  , StoredBlock(..)
  , readBlock
  , readFinalizationRecord
  , readTransactionStatus
  , readFinalizedBlockAtHeight
  , memberTransactionTable
  , loadBlocksFinalizationIndexes
  , getLastBlock
  , getFirstBlock
  , writeBlock
  , writeFinalizationRecord
  , writeTransactionStatus
  , writeTransactionStatuses
  ) where

import Concordium.GlobalState.LMDB.Helpers
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.BlockPointer
import Concordium.Types
import Concordium.Types.Execution (TransactionIndex)
import qualified Concordium.Types.Transactions as T
import Concordium.Crypto.SHA256
import Concordium.Types.HashableTo
import Control.Concurrent (runInBoundThread)
import Control.Exception (tryJust)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory
import qualified Data.HashMap.Strict as HM
import Concordium.Logger


data StoredBlock st = StoredBlock {
  sbFinalizationIndex :: !FinalizationIndex,
  sbInfo :: !BasicBlockPointerData,
  sbBlock :: !Block,
  sbState :: !st
}

instance S.Serialize st => S.Serialize (StoredBlock st) where
  put StoredBlock{..} = S.put sbFinalizationIndex <>
          S.put sbInfo <>
          putBlock sbBlock <>
          S.put sbState
  get = do
          sbFinalizationIndex <- S.get
          sbInfo <- S.get
          sbBlock <- getBlock (T.utcTimeToTransactionTime (_bpReceiveTime sbInfo))
          sbState <- S.get
          return StoredBlock{..}

newtype BlockStore st = BlockStore MDB_dbi'

instance S.Serialize st => MDBDatabase (BlockStore st) where
  type DBKey (BlockStore st) = BlockHash
  type DBValue (BlockStore st) = StoredBlock st
  encodeKey _ = hashToByteString

newtype FinalizationRecordStore = FinalizationRecordStore MDB_dbi'

instance MDBDatabase FinalizationRecordStore where
  type DBKey FinalizationRecordStore = FinalizationIndex
  type DBValue FinalizationRecordStore = FinalizationRecord

newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

data FinalizedTransactionStatus = FinalizedTransactionStatus {
  ftsSlot :: !Slot,
  ftsBlockHash :: !BlockHash,
  ftsIndex :: !TransactionIndex
} deriving (Eq, Show)

instance S.Serialize FinalizedTransactionStatus where
  put FinalizedTransactionStatus{..} = S.put ftsSlot >> S.put ftsBlockHash >> S.put ftsIndex
  get = FinalizedTransactionStatus <$> S.get <*> S.get <*> S.get

-- |Convert a 'FinalizedTransactionStatus' to a 'TransactionStatus'
finalizedToTransactionStatus :: FinalizedTransactionStatus -> T.TransactionStatus
finalizedToTransactionStatus FinalizedTransactionStatus{..} = 
  T.Finalized{_tsSlot = ftsSlot, tsBlockHash = ftsBlockHash, tsFinResult = ftsIndex}

instance MDBDatabase TransactionStatusStore where
  type DBKey TransactionStatusStore = TransactionHash
  type DBValue TransactionStatusStore = FinalizedTransactionStatus
  encodeKey _ = hashToByteString . v0TransactionHash

newtype FinalizedByHeightStore = FinalizedByHeightStore MDB_dbi'

instance MDBDatabase FinalizedByHeightStore where
  type DBKey FinalizedByHeightStore = BlockHeight
  type DBValue FinalizedByHeightStore = BlockHash
  encodeValue _ = LBS.fromStrict . hashToByteString

-- |Values used by the LMDBStoreMonad to manage the database.
-- Sometimes we only want read access
data DatabaseHandlers st = DatabaseHandlers {
    _dbMapSize :: !Int,
    _storeEnv :: !MDB_env,
    _blockStore :: !(BlockStore st),
    _finalizationRecordStore :: !FinalizationRecordStore,
    _transactionStatusStore :: !TransactionStatusStore,
    _finalizedByHeightStore :: !FinalizedByHeightStore
}
makeLenses ''DatabaseHandlers

class HasDatabaseHandlers st s | s -> st where
  dbHandlers :: Lens' s (DatabaseHandlers st)

blockStoreName, finalizationRecordStoreName, transactionStatusStoreName, finalizedByHeightStoreName :: String
blockStoreName = "blocks"
finalizationRecordStoreName = "finalization"
finalizedByHeightStoreName = "finalizedByHeight"
transactionStatusStoreName = "transactionstatus"

dbStepSize :: Int
dbStepSize = 2^(26 :: Int) -- 64MB

dbInitSize :: Int
dbInitSize = dbStepSize


-- NB: The @ati@ is stored in an external database if chosen to.

-- |Initialize database handlers in ReadWrite mode.
-- This simply loads the references and does not initialize the databases.
databaseHandlers ::  RuntimeParameters -> IO (DatabaseHandlers st)
databaseHandlers RuntimeParameters{..} = do
  let _dbMapSize = dbInitSize
  _storeEnv <- mdb_env_create
  mdb_env_set_mapsize _storeEnv _dbMapSize
  mdb_env_set_maxdbs _storeEnv 4
  mdb_env_set_maxreaders _storeEnv 126
  -- TODO: Consider MDB_NOLOCK
  mdb_env_open _storeEnv rpTreeStateDir []
  transaction _storeEnv False $ \txn -> do
    _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) [MDB_CREATE]
    _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) [MDB_CREATE]
    _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) [MDB_CREATE]
    _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) [MDB_CREATE]
    return DatabaseHandlers{..}

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initializeDatabase :: (S.Serialize st) => PersistentBlockPointer ati bs -> st -> RuntimeParameters -> IO (DatabaseHandlers st)
initializeDatabase gb stRef rp@RuntimeParameters{..} = do
  -- The initial mapsize needs to be high enough to allocate the genesis block and its finalization record or
  -- initialization would fail. It also needs to be a multiple of the OS page size. We considered keeping 4096 as a typical
  -- OS page size and setting the initial mapsize to 64MB which is very unlikely to be reached just by the genesis block.
  createDirectoryIfMissing False rpTreeStateDir
  handlers@DatabaseHandlers{..} <- databaseHandlers rp
  let gbh = getHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  transaction _storeEnv False $ \txn -> do
    storeRecord txn _finalizedByHeightStore 0 gbh
    storeRecord txn _blockStore gbh StoredBlock {
                    sbFinalizationIndex = 0,
                    sbInfo = _bpInfo gb,
                    sbBlock = _bpBlock gb,
                    sbState = stRef
                  }
    storeRecord txn _finalizedByHeightStore 0 gbh
    storeRecord txn _finalizationRecordStore 0 gbfin
  return handlers

closeDatabase :: DatabaseHandlers st -> IO ()
closeDatabase db = runInBoundThread $ mdb_env_close (db ^. storeEnv)

resizeDatabaseHandlers :: (MonadIO m, MonadLogger m) => DatabaseHandlers st -> Int -> m (DatabaseHandlers st)
resizeDatabaseHandlers dbh size = do
  let delta = size + (dbStepSize - size `mod` dbStepSize)
      _dbMapSize = (dbh ^. dbMapSize) + delta
      _storeEnv = dbh ^. storeEnv
  logEvent LMDB LLDebug $ "Resizing database from " ++ show (dbh ^. dbMapSize) ++ " to " ++ show _dbMapSize
  liftIO $ do
    mdb_env_set_mapsize _storeEnv _dbMapSize
    transaction _storeEnv False $ \txn -> do
      _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) [MDB_CREATE]
      _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) [MDB_CREATE]
      _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) [MDB_CREATE]
      _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) [MDB_CREATE]
      return DatabaseHandlers{..}

-- |Read a block from the database by hash.
readBlock :: (MonadIO m, MonadState s m, HasDatabaseHandlers st s, S.Serialize st)
  => BlockHash
  -> m (Maybe (StoredBlock st))
readBlock bh = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. blockStore) bh

-- |Read a finalization record from the database by finalization index.
readFinalizationRecord :: (MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => FinalizationIndex
  -> m (Maybe FinalizationRecord)
readFinalizationRecord finIndex = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. finalizationRecordStore) finIndex

-- |Read the status of a transaction from the database by the transaction hash.
readTransactionStatus :: (MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => TransactionHash
  -> m (Maybe FinalizedTransactionStatus)
readTransactionStatus txHash = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. transactionStatusStore) txHash

-- |Read a block from the database by its height.
readFinalizedBlockAtHeight :: (MonadIO m, MonadState s m, HasDatabaseHandlers st s, S.Serialize st)
  => BlockHeight
  -> m (Maybe (StoredBlock st))
readFinalizedBlockAtHeight bHeight = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> do
        mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) bHeight
        join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash

-- |Check if the given key is in the on-disk transaction table.        
memberTransactionTable :: (MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => TransactionHash
  -> m Bool
memberTransactionTable th = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> isRecordPresent txn (dbh ^. transactionStatusStore) th

-- |Build a table of a block finalization indexes for blocks.
loadBlocksFinalizationIndexes :: S.Serialize st => DatabaseHandlers st -> IO (Either String (HM.HashMap BlockHash FinalizationIndex))
loadBlocksFinalizationIndexes dbh = transaction (dbh ^. storeEnv) True $ \txn ->
    withPrimitiveCursor txn (mdbDatabase $ dbh ^. blockStore) $ \cursor -> do
      let
          loop Nothing cur = return (Right cur)
          loop (Just (keyv, valv)) cur =
            S.decode <$> byteStringFromMDB_val keyv >>= \case
              Left s -> return $ Left $ "Failed reading block hash while loading blocks: " ++ s
              Right bh -> S.runGet S.get <$> byteStringFromMDB_val valv >>= \case
                Left s -> return $ Left $ "Failed loading block finalization index for block " ++ show bh ++ ": " ++ s
                Right finIndex -> do
                  let nxt = HM.insert bh finIndex cur
                  nxtRes <- getPrimitiveCursor CursorNext cursor
                  loop nxtRes nxt
      fstRes <- getPrimitiveCursor CursorFirst cursor
      loop fstRes HM.empty

-- |Get the last finalized block by block height.
getLastBlock :: S.Serialize st => DatabaseHandlers st -> IO (Either String (FinalizationRecord, StoredBlock st))
getLastBlock dbh = transaction (dbh ^. storeEnv) True $ \txn -> do
    mLastFin <- withCursor txn (dbh ^. finalizationRecordStore) $ getCursor CursorLast
    case mLastFin of
      Just (Right (_, finRec)) ->
        loadRecord txn (dbh ^. blockStore) (finalizationBlockPointer finRec) >>= \case
          Just block -> return $ Right (finRec, block)
          Nothing -> return $ Left "Could not read last finalized block"
      Just (Left s) -> return $ Left $ "Could not read last finalization record: " ++ s
      Nothing -> return $ Left "No last finalized block found"

-- |Get the first block 
getFirstBlock :: (S.Serialize st) => DatabaseHandlers st -> IO (Maybe (StoredBlock st))
getFirstBlock dbh = transaction (dbh ^. storeEnv) True $ \txn -> do
        mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) 0
        join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash


-- |Perform a database action that may require the database to be resized,
-- resizing the database if necessary.
resizeOnFull :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => Int
  -- ^Additional size
  -> (DatabaseHandlers st -> IO a)
  -- ^Action that may require resizing
  -> m a
resizeOnFull addSize a = do
    dbh <- use dbHandlers
    (dbh', res) <- inner dbh
    dbHandlers .= dbh'
    return res
  where
    inner dbh = do
      r <- liftIO $ tryJust selectDBFullError ((dbh, ) <$> a dbh)
      case r of
        Left _ -> do
          -- Resize the database handlers, and try to add again in case the size estimate
          -- given by lmdbStoreTypeSize is off.
          dbh' <- resizeDatabaseHandlers dbh addSize
          inner dbh'
        Right res -> return res
    -- only handle the db full error and propagate other exceptions.
    selectDBFullError = \case (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
                              _ -> Nothing

writeBlock :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers st s, S.Serialize st)
  => StoredBlock st -> m ()
writeBlock block = resizeOnFull blockSize
    $ \dbh -> transaction (dbh ^. storeEnv) False 
    $ \txn -> do
        let b = sbInfo block
        storeReplaceRecord txn (dbh ^. finalizedByHeightStore) (_bpHeight b) (_bpHash b)
        storeReplaceRecord txn (dbh ^. blockStore) (_bpHash b) block
  where
    blockSize = 2*digestSize + fromIntegral (LBS.length (S.encodeLazy block))

writeFinalizationRecord :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => FinalizationRecord -> m ()
writeFinalizationRecord finRec = resizeOnFull finRecSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn ->
        storeReplaceRecord txn (dbh ^. finalizationRecordStore) (finalizationIndex finRec) finRec
  where
    finRecSize = let FinalizationProof (vs, _)  = finalizationProof finRec in
          -- key + finIndex + finBlockPointer + finProof (list of Word32s + BlsSignature.signatureSize) + finDelay
          digestSize + 64 + digestSize + (32 * Prelude.length vs) + 48 + 64

writeTransactionStatus :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => TransactionHash -> FinalizedTransactionStatus -> m ()
writeTransactionStatus th ts = resizeOnFull tsSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn ->
        storeReplaceRecord txn (dbh ^. transactionStatusStore) th ts
  where
    tsSize = 2 * digestSize + 16

writeTransactionStatuses :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers st s)
  => [(TransactionHash, FinalizedTransactionStatus)] -> m ()
writeTransactionStatuses ts0 = do
    dbh <- use dbHandlers
    dbh' <- doWTS dbh ts0
    dbHandlers .= dbh'
  where
    doWTS dbh [] = return dbh
    doWTS dbh ts = do
      ts' <- liftIO $ transaction (dbh ^. storeEnv) False $ process dbh ts
      case ts' of
        [] -> return dbh
        _ -> do
          dbh' <- resizeDatabaseHandlers dbh (Prelude.length ts' * (2*digestSize + 16))
          doWTS dbh' ts'
    process _ [] _ = return []
    process dbh ts@((tHash, tStatus) : rts) txn = do
      res <- tryJust selectDBFullError $ storeReplaceRecord txn (dbh ^. transactionStatusStore) tHash tStatus
      case res of
        -- If the DB is full, commit the transaction as is, then retry
        -- once it is resized.
        Left _ -> return ts
        Right _ -> process dbh rts txn
    selectDBFullError = \case (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
                              _ -> Nothing

{-
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
--
-- The underlying monad must be a MonadLogger in order to log the database resizing and other
-- important events.
class (MonadIO m, MonadLogger m) => LMDBStoreMonad m where
  -- |Write a block that was finalized by the given finalization record.
  -- The finalization index is stored with the block.
  writeBlock :: BlockPointerType m -> FinalizationRecord -> m ()

  writeFinalizationRecord :: FinalizationRecord -> m ()

  writeTransactionStatus :: TransactionHash -> T.TransactionStatus -> m ()

  writeTransactionStatuses :: [(TransactionHash, T.TransactionStatus)] -> m ()
-}