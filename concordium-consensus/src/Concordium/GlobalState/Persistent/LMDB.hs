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
  , makeDatabaseHandlers
  , initializeDatabase
  , closeDatabase
  , resizeOnResized
  , finalizedByHeightStore
  , StoredBlock(..)
  , readBlock
  , readFinalizationRecord
  , readTransactionStatus
  , readFinalizedBlockAtHeight
  , memberTransactionTable
  , loadBlocksFinalizationIndexes
  , getFinalizedBlockAtHeight
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
import qualified Concordium.GlobalState.TransactionTable as T
import Concordium.Crypto.SHA256
import Concordium.Types.HashableTo
import Control.Concurrent (runInBoundThread)
import Control.Exception (tryJust, handleJust)
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


data StoredBlock pv st = StoredBlock {
  sbFinalizationIndex :: !FinalizationIndex,
  sbInfo :: !BasicBlockPointerData,
  sbBlock :: !(Block pv),
  sbState :: !st
}

instance (IsProtocolVersion pv, S.Serialize st) => S.Serialize (StoredBlock pv st) where
  put StoredBlock{..} = S.put sbFinalizationIndex <>
          S.put sbInfo <>
          putBlockV1 sbBlock <>
          S.put sbState
  get = do
          sbFinalizationIndex <- S.get
          sbInfo <- S.get
          sbBlock <- getBlockV1 (utcTimeToTransactionTime (_bpReceiveTime sbInfo))
          sbState <- S.get
          return StoredBlock{..}

newtype BlockStore (pv :: ProtocolVersion) st = BlockStore MDB_dbi'

instance (IsProtocolVersion pv, S.Serialize st) => MDBDatabase (BlockStore pv st) where
  type DBKey (BlockStore pv st) = BlockHash
  type DBValue (BlockStore pv st) = StoredBlock pv st
  encodeKey _ = hashToByteString . blockHash

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
  encodeValue _ = LBS.fromStrict . hashToByteString . blockHash

-- |Values used by the LMDBStoreMonad to manage the database.
-- Sometimes we only want read access
data DatabaseHandlers pv st = DatabaseHandlers {
    _storeEnv :: !MDB_env,
    _blockStore :: !(BlockStore pv st),
    _finalizationRecordStore :: !FinalizationRecordStore,
    _transactionStatusStore :: !TransactionStatusStore,
    _finalizedByHeightStore :: !FinalizedByHeightStore
}
makeLenses ''DatabaseHandlers

class HasDatabaseHandlers pv st s | s -> pv st where
  dbHandlers :: Lens' s (DatabaseHandlers pv st)

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
databaseHandlers :: RuntimeParameters -> IO (DatabaseHandlers pv st)
databaseHandlers RuntimeParameters{..} = makeDatabaseHandlers rpTreeStateDir False

-- |Initialize database handlers.
makeDatabaseHandlers
  :: FilePath
  -- ^Path of database
  -> Bool
  -- ^Open read only
  -> IO (DatabaseHandlers pv st)
makeDatabaseHandlers treeStateDir readOnly = do
  _storeEnv <- mdb_env_create
  mdb_env_set_mapsize _storeEnv dbInitSize
  mdb_env_set_maxdbs _storeEnv 4
  mdb_env_set_maxreaders _storeEnv 126
  -- TODO: Consider MDB_NOLOCK
  mdb_env_open _storeEnv treeStateDir [MDB_RDONLY | readOnly]
  transaction _storeEnv readOnly $ \txn -> do
    _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) [MDB_CREATE | not readOnly]
    _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) [MDB_CREATE | not readOnly]
    _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) [MDB_CREATE | not readOnly]
    _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) [MDB_CREATE | not readOnly]
    return DatabaseHandlers{..}

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initializeDatabase :: (IsProtocolVersion pv, S.Serialize st) => PersistentBlockPointer pv ati bs -> st -> RuntimeParameters -> IO (DatabaseHandlers pv st)
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

closeDatabase :: DatabaseHandlers pv st -> IO ()
closeDatabase db = runInBoundThread $ mdb_env_close (db ^. storeEnv)

-- |Resize the LMDB map if the file size has changed.
-- This is used to allow a secondary process that is reading the database
-- to handle resizes to the database that are made by the writer.
-- The supplied action will be executed. If it fails with an 'MDB_MAP_RESIZED'
-- error, then the map will be resized and the action retried.
resizeOnResized :: DatabaseHandlers pv st -> IO a -> IO a
resizeOnResized dbh a = inner
  where
    inner = handleJust checkResized onResized a
    checkResized LMDB_Error{..} = guard (e_code == Right MDB_MAP_RESIZED)
    onResized _ = mdb_env_set_mapsize (dbh ^. storeEnv) 0 >> inner

resizeDatabaseHandlers :: (MonadIO m, MonadLogger m) => DatabaseHandlers pv st -> Int -> m ()
resizeDatabaseHandlers dbh size = do
  envInfo <- liftIO $ mdb_env_info (dbh ^. storeEnv)
  let delta = size + (dbStepSize - size `mod` dbStepSize)
      oldMapSize = fromIntegral $ me_mapsize envInfo
      newMapSize = oldMapSize + delta
      _storeEnv = dbh ^. storeEnv
  logEvent LMDB LLDebug $ "Resizing database from " ++ show oldMapSize ++ " to " ++ show newMapSize
  liftIO $ mdb_env_set_mapsize _storeEnv newMapSize

-- |Read a block from the database by hash.
readBlock :: (MonadIO m, MonadState s m, IsProtocolVersion pv, HasDatabaseHandlers pv st s, S.Serialize st)
  => BlockHash
  -> m (Maybe (StoredBlock pv st))
readBlock bh = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. blockStore) bh

-- |Read a finalization record from the database by finalization index.
readFinalizationRecord :: (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => FinalizationIndex
  -> m (Maybe FinalizationRecord)
readFinalizationRecord finIndex = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. finalizationRecordStore) finIndex

-- |Read the status of a transaction from the database by the transaction hash.
readTransactionStatus :: (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => TransactionHash
  -> m (Maybe FinalizedTransactionStatus)
readTransactionStatus txHash = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> loadRecord txn (dbh ^. transactionStatusStore) txHash

-- |Get a block from the database by its height.
getFinalizedBlockAtHeight :: (IsProtocolVersion pv, S.Serialize st)
  => DatabaseHandlers pv st
  -> BlockHeight
  -> IO (Maybe (StoredBlock pv st))
getFinalizedBlockAtHeight dbh bHeight = transaction (dbh ^. storeEnv) True
    $ \txn -> do
        mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) bHeight
        join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash

-- |Read a block from the database by its height.
readFinalizedBlockAtHeight :: (MonadIO m, MonadState s m, IsProtocolVersion pv, HasDatabaseHandlers pv st s, S.Serialize st)
  => BlockHeight
  -> m (Maybe (StoredBlock pv st))
readFinalizedBlockAtHeight bHeight = do
  dbh <- use dbHandlers
  liftIO $ getFinalizedBlockAtHeight dbh bHeight

-- |Check if the given key is in the on-disk transaction table.
memberTransactionTable :: (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => TransactionHash
  -> m Bool
memberTransactionTable th = do
  dbh <- use dbHandlers
  liftIO
    $ transaction (dbh ^. storeEnv) True
    $ \txn -> isRecordPresent txn (dbh ^. transactionStatusStore) th

-- |Build a table of a block finalization indexes for blocks.
loadBlocksFinalizationIndexes :: (IsProtocolVersion pv, S.Serialize st) => DatabaseHandlers pv st -> IO (Either String (HM.HashMap BlockHash FinalizationIndex))
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
getLastBlock :: (IsProtocolVersion pv, S.Serialize st) => DatabaseHandlers pv st -> IO (Either String (FinalizationRecord, StoredBlock pv st))
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
getFirstBlock :: (IsProtocolVersion pv, S.Serialize st) => DatabaseHandlers pv st -> IO (Maybe (StoredBlock pv st))
getFirstBlock dbh = transaction (dbh ^. storeEnv) True $ \txn -> do
        mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) 0
        join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash


-- |Perform a database action that may require the database to be resized,
-- resizing the database if necessary. The size argument is only evaluated
-- if the resize actually happens.
resizeOnFull :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => Int
  -- ^Additional size
  -> (DatabaseHandlers pv st -> IO a)
  -- ^Action that may require resizing
  -> m a
resizeOnFull addSize a = do
    dbh <- use dbHandlers
    inner dbh
  where
    inner dbh = do
      r <- liftIO $ tryJust selectDBFullError (a dbh)
      case r of
        Left _ -> do
          -- Resize the database handlers, and try to add again in case the size estimate
          -- given by lmdbStoreTypeSize is off.
          resizeDatabaseHandlers dbh addSize
          inner dbh
        Right res -> return res
    -- only handle the db full error and propagate other exceptions.
    selectDBFullError = \case (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
                              _ -> Nothing

-- |Write a block to the database. Adds it both to the index by height and
-- by block hash.
writeBlock :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s, IsProtocolVersion pv, S.Serialize st)
  => StoredBlock pv st -> m ()
writeBlock block = resizeOnFull blockSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn -> do
        let b = sbInfo block
        storeReplaceRecord txn (dbh ^. finalizedByHeightStore) (_bpHeight b) (_bpHash b)
        storeReplaceRecord txn (dbh ^. blockStore) (_bpHash b) block
  where
    blockSize = 2*digestSize + fromIntegral (LBS.length (S.encodeLazy block))

-- |Write a finalization record to the database.
writeFinalizationRecord :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => FinalizationRecord -> m ()
writeFinalizationRecord finRec = resizeOnFull finRecSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn ->
        storeReplaceRecord txn (dbh ^. finalizationRecordStore) (finalizationIndex finRec) finRec
  where
    finRecSize = let FinalizationProof vs _ = finalizationProof finRec in
          -- key + finIndex + finBlockPointer + finProof (list of Word32s + BlsSignature.signatureSize) + finDelay
          digestSize + 64 + digestSize + (32 * Prelude.length vs) + 48 + 64

-- |Write a single transaction status to the database.
writeTransactionStatus :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => TransactionHash -> FinalizedTransactionStatus -> m ()
writeTransactionStatus th ts = resizeOnFull tsSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn ->
        storeReplaceRecord txn (dbh ^. transactionStatusStore) th ts
  where
    tsSize = 2 * digestSize + 16

-- |Write a collection of transaction statuses to the database.  This occurs
-- as a single transaction which is faster than writing them individually.
writeTransactionStatuses :: (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s)
  => [(TransactionHash, FinalizedTransactionStatus)] -> m ()
writeTransactionStatuses tss = resizeOnFull tssSize
    $ \dbh -> transaction (dbh ^. storeEnv) False
    $ \txn -> forM_ tss (\(tHash, tStatus) -> storeReplaceRecord txn (dbh ^. transactionStatusStore) tHash tStatus)
  where
    tssSize = (Prelude.length tss) * (2 * digestSize + 16)
