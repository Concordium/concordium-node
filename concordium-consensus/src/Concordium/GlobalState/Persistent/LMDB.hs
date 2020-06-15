{-# LANGUAGE FlexibleContexts, NumericUnderscores, ScopedTypeVariables, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, StandaloneDeriving, DerivingVia, RecordWildCards, LambdaCase, TypeApplications, DefaultSignatures #-}
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
import Control.Exception (handleJust, bracketOnError, assert, tryJust)
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.State
import Data.ByteString
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Coerce
import Data.Maybe(isJust)
import qualified Data.Serialize as S
import Database.LMDB.Raw
-- import Database.LMDB.Simple as L
import Lens.Micro.Platform
import System.Directory
import qualified Data.HashMap.Strict as HM
import Foreign.Marshal.Utils
import Foreign.Ptr

class MDBDatabase db where
  type DBKey db
  type DBValue db
  mdbDatabase :: db -> MDB_dbi'
  default mdbDatabase :: (Coercible db MDB_dbi') => db -> MDB_dbi'
  mdbDatabase = coerce
  encodeKey :: Proxy db -> DBKey db -> ByteString
  default encodeKey :: (S.Serialize (DBKey db)) => Proxy db -> DBKey db -> ByteString
  encodeKey _ = S.encode
  encodeValue :: Proxy db -> DBValue db -> LBS.ByteString
  default encodeValue :: (S.Serialize (DBValue db)) => Proxy db -> DBValue db -> LBS.ByteString
  encodeValue _ = S.encodeLazy
  -- |Decode a value. The result should not retain the pointer.
  decodeValue :: Proxy db -> MDB_val -> IO (Either String (DBValue db))
  default decodeValue :: (S.Serialize (DBValue db)) => Proxy db -> MDB_val -> IO (Either String (DBValue db))
  decodeValue _ v = S.decode <$> byteStringFromMDB_val v

transaction :: MDB_env -> Bool -> (MDB_txn -> IO a) -> IO a
transaction env readOnly tx
  = bracketOnError
      (mdb_txn_begin env Nothing readOnly)
      mdb_txn_abort 
      runTx
  where
    runTx txn = do
      res <- tx txn
      mdb_txn_commit txn
      return res

-- |Use a 'ByteString' as an 'MDB_val'.
withMDB_val :: ByteString -> (MDB_val -> IO a) -> IO a
withMDB_val bs a = BS.unsafeUseAsCStringLen bs $ \(ptr, plen) -> a $ MDB_val (fromIntegral plen) (coerce ptr)

byteStringFromMDB_val :: MDB_val -> IO ByteString
byteStringFromMDB_val (MDB_val len ptr) = packCStringLen (coerce ptr, fromIntegral len)

-- |Write a lazy 'LBS.ByteString' into an 'MDB_val'.
-- The destination must have the same size as the source.
writeMDB_val :: LBS.ByteString -> MDB_val -> IO ()
writeMDB_val lbs v = assert (LBS.length lbs == fromIntegral (mv_size v)) $ do
  let f ptr chunk =
        BS.unsafeUseAsCStringLen chunk $ \(cptr, clen) -> do
          copyBytes ptr cptr clen
          return $ plusPtr ptr clen
  foldM_ f (coerce $ mv_data v) (LBS.toChunks lbs)

-- |Store a record. Do not replace an existing record at the same key.
storeRecord :: forall db. (MDBDatabase db) 
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> DBValue db
  -- ^Value
   -> IO ()
storeRecord txn dbi key val = withMDB_val (encodeKey prox key) $ \keyv -> do
    let encVal = encodeValue prox val
    res <- tryJust isKeyExist $ mdb_reserve' writeFlags txn (mdbDatabase dbi) keyv (fromIntegral $ LBS.length encVal)
    case res of
      Left _ -> return () -- Key exists, so nothing to do
      Right valv -> writeMDB_val encVal valv
  where
    prox :: Proxy db
    prox = Proxy
    writeFlags = compileWriteFlags [MDB_NOOVERWRITE]
    isKeyExist LMDB_Error{e_code = Right MDB_KEYEXIST} = Just ()
    isKeyExist _ = Nothing

-- |Store a record. Replace any existing record at the same key.
storeReplaceRecord :: forall db. (MDBDatabase db)
  => MDB_txn
  -- ^Transaction
  -> db
  -- ^Table
  -> DBKey db
  -- ^Key
  -> DBValue db
  -- ^Value
   -> IO ()
storeReplaceRecord txn dbi key val = withMDB_val (encodeKey prox key) $ \keyv -> do
    let encVal = encodeValue prox val
    valv <- mdb_reserve' writeFlags txn (mdbDatabase dbi) keyv (fromIntegral $ LBS.length encVal)
    writeMDB_val encVal valv
  where
    prox :: Proxy db
    prox = Proxy
    writeFlags = compileWriteFlags []

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

instance MDBDatabase TransactionStatusStore where
  type DBKey TransactionStatusStore = TransactionHash
  type DBValue TransactionStatusStore = T.TransactionStatus
  encodeKey _ = hashToByteString

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

class HasDatabaseHandlers st s where
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
closeDatabase db = mdb_env_close (db ^. storeEnv)

resizeDatabaseHandlers :: DatabaseHandlers st -> Int -> IO (DatabaseHandlers st)
resizeDatabaseHandlers dbh size = do
  let delta = size + (dbStepSize - size `mod` dbStepSize)
      _dbMapSize = (dbh ^. dbMapSize) + delta
      _storeEnv = dbh ^. storeEnv
  mdb_env_set_mapsize _storeEnv _dbMapSize
  transaction _storeEnv False $ \txn -> do
    _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) [MDB_CREATE]
    _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) [MDB_CREATE]
    _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) [MDB_CREATE]
    _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) [MDB_CREATE]
    return DatabaseHandlers{..}

-- |For now the database supports four stores: blocks, finalization records, transaction statuses,
-- and hashes of finalized blocks indexed by height. This type abstracts access to those four stores.
--
-- * finalization records can be written separately.
-- * Blocks are written together with adding an index to the by-height store. This is written in a single database transaction.
-- * Transaction statuses can be written either individually, each in its own database transaction, or in one transaction.
--   The latter is preferrable for performance reasons.
data LMDBStoreType st = Block !(StoredBlock st)
                   -- ^The Blockhash, block height and the serialized form of the block
                   | Finalization !FinalizationIndex !FinalizationRecord
                   -- ^The finalization index and the associated finalization record
                   | TxStatus !TransactionHash !T.TransactionStatus
                   -- ^Write a single transaction record.
                   | TxStatuses ![(TransactionHash, T.TransactionStatus)]
                   -- ^Write all transaction records in a single database transaction.
                   -- deriving (Show)

lmdbStoreTypeSize :: S.Serialize st => LMDBStoreType st -> Int

lmdbStoreTypeSize (Block sb) = 2*digestSize + fromIntegral (LBS.length (S.encodeLazy sb))
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
putInProperDB :: (S.Serialize st) => LMDBStoreType st -> DatabaseHandlers st -> IO ()
putInProperDB (Block value) dbh = do
  let env = dbh ^. storeEnv
      b = sbInfo value
  transaction env False $ \txn -> do
    storeReplaceRecord txn (dbh ^. finalizedByHeightStore) (_bpHeight b) (_bpHash b)
    storeReplaceRecord txn (dbh ^. blockStore) (_bpHash b) value
putInProperDB (Finalization key value) dbh =  do
  let env = dbh ^. storeEnv
  transaction env False $ \txn ->
    storeReplaceRecord txn (dbh ^. finalizationRecordStore) key value
putInProperDB (TxStatus key value) dbh =  do
  let env = dbh ^. storeEnv
  transaction env False $ \txn ->
    storeReplaceRecord txn (dbh ^. transactionStatusStore) key value
putInProperDB (TxStatuses statuses) dbh = do
  let env = dbh ^. storeEnv
  transaction env False $ \txn -> do
    let putter (key, value) = storeReplaceRecord txn (dbh ^. transactionStatusStore) key value
    mapM_ putter statuses
  

-- |Provided default function that tries to perform an insertion in a given database of a given value,
-- altering the environment if needed when the database grows.
putOrResize :: (MonadIO m, S.Serialize st) => DatabaseHandlers st -> LMDBStoreType st -> m (DatabaseHandlers st)
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


resizeOnFull :: (MonadIO m, S.Serialize st, MonadState s m, HasDatabaseHandlers st s) => (DatabaseHandlers st -> IO a) -> m a
resizeOnFull a = do
    dbh <- use dbHandlers
    liftIO $ handleJust selectDBFullError handleResize a'
    undefined
  where
    a' dbh = (dbh, ) <$> a dbh
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
