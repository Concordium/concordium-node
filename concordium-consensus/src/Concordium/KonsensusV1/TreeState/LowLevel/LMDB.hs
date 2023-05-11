{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module provides an implementation of the 'MonadTreeStateStore' interface that uses LMDB
-- for storage.
module Concordium.KonsensusV1.TreeState.LowLevel.LMDB where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Lens.Micro.Platform

import Concordium.Common.Version
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.Types

import Concordium.GlobalState.LMDB.Helpers
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo

-- * Exceptions

-- |Exception occurring from a violation of database invariants in the LMDB database.
newtype DatabaseInvariantViolation = DatabaseInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseInvariantViolation where
    displayException (DatabaseInvariantViolation reason) =
        "Database invariant violation: "
            ++ show reason

-- * Database tables

-- ** Block store

-- |Block store for finalized blocks by height.
newtype BlockStore (pv :: ProtocolVersion) = BlockStore MDB_dbi'

instance IsProtocolVersion pv => MDBDatabase (BlockStore pv) where
    type DBKey (BlockStore pv) = BlockHeight
    type DBValue (BlockStore pv) = StoredBlock pv

-- ** Blocks by hash index

-- |Index mapping block hashes to block heights.
newtype BlockHashIndex = BlockHashIndex MDB_dbi'

instance MDBDatabase BlockHashIndex where
    type DBKey BlockHashIndex = BlockHash
    encodeKey _ = Hash.hashToByteString . blockHash
    type DBValue BlockHashIndex = BlockHeight

-- ** Transaction status

-- |A transaction status store table. A @TransactionStatusStore@ stores
-- 'FinalizedTransactionStatus'es indexed by 'TransactionHash'.
newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

instance MDBDatabase TransactionStatusStore where
    type DBKey TransactionStatusStore = TransactionHash
    type DBValue TransactionStatusStore = FinalizedTransactionStatus

-- ** Consensus status

--

-- $consensusStatus
-- The consensus status table stores the round status and latest finalization entry. To support
-- access, we provide two implementations of 'MDBDatabase' in the form of 'RoundStatusStore' and
-- 'LatestFinalizationEntryStore'. These are intended to be stored in the same underlying table,
-- and thus their keys are serialized distinctly. Consequently, it is not appropriate to use a
-- cursor (e.g. using 'withCursor'), which can result in failed or incorrect deserialization.

-- |Key used to index the round status entry in the consensus status table.
-- Represented as the byte @0@.
data RoundStatusKey = CSKRoundStatus
    deriving (Eq, Ord, Bounded, Enum, Show)

instance S.Serialize RoundStatusKey where
    put CSKRoundStatus = S.putWord8 0
    get =
        S.getWord8 >>= \case
            0 -> return CSKRoundStatus
            _ -> fail "Expected CSKRoundStatus"

-- |Round status store table.
newtype RoundStatusStore = RoundStatusStore MDB_dbi'

instance MDBDatabase RoundStatusStore where
    type DBKey RoundStatusStore = RoundStatusKey
    type DBValue RoundStatusStore = PersistentRoundStatus

-- |Key used to index the latest finalization entry in the consensus status table.
-- Represented as the byte @1@.
data LatestFinalizationEntryKey = CSKLatestFinalizationEntry
    deriving (Eq, Ord, Bounded, Enum, Show)

instance S.Serialize LatestFinalizationEntryKey where
    put CSKLatestFinalizationEntry = S.putWord8 1
    get =
        S.getWord8 >>= \case
            1 -> return CSKLatestFinalizationEntry
            _ -> fail "Expected CSKLatestFinalizationEntry"

-- |Latest finalization status store table.
newtype LatestFinalizationEntryStore = LatestFinalizationEntryStore MDB_dbi'

instance MDBDatabase LatestFinalizationEntryStore where
    type DBKey LatestFinalizationEntryStore = LatestFinalizationEntryKey
    type DBValue LatestFinalizationEntryStore = FinalizationEntry

-- ** Metadata store

-- |The metadata store table.
-- This table is for storing version-related information.
newtype MetadataStore = MetadataStore MDB_dbi'

instance MDBDatabase MetadataStore where
    type DBKey MetadataStore = BS.ByteString
    encodeKey _ bs = bs
    decodeKey _ k = Right <$> byteStringFromMDB_val k
    type DBValue MetadataStore = BS.ByteString
    encodeValue _ = LBS.fromStrict
    decodeValue _ _ v = Right <$> byteStringFromMDB_val v

-- |Key to the version information.
-- This key should map to a serialized 'VersionMetadata' structure.
versionMetadata :: DBKey MetadataStore
versionMetadata = "version"

data VersionMetadata = VersionMetadata
    { -- |Version signifier for the database itself.
      vmDatabaseVersion :: !Version,
      -- |Protocol version, which may impact the storage of blocks/finalization records
      -- independently of the database version.
      vmProtocolVersion :: !ProtocolVersion
    }
    deriving (Eq)

instance Show VersionMetadata where
    show VersionMetadata{..} =
        "{databaseVersion: "
            ++ show vmDatabaseVersion
            ++ ", protocolVersion: "
            ++ show vmProtocolVersion
            ++ "}"

instance S.Serialize VersionMetadata where
    put VersionMetadata{..} = do
        S.put vmDatabaseVersion
        S.put vmProtocolVersion
    get = do
        vmDatabaseVersion <- S.get
        vmProtocolVersion <- S.get
        return VersionMetadata{..}

-- * Database

-- |The LMDB environment and tables.
data DatabaseHandlers (pv :: ProtocolVersion) = DatabaseHandlers
    { -- |The LMDB environment.
      _storeEnv :: !StoreEnv,
      -- |Blocks by height.
      _blockStore :: !(BlockStore pv),
      -- |Index of blocks by hash.
      _blockHashIndex :: !BlockHashIndex,
      -- |Index of finalized transactions by hash.
      _transactionStatusStore :: !TransactionStatusStore,
      -- |Storage for the 'RoundStatus'.
      _roundStatusStore :: !RoundStatusStore,
      -- |Storage for the latest 'FinalizationEntry'.
      _latestFinalizationEntryStore :: !LatestFinalizationEntryStore,
      -- |Metadata storage (i.e. version information).
      _metadataStore :: !MetadataStore
    }

makeClassy ''DatabaseHandlers

-- |Name of the table used for storing blocks by height.
blockStoreName :: String
blockStoreName = "blocksByHeight"

-- |Name of the table used for indexing blocks by hash.
blockHashIndexName :: String
blockHashIndexName = "blockHashIndex"

-- |Name of the table used for indexing transactions by hash.
transactionStatusStoreName :: String
transactionStatusStoreName = "transactionStatus"

-- |Name of the table used for storing the round status and latest finalization entry.
consensusStatusStoreName :: String
consensusStatusStoreName = "consensusStatus"

-- |Name of the table used for storing version metadata.
metadataStoreName :: String
metadataStoreName = "metadata"

-- |The number of databases in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 5

-- |Database growth size increment.
-- This is currently set at 64MB, and must be a multiple of the page size.
dbStepSize :: Int
dbStepSize = 2 ^ (26 :: Int) -- 64MB

-- |Maximum step to increment the database size.
dbMaxStepSize :: Int
dbMaxStepSize = 2 ^ (30 :: Int) -- 1GB

-- |Initial database size.
-- This is currently set to be the same as 'dbStepSize'.
dbInitSize :: Int
dbInitSize = dbStepSize

-- ** Helpers

-- |Resize the LMDB map if the file size has changed.
-- This is used to allow a secondary process that is reading the database
-- to handle resizes to the database that are made by the writer.
-- The supplied action will be executed. If it fails with an 'MDB_MAP_RESIZED'
-- error, then the map will be resized and the action retried.
resizeOnResized :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv, MonadCatch m) => m a -> m a
resizeOnResized a = do
    dbh <- view databaseHandlers
    resizeOnResizedInternal (dbh ^. storeEnv) a

-- |Perform a database action and resize the LMDB map if the file size has changed. The difference
-- with `resizeOnResized` is that this function takes database handlers as an argument, instead of
-- reading their value from `HasDatabaseHandlers`.
resizeOnResizedInternal :: (MonadIO m, MonadCatch m) => StoreEnv -> m a -> m a
resizeOnResizedInternal se a = inner
  where
    inner = handleJust checkResized onResized a
    checkResized LMDB_Error{..} = guard (e_code == Right MDB_MAP_RESIZED)
    onResized _ = do
        liftIO (withWriteStoreEnv se $ flip mdb_env_set_mapsize 0)
        inner

-- |Increase the database size by at least the supplied size.
-- The size SHOULD be a multiple of 'dbStepSize', and MUST be a multiple of the page size.
resizeDatabaseHandlers :: (MonadIO m, MonadLogger m) => DatabaseHandlers pv -> Int -> m ()
resizeDatabaseHandlers dbh delta = do
    envInfo <- liftIO $ mdb_env_info (dbh ^. storeEnv . seEnv)
    let oldMapSize = fromIntegral $ me_mapsize envInfo
        newMapSize = oldMapSize + delta
        _storeEnv = dbh ^. storeEnv
    logEvent LMDB LLDebug $ "Resizing database from " ++ show oldMapSize ++ " to " ++ show newMapSize
    liftIO . withWriteStoreEnv (dbh ^. storeEnv) $ flip mdb_env_set_mapsize newMapSize

-- ** Initialization

-- |Initialize database handlers.
-- The size will be rounded up to a multiple of 'dbStepSize'.
-- (This ensures in particular that the size is a multiple of the page size, which is required by
-- LMDB.)
makeDatabaseHandlers ::
    -- |Path of database
    FilePath ->
    -- |Open read only
    Bool ->
    -- |Initial database size
    Int ->
    IO (DatabaseHandlers pv)
makeDatabaseHandlers treeStateDir readOnly initSize = do
    _storeEnv <- makeStoreEnv
    -- here nobody else has access to the environment, so we need not lock
    let env = _storeEnv ^. seEnv
    mdb_env_set_mapsize env (initSize + dbStepSize - initSize `mod` dbStepSize)
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    mdb_env_open env treeStateDir [MDB_RDONLY | readOnly]
    transaction _storeEnv readOnly $ \txn -> do
        _blockStore <-
            BlockStore
                <$> mdb_dbi_open'
                    txn
                    (Just blockStoreName)
                    [MDB_CREATE | not readOnly]
        _blockHashIndex <-
            BlockHashIndex
                <$> mdb_dbi_open'
                    txn
                    (Just blockHashIndexName)
                    [MDB_CREATE | not readOnly]
        _transactionStatusStore <-
            TransactionStatusStore
                <$> mdb_dbi_open'
                    txn
                    (Just transactionStatusStoreName)
                    [MDB_CREATE | not readOnly]
        consensusStatusStore <-
            mdb_dbi_open'
                txn
                (Just consensusStatusStoreName)
                [MDB_CREATE | not readOnly]
        let _roundStatusStore = RoundStatusStore consensusStatusStore
        let _latestFinalizationEntryStore = LatestFinalizationEntryStore consensusStatusStore
        _metadataStore <-
            MetadataStore
                <$> mdb_dbi_open'
                    txn
                    (Just metadataStoreName)
                    [MDB_CREATE | not readOnly]
        return DatabaseHandlers{..}

-- |Initialize database handlers in ReadWrite mode.
-- This simply loads the references and does not initialize the databases.
-- The initial size is set to 64MB.
openDatabase :: FilePath -> IO (DatabaseHandlers pv)
openDatabase treeStateDir = makeDatabaseHandlers treeStateDir False dbInitSize

-- |Close the database. The database should not be used after it is closed.
closeDatabase :: DatabaseHandlers pv -> IO ()
closeDatabase dbHandlers = runInBoundThread $ mdb_env_close $ dbHandlers ^. storeEnv . seEnv

-- |'DatabaseHandlers' existentially quantified over the protocol version and without block state.
-- Note that we can treat the state type as '()' soundly when reading, since the state is the last
-- part of the serialization: we just ignore the remaining bytes.
data VersionDatabaseHandlers
    = forall pv.
        IsProtocolVersion pv =>
      VersionDatabaseHandlers (DatabaseHandlers pv)

-- |Open an existing database for reading. This checks that the version is supported and returns
-- a handler that is existentially quantified over the protocol version.
--
-- This is required for functionality such as the block exporter, which reads the database but does
-- not have sufficient context to infer the protocol version.
openReadOnlyDatabase ::
    -- |Path of database
    FilePath ->
    IO (Maybe VersionDatabaseHandlers)
openReadOnlyDatabase treeStateDir = do
    _storeEnv <- makeStoreEnv
    let env = _storeEnv ^. seEnv
    mdb_env_set_mapsize env dbInitSize
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    mdb_env_open env treeStateDir [MDB_RDONLY]
    (_metadataStore, mversion) <- resizeOnResizedInternal _storeEnv $ transaction _storeEnv True $ \txn -> do
        _metadataStore <- MetadataStore <$> mdb_dbi_open' txn (Just metadataStoreName) []
        mversion <- loadRecord txn _metadataStore versionMetadata
        return (_metadataStore, mversion)
    case mversion of
        Nothing -> Nothing <$ mdb_env_close env
        Just v -> case S.decode v of
            Right VersionMetadata{vmDatabaseVersion = 1, ..} ->
                -- Promote the term level vmProtocolVersion to a type-level value pv, which is
                -- existentially quantified in the return type.  We do not currently match on the
                -- protocol version itself, since the database handlers are parametric in the protocol
                -- version.
                case promoteProtocolVersion vmProtocolVersion of
                    SomeProtocolVersion (_ :: SProtocolVersion pv) ->
                        resizeOnResizedInternal _storeEnv $ transaction _storeEnv True $ \txn -> do
                            _blockStore <-
                                BlockStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just blockStoreName)
                                        []
                            _blockHashIndex <-
                                BlockHashIndex
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just blockHashIndexName)
                                        []
                            _transactionStatusStore <-
                                TransactionStatusStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just transactionStatusStoreName)
                                        []
                            consensusStatusStore <-
                                mdb_dbi_open'
                                    txn
                                    (Just consensusStatusStoreName)
                                    []
                            let _roundStatusStore =
                                    RoundStatusStore consensusStatusStore
                            let _latestFinalizationEntryStore =
                                    LatestFinalizationEntryStore consensusStatusStore
                            return (Just (VersionDatabaseHandlers @pv DatabaseHandlers{..}))
            _ -> Nothing <$ mdb_env_close env

-- ** Monad implementation

-- |A newtype wrapper that provides a 'MonadTreeStateStore' implementation using LMDB.
newtype DiskLLDBM (pv :: ProtocolVersion) m a = DiskLLDBM {runDiskLLDBM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger) via m
    deriving (MonadTrans) via IdentityT

deriving instance MonadReader r m => MonadReader r (DiskLLDBM pv m)

instance IsProtocolVersion pv => MonadProtocolVersion (DiskLLDBM pv m) where
    type MPV (DiskLLDBM pv m) = pv

-- |Run a read-only transaction.
asReadTransaction :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv) => (DatabaseHandlers pv -> MDB_txn -> IO a) -> DiskLLDBM pv m a
asReadTransaction t = do
    dbh <- view databaseHandlers
    liftIO $ transaction (dbh ^. storeEnv) True $ t dbh

-- |Run a write transaction. If the transaction fails due to the database being full, this resizes
-- the database and retries the transaction.
asWriteTransaction :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv, MonadLogger m) => (DatabaseHandlers pv -> MDB_txn -> IO a) -> DiskLLDBM pv m a
asWriteTransaction t = do
    dbh <- view databaseHandlers
    let doTransaction = transaction (dbh ^. storeEnv) False $ t dbh
        inner step = do
            r <- liftIO $ tryJust selectDBFullError doTransaction
            case r of
                Left _ -> do
                    -- We resize by the step size initially, and by double for each successive
                    -- failure.
                    resizeDatabaseHandlers dbh step
                    inner (min (step * 2) dbMaxStepSize)
                Right res -> return res
    inner dbStepSize
  where
    -- only handle the db full error and propagate other exceptions.
    selectDBFullError = \case
        (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
        _ -> Nothing

instance
    ( IsProtocolVersion pv,
      MonadReader r m,
      HasDatabaseHandlers r pv,
      MonadIO m,
      MonadCatch m,
      MonadLogger m
    ) =>
    MonadTreeStateStore (DiskLLDBM pv m)
    where
    lookupBlock bh = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. blockHashIndex) bh >>= \case
            Just height -> loadRecord txn (dbh ^. blockStore) height
            Nothing -> return Nothing
    memberBlock bh = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. blockHashIndex) bh
    lookupFirstBlock = lookupBlockByHeight 0
    lookupLastBlock = asReadTransaction $ \dbh txn ->
        withCursor txn (dbh ^. blockStore) (getCursor CursorLast) <&> \case
            Just (Right (_, v)) -> Just v
            _ -> Nothing
    lookupBlockByHeight height = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. blockStore) height
    lookupTransaction txHash = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. transactionStatusStore) txHash
    memberTransaction txHash = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. transactionStatusStore) txHash

    writeBlocks blocks fe = asWriteTransaction $ \dbh txn -> do
        forM_ blocks $ \block -> do
            let height = blockHeight block
            storeReplaceRecord txn (dbh ^. blockStore) height block
            storeReplaceRecord txn (dbh ^. blockHashIndex) (getHash block) height
            forM_ (zip (blockTransactions block) [0 ..]) $ \(tx, ti) ->
                storeReplaceRecord
                    txn
                    (dbh ^. transactionStatusStore)
                    (getHash tx)
                    (FinalizedTransactionStatus height ti)
        storeReplaceRecord
            txn
            (dbh ^. latestFinalizationEntryStore)
            CSKLatestFinalizationEntry
            fe

    lookupLatestFinalizationEntry = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. latestFinalizationEntryStore) CSKLatestFinalizationEntry

    lookupCurrentRoundStatus = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. roundStatusStore) CSKRoundStatus >>= \case
            Just rs -> return rs
            _ -> throwM (DatabaseInvariantViolation "Missing current round status")

    writeCurrentRoundStatus rs = asWriteTransaction $ \dbh txn ->
        storeReplaceRecord txn (dbh ^. roundStatusStore) CSKRoundStatus rs

    rollBackBlocksUntil predicate = roll 0
      where
        -- The ctr indicates how many blocks that have been rolled back.
        roll ctr = do
            getLast >>= \case
                Nothing -> return $ Right ctr
                Just (Left e) -> return $ Left $ "Could not load last finalized block: " ++ e
                Just (Right (h, sb)) -> do
                    ok <- predicate sb
                    if ok
                        then return $ Right ctr
                        else do
                            unless (ctr == 0) $ do
                                logEvent
                                    TreeState
                                    LLWarning
                                    "Database corruption detected.\
                                    \ Attempting to roll-back to a usable state."
                            logEvent TreeState LLDebug $
                                "Removing block "
                                    ++ show (getHash sb :: BlockHash)
                                    ++ " at height "
                                    ++ show h
                            asWriteTransaction $ \dbh txn -> do
                                -- If we haven't already, remove the latest finalization entry
                                unless (ctr == 0) . void $
                                    deleteRecord
                                        txn
                                        (dbh ^. latestFinalizationEntryStore)
                                        CSKLatestFinalizationEntry
                                -- Remove the block
                                _ <- deleteRecord txn (dbh ^. blockStore) h
                                _ <- deleteRecord txn (dbh ^. blockHashIndex) (getHash sb)
                                -- Remove the block transactions
                                forM_ (blockTransactions sb) $ \tx ->
                                    deleteRecord txn (dbh ^. transactionStatusStore) (getHash tx)
                            roll $! ctr + 1
        getLast = asReadTransaction $ \dbh txn ->
            withCursor txn (dbh ^. blockStore) (getCursor CursorLast)

-- |Initialise the low-level database by writing out the genesis block and initial round status.
initialiseLowLevelDB ::
    (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv, MonadLogger m, IsProtocolVersion pv) =>
    -- |Genesis block.
    StoredBlock pv ->
    -- |Initial round status.
    PersistentRoundStatus ->
    DiskLLDBM pv m ()
initialiseLowLevelDB genesisBlock roundStatus = asWriteTransaction $ \dbh txn -> do
    storeReplaceRecord txn (dbh ^. blockStore) 0 genesisBlock
    storeReplaceRecord txn (dbh ^. blockHashIndex) (getHash genesisBlock) 0
    storeReplaceRecord txn (dbh ^. roundStatusStore) CSKRoundStatus roundStatus
