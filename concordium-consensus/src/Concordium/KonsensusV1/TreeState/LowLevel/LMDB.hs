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
import qualified Data.ByteString.Unsafe as BS
import Data.Data
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Foreign (
    Storable (..),
    alloca,
    castPtr,
    malloc,
 )
import Foreign.C.Types
import Lens.Micro.Platform
import System.IO.Unsafe

import Concordium.Common.Version
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.Types

import Concordium.GlobalState.LMDB.Helpers
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Types.HashableTo

-- |Exception occurring from a violation of database invariants in the LMDB database.
newtype DatabaseInvariantViolation = DatabaseInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseInvariantViolation where
    displayException (DatabaseInvariantViolation reason) =
        "Database invariant violation: "
            ++ show reason

-- |Size of 'CSize'.
sizeOfCSize :: Int
sizeOfCSize = sizeOf (undefined :: CSize)

-- |Block store for finalized blocks by height.
-- Note: This table should be opened with the option 'MDB_INTEGERKEY', as the keys are stored in
-- platform endianness.
newtype BlockStore (pv :: ProtocolVersion) = BlockStore MDB_dbi'

instance IsProtocolVersion pv => MDBDatabase (BlockStore pv) where
    type DBKey (BlockStore pv) = BlockHeight
    type DBValue (BlockStore pv) = StoredBlock pv
    encodeKey _ (BlockHeight h) = unsafePerformIO $ do
        ptr <- malloc
        poke ptr (CSize h)
        BS.unsafePackMallocCStringLen (castPtr ptr, sizeOfCSize)
    withKey _ (BlockHeight h) f = alloca $ \ptr -> do
        poke ptr (CSize h)
        f $ MDB_val (fromIntegral sizeOfCSize) (castPtr ptr)
    decodeKey _ (MDB_val sz ptr)
        | sz == fromIntegral sizeOfCSize = do
            (CSize h) <- peek (castPtr ptr)
            return (Right (BlockHeight h))
        | otherwise = return (Left $ "decoded block store key with invalid size: " ++ show sz)

-- |Index mapping block hashes to block heights.
newtype BlockHashIndex = BlockHashIndex MDB_dbi'

instance MDBDatabase BlockHashIndex where
    type DBKey BlockHashIndex = BlockHash
    encodeKey _ = Hash.hashToByteString . blockHash
    type DBValue BlockHashIndex = BlockHeight

-- |A transaction status store table. A @TransactionStatusStore@ stores
-- 'FinalizedTransactionStatus'es indexed by 'TransactionHash'.
newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

instance MDBDatabase TransactionStatusStore where
    type DBKey TransactionStatusStore = TransactionHash
    type DBValue TransactionStatusStore = FinalizedTransactionStatus

data ConsensusStatusKey
    = CSKRoundStatus
    | CSKLatestFinalizationEntry
    deriving (Eq, Ord, Bounded, Enum, Show)

data ConsensusStatusVal
    = CSVRoundStatus !RoundStatus
    | CSVLastFinalizationEntry !FinalizationEntry

instance S.Serialize ConsensusStatusVal where
    put (CSVRoundStatus rs) = do
        S.putWord8 0
        S.put rs
    put (CSVLastFinalizationEntry lfe) = do
        S.putWord8 1
        S.put lfe
    get =
        S.getWord8 >>= \case
            0 -> CSVRoundStatus <$> S.get
            1 -> CSVLastFinalizationEntry <$> S.get
            _ -> fail "Unsupported consensus status type"

-- |Consensus status store table.
-- Note: This table should be opened with the option 'MDB_INTEGERKEY', as the keys are stored in
-- platform endianness.
newtype ConsensusStatusStore = ConsensusStatusStore MDB_dbi'

instance MDBDatabase ConsensusStatusStore where
    type DBKey ConsensusStatusStore = ConsensusStatusKey
    encodeKey _ k = unsafePerformIO $ do
        ptr <- malloc
        poke ptr (fromIntegral (fromEnum k) :: CSize)
        BS.unsafePackMallocCStringLen (castPtr ptr, sizeOfCSize)
    withKey _ k f = alloca $ \ptr -> do
        poke ptr (fromIntegral (fromEnum k) :: CSize)
        f $ MDB_val (fromIntegral sizeOfCSize) (castPtr ptr)
    decodeKey _ (MDB_val sz ptr)
        | sz == fromIntegral sizeOfCSize = do
            (CSize h) <- peek (castPtr ptr)
            return (Right (toEnum (fromIntegral h)))
        | otherwise = return (Left $ "decoded consensus status store key with invalid size: " ++ show sz)
    type DBValue ConsensusStatusStore = ConsensusStatusVal

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

data DatabaseHandlers (pv :: ProtocolVersion) = DatabaseHandlers
    { _storeEnv :: !StoreEnv,
      _blockStore :: !(BlockStore pv),
      _blockHashIndex :: !BlockHashIndex,
      _transactionStatusStore :: !TransactionStatusStore,
      _consensusStatusStore :: !ConsensusStatusStore,
      _metadataStore :: !MetadataStore
    }

makeClassy ''DatabaseHandlers

blockStoreName :: String
blockStoreName = "blocksByHeight"

blockHashIndexName :: String
blockHashIndexName = "blockHashIndex"

transactionStatusStoreName :: String
transactionStatusStoreName = "transactionStatus"

consensusStatusStoreName :: String
consensusStatusStoreName = "consensusStatus"

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
        -- Note: BlockStore is opened with 'MDB_INTEGERKEY'.
        _blockStore <-
            BlockStore
                <$> mdb_dbi_open'
                    txn
                    (Just blockStoreName)
                    (MDB_INTEGERKEY : [MDB_CREATE | not readOnly])
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
        -- Note: ConsensusStatusStore is opened with 'MDB_INTEGERKEY'.
        _consensusStatusStore <-
            ConsensusStatusStore
                <$> mdb_dbi_open'
                    txn
                    (Just consensusStatusStoreName)
                    (MDB_INTEGERKEY : [MDB_CREATE | not readOnly])
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
                            -- Note: BlockStore is opened with 'MDB_INTEGERKEY'.
                            _blockStore <-
                                BlockStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just blockStoreName)
                                        [MDB_INTEGERKEY]
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
                            -- Note: ConsensusStatusStore is opened with 'MDB_INTEGERKEY'.
                            _consensusStatusStore <-
                                ConsensusStatusStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just consensusStatusStoreName)
                                        [MDB_INTEGERKEY]
                            return (Just (VersionDatabaseHandlers @pv DatabaseHandlers{..}))
            _ -> Nothing <$ mdb_env_close env

newtype DiskLLDBM (pv :: ProtocolVersion) m a = DiskLLDBM {runDiskLLDBM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger) via m
    deriving (MonadTrans) via IdentityT

deriving instance MonadReader r m => MonadReader r (DiskLLDBM pv m)

instance IsProtocolVersion pv => MonadProtocolVersion (DiskLLDBM pv m) where
    type MPV (DiskLLDBM pv m) = pv

asReadTransaction :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv) => (DatabaseHandlers pv -> MDB_txn -> IO a) -> DiskLLDBM pv m a
asReadTransaction t = do
    dbh <- view databaseHandlers
    liftIO $ transaction (dbh ^. storeEnv) True $ t dbh

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
            Just blockHeight -> loadRecord txn (dbh ^. blockStore) blockHeight
            Nothing -> return Nothing
    memberBlock bh = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. blockHashIndex) bh
    lookupFirstBlock = lookupBlockByHeight 0
    lookupLastBlock = asReadTransaction $ \dbh txn ->
        withCursor txn (dbh ^. blockStore) (getCursor CursorLast) <&> \case
            Just (Right (_, v)) -> Just v
            _ -> Nothing
    lookupBlockByHeight blockHeight = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. blockStore) blockHeight
    lookupTransaction txHash = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. transactionStatusStore) txHash
    memberTransaction txHash = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. transactionStatusStore) txHash

    writeBlocks blocks fe = asWriteTransaction $ \dbh txn -> do
        forM_ blocks $ \block -> do
            let height = bmHeight (stbInfo block)
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
            (dbh ^. consensusStatusStore)
            CSKLatestFinalizationEntry
            (CSVLastFinalizationEntry fe)

    lookupLatestFinalizationEntry = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. consensusStatusStore) CSKLatestFinalizationEntry <&> \case
            Just (CSVLastFinalizationEntry fe) -> Just fe
            _ -> Nothing

    lookupCurrentRoundStatus = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. consensusStatusStore) CSKRoundStatus >>= \case
            Just (CSVRoundStatus rs) -> return rs
            _ -> throwM (DatabaseInvariantViolation "Missing current round status")

    writeCurrentRoundStatus rs = asWriteTransaction $ \dbh txn ->
        storeReplaceRecord txn (dbh ^. consensusStatusStore) CSKRoundStatus (CSVRoundStatus rs)

    rollBackBlocksUntil predicate = roll False
      where
        -- The boolean indicates whether blocks have already been rolled back.
        roll b = do
            getLast >>= \case
                Nothing -> return $ Right b
                Just (Left e) -> return $ Left $ "Could not load last finalized block: " ++ e
                Just (Right (h, sb)) -> do
                    ok <- predicate sb
                    if ok
                        then return $ Right b
                        else do
                            unless b $ do
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
                                unless b . void $
                                    deleteRecord
                                        txn
                                        (dbh ^. consensusStatusStore)
                                        CSKLatestFinalizationEntry
                                -- Remove the block
                                _ <- deleteRecord txn (dbh ^. blockStore) h
                                _ <- deleteRecord txn (dbh ^. blockHashIndex) (getHash sb)
                                -- Remove the block transactions
                                forM_ (blockTransactions sb) $ \tx ->
                                    deleteRecord txn (dbh ^. transactionStatusStore) (getHash tx)
                            roll True
        getLast = asReadTransaction $ \dbh txn ->
            withCursor txn (dbh ^. blockStore) (getCursor CursorLast)
