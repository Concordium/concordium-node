{-# LANGUAGE BangPatterns #-}
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
import Data.List (intercalate)
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory

import Concordium.Common.Version
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.LMDB.Helpers
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- * Exceptions

-- |Exception occurring from a violation of database invariants in the LMDB database.
newtype DatabaseInvariantViolation = DatabaseInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseInvariantViolation where
    displayException (DatabaseInvariantViolation reason) =
        "Database invariant violation: "
            ++ show reason

newtype DatabaseRecoveryFailure = DatabaseRecoveryFailure String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseRecoveryFailure where
    displayException (DatabaseRecoveryFailure reason) =
        "Database recovery failed: " ++ show reason

-- * Database tables

-- ** Block store

-- |Block store for certified blocks by hash.
newtype BlockStore (pv :: ProtocolVersion) = BlockStore MDB_dbi'

instance IsProtocolVersion pv => MDBDatabase (BlockStore pv) where
    type DBKey (BlockStore pv) = BlockHash
    type DBValue (BlockStore pv) = StoredBlock pv
    encodeKey _ = Hash.hashToByteString . blockHash

-- ** Finalized blocks by height index

-- |Index mapping block hashes to block heights.
newtype FinalizedBlockIndex = FinalizedBlockIndex MDB_dbi'

instance MDBDatabase FinalizedBlockIndex where
    type DBKey FinalizedBlockIndex = BlockHeight
    type DBValue FinalizedBlockIndex = BlockHash

-- ** Transaction status

-- |A transaction status store table. A @TransactionStatusStore@ stores
-- 'FinalizedTransactionStatus'es indexed by 'TransactionHash'.
newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

instance MDBDatabase TransactionStatusStore where
    type DBKey TransactionStatusStore = TransactionHash
    type DBValue TransactionStatusStore = FinalizedTransactionStatus

-- ** Non-finalized quorum certificates

-- |A table of quorum certificates for non-finalized blocks, indexed by round number.
-- Except at genesis, this table should always have a minimal entry for the round after the round
-- of the last finalized block.
newtype NonFinalizedQuorumCertificateStore = NonFinalizedQuorumCertificateStore MDB_dbi'

instance MDBDatabase NonFinalizedQuorumCertificateStore where
    type DBKey NonFinalizedQuorumCertificateStore = Round
    type DBValue NonFinalizedQuorumCertificateStore = QuorumCertificate

-- ** Consensus status

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
      -- |Blocks by hash.
      _blockStore :: !(BlockStore pv),
      -- |Index of finalized blocks by block height.
      _finalizedBlockIndex :: !FinalizedBlockIndex,
      -- |Index of finalized transactions by hash.
      _transactionStatusStore :: !TransactionStatusStore,
      -- |Non-finalized quorum certificates index by round.
      _nonFinalizedQuorumCertificateStore :: !NonFinalizedQuorumCertificateStore,
      -- |Storage for the 'RoundStatus'.
      _roundStatusStore :: !RoundStatusStore,
      -- |Storage for the latest 'FinalizationEntry'.
      _latestFinalizationEntryStore :: !LatestFinalizationEntryStore,
      -- |Metadata storage (i.e. version information).
      _metadataStore :: !MetadataStore
    }

makeClassy ''DatabaseHandlers

-- |Name of the table used for storing blocks by block hash.
blockStoreName :: String
blockStoreName = "blocksByHash"

-- |Name of the table used for indexing finalized blocks by height.
finalizedBlockIndexName :: String
finalizedBlockIndexName = "finalizedBlockIndex"

-- |Name of the table used for indexing transactions by hash.
transactionStatusStoreName :: String
transactionStatusStoreName = "transactionStatus"

-- |Name of the table used for storing quorum certificates for non-finalized blocks by round.
nonFinalizedQuorumCertificateStoreName :: String
nonFinalizedQuorumCertificateStoreName = "nonFinalizedQuorumCertificates"

-- |Name of the table used for storing the round status and latest finalization entry.
consensusStatusStoreName :: String
consensusStatusStoreName = "consensusStatus"

-- |Name of the table used for storing version metadata.
metadataStoreName :: String
metadataStoreName = "metadata"

-- |The number of databases in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 6

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
        _finalizedBlockIndex <-
            FinalizedBlockIndex
                <$> mdb_dbi_open'
                    txn
                    (Just finalizedBlockIndexName)
                    [MDB_CREATE | not readOnly]
        _transactionStatusStore <-
            TransactionStatusStore
                <$> mdb_dbi_open'
                    txn
                    (Just transactionStatusStoreName)
                    [MDB_CREATE | not readOnly]
        _nonFinalizedQuorumCertificateStore <-
            NonFinalizedQuorumCertificateStore
                <$> mdb_dbi_open'
                    txn
                    (Just nonFinalizedQuorumCertificateStoreName)
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
openDatabase treeStateDir = do
    createDirectoryIfMissing False treeStateDir
    makeDatabaseHandlers treeStateDir False dbInitSize

-- |Close the database. The database should not be used after it is closed.
closeDatabase :: DatabaseHandlers pv -> IO ()
closeDatabase dbHandlers = runInBoundThread $ mdb_env_close $ dbHandlers ^. storeEnv . seEnv

-- |Check that the database version matches the expected version.
-- If it does not, this throws a 'DatabaseInvariantViolation' exception.
checkDatabaseVersion :: forall pv. IsProtocolVersion pv => DatabaseHandlers pv -> LogIO ()
checkDatabaseVersion db = do
    metadata <- liftIO . transaction (db ^. storeEnv) True $ \txn ->
        loadRecord txn (db ^. metadataStore) versionMetadata
    case metadata of
        Nothing ->
            throwM . DatabaseInvariantViolation $
                "no version data was found, but expected " ++ show expectedVersion
        Just vs -> case S.decode vs of
            Right vm
                | vm == expectedVersion -> do
                    logEvent LMDB LLTrace $ "Database version: " ++ show vm
                | otherwise ->
                    throwM . DatabaseInvariantViolation $
                        "database version is " ++ show vm ++ " but expected " ++ show expectedVersion
            _ ->
                throwM . DatabaseInvariantViolation $
                    "version data could not be deserialized, but expected " ++ show expectedVersion
  where
    expectedVersion =
        VersionMetadata
            { vmDatabaseVersion = 1,
              vmProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
            }

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
                            _finalizedBlockIndex <-
                                FinalizedBlockIndex
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just finalizedBlockIndexName)
                                        []
                            _transactionStatusStore <-
                                TransactionStatusStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just transactionStatusStoreName)
                                        []
                            _nonFinalizedQuorumCertificateStore <-
                                NonFinalizedQuorumCertificateStore
                                    <$> mdb_dbi_open'
                                        txn
                                        (Just nonFinalizedQuorumCertificateStoreName)
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

-- |Helper function for implementing 'writeFinalizedBlocks'.
writeFinalizedBlocksHelper ::
    (HasDatabaseHandlers dbh pv, IsProtocolVersion pv) =>
    [StoredBlock pv] ->
    FinalizationEntry ->
    dbh ->
    MDB_txn ->
    IO [BlockHash]
writeFinalizedBlocksHelper finBlocks finEntry dbh txn = do
    -- Delete the quorum certificates for rounds up to and including the round of the
    -- new last finalized block. Where the QCs are for blocks that will not be finalized,
    -- also remove the block from the block table.
    delBlocks <- withCursor txn (dbh ^. nonFinalizedQuorumCertificateStore) $ \cursor -> do
        let loop del fins Nothing = do
                forM_ fins $ \fin ->
                    storeReplaceRecord txn (dbh ^. blockStore) (getHash fin) fin
                return del
            loop _ _ (Just (Left e)) = throwM (DatabaseInvariantViolation e)
            loop del fins@(nextFin : restFin) nextQC@(Just (Right (rnd, qc))) = do
                -- If the block is in a lesser round than the next block in the finalized list,
                -- remove it from the block table (since it is now dead).
                case compare rnd (blockRound nextFin) of
                    LT -> do
                        _ <- deleteRecord txn (dbh ^. blockStore) (qcBlock qc)
                        -- Delete the QC entry.
                        deleteAtCursor cursor
                        loop (qcBlock qc : del) fins =<< getCursor CursorNext cursor
                    EQ -> do
                        -- Delete the QC entry.
                        deleteAtCursor cursor
                        if null restFin
                            then return del
                            else loop del restFin =<< getCursor CursorNext cursor
                    GT -> do
                        storeReplaceRecord txn (dbh ^. blockStore) (getHash nextFin) nextFin
                        loop del restFin nextQC
            loop del [] _ = return del
        loop [] finBlocks =<< getCursor CursorFirst cursor
    -- Write the latest finalization entry
    storeReplaceRecord
        txn
        (dbh ^. latestFinalizationEntryStore)
        CSKLatestFinalizationEntry
        finEntry
    -- Index the newly-finalized blocks and their transactions
    forM_ finBlocks $ \block -> do
        let height = blockHeight block
        -- Write the block to the finalized index
        storeReplaceRecord txn (dbh ^. finalizedBlockIndex) height (getHash block)
        -- Write the block's transactions to the transaction status index.
        forM_ (zip (blockTransactions block) [0 ..]) $ \(tx, ti) ->
            storeReplaceRecord
                txn
                (dbh ^. transactionStatusStore)
                (getHash tx)
                (FinalizedTransactionStatus height ti)
    return delBlocks

writeCertifiedBlockHelper ::
    ( HasDatabaseHandlers s pv,
      IsProtocolVersion pv
    ) =>
    StoredBlock pv ->
    QuorumCertificate ->
    s ->
    MDB_txn ->
    IO ()
writeCertifiedBlockHelper certBlock qc dbh txn = do
    -- Add the certified block
    storeReplaceRecord txn (dbh ^. blockStore) (getHash certBlock) certBlock
    -- Add the new QC
    storeReplaceRecord txn (dbh ^. nonFinalizedQuorumCertificateStore) (qcRound qc) qc

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
        loadRecord txn (dbh ^. blockStore) bh
    memberBlock bh = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. blockStore) bh
    lookupFirstBlock = lookupBlockByHeight 0
    lookupLastFinalizedBlock = asReadTransaction $ \dbh txn ->
        withCursor txn (dbh ^. finalizedBlockIndex) (getCursor CursorLast) >>= \case
            Just (Right (_, bh)) ->
                loadRecord txn (dbh ^. blockStore) bh
            _ -> return Nothing
    lookupBlockByHeight height = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. finalizedBlockIndex) height >>= \case
            Just bh -> loadRecord txn (dbh ^. blockStore) bh
            _ -> return Nothing
    lookupTransaction txHash = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. transactionStatusStore) txHash
    memberTransaction txHash = asReadTransaction $ \dbh txn ->
        isRecordPresent txn (dbh ^. transactionStatusStore) txHash

    writeFinalizedBlocks finBlocks finEntry = do
        delBlocks <- asWriteTransaction $ writeFinalizedBlocksHelper finBlocks finEntry
        logEvent LMDB LLTrace $
            "Finalized blocks: "
                ++ show (getHash @BlockHash <$> finBlocks)
                ++ ". Deleted blocks: "
                ++ show delBlocks
        return ()

    writeCertifiedBlock certBlock qc = do
        asWriteTransaction $ writeCertifiedBlockHelper certBlock qc
        logEvent LMDB LLTrace $
            "Certified block: "
                ++ show (qcBlock qc)
                ++ " (height "
                ++ show (blockHeight certBlock)
                ++ ")"

    writeCertifiedBlockWithFinalization finBlocks certBlock finEntry = do
        delBlocks <- asWriteTransaction $ \dbh txn -> do
            delBlocks <- writeFinalizedBlocksHelper finBlocks finEntry dbh txn
            writeCertifiedBlockHelper certBlock (feSuccessorQuorumCertificate finEntry) dbh txn
            return delBlocks
        logEvent LMDB LLTrace $
            "Finalized blocks: "
                ++ show (getHash @BlockHash <$> finBlocks)
                ++ ". Deleted blocks: "
                ++ show delBlocks
        logEvent LMDB LLTrace $
            "Certified block: "
                ++ show (qcBlock (feSuccessorQuorumCertificate finEntry))
                ++ " (height "
                ++ show (blockHeight certBlock)
                ++ ")"

    lookupLatestFinalizationEntry = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. latestFinalizationEntryStore) CSKLatestFinalizationEntry

    lookupCertifiedBlocks = asReadTransaction $ \dbh txn -> do
        withCursor txn (dbh ^. nonFinalizedQuorumCertificateStore) $ \cursor -> do
            let loop l Nothing = return l
                loop _ (Just (Left e)) = throwM . DatabaseInvariantViolation $ e
                loop l (Just (Right (_, qc))) = do
                    loadRecord txn (dbh ^. blockStore) (qcBlock qc) >>= \case
                        Nothing ->
                            throwM . DatabaseInvariantViolation $
                                "Missing block for QC "
                                    <> show (qcBlock qc)
                                    <> " in round "
                                    <> show (qcRound qc)
                        Just block ->
                            loop ((block, qc) : l) =<< getCursor CursorPrevious cursor
            loop [] =<< getCursor CursorLast cursor

    lookupCurrentRoundStatus = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. roundStatusStore) CSKRoundStatus >>= \case
            Just rs -> return rs
            _ -> throwM (DatabaseInvariantViolation "Missing current round status")

    writeCurrentRoundStatus rs = asWriteTransaction $ \dbh txn ->
        storeReplaceRecord txn (dbh ^. roundStatusStore) CSKRoundStatus rs

-- |Initialise the low-level database by writing out the genesis block, initial round status and
-- version metadata.
initialiseLowLevelDB ::
    forall pv r m.
    (MonadIO m, MonadReader r m, HasDatabaseHandlers r pv, MonadLogger m, IsProtocolVersion pv) =>
    -- |Genesis block.
    StoredBlock pv ->
    -- |Initial persistent round status.
    PersistentRoundStatus ->
    DiskLLDBM pv m ()
initialiseLowLevelDB genesisBlock roundStatus = asWriteTransaction $ \dbh txn -> do
    storeReplaceRecord txn (dbh ^. blockStore) (getHash genesisBlock) genesisBlock
    storeReplaceRecord txn (dbh ^. finalizedBlockIndex) 0 (getHash genesisBlock)
    storeReplaceRecord txn (dbh ^. roundStatusStore) CSKRoundStatus roundStatus
    let metadata =
            VersionMetadata
                { vmDatabaseVersion = 1,
                  vmProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
                }
    storeReplaceRecord txn (dbh ^. metadataStore) versionMetadata $ S.encode metadata

-- |Remove certified and finalized blocks from the database whose states cannot be loaded.
-- This can throw an exception if the database recovery was not possible.
--
-- This uses the following assumptions:
--   * If a block's state can be loaded, then so can the state of its parent, or any block
--     with a lesser 'BlockStateRef'.
--   * The database invariants hold.
--   * No other concurrent accesses are made to the database.
--
-- The updates to the database are performed in the following write transactions:
--   * Removing a single certified block.
--   * Removing all certified blocks.
--   * Removing the last finalized block and implicitly finalized blocks, rolling back the
--     latest finalization entry to the prior explicitly finalized block (or removing it if
--     it would be for the genesis block).
rollBackBlocksUntil ::
    forall pv r m.
    ( IsProtocolVersion pv,
      MonadReader r m,
      HasDatabaseHandlers r pv,
      MonadIO m,
      MonadCatch m,
      MonadLogger m
    ) =>
    -- |Callback for checking if the state at a given reference is valid.
    (BlockStateRef pv -> DiskLLDBM pv m Bool) ->
    DiskLLDBM pv m (Int, BlockStateRef pv)
rollBackBlocksUntil checkState = do
    lookupLastFinalizedBlock >>= \case
        Nothing -> throwM . DatabaseRecoveryFailure $ "No last finalized block."
        Just lastFin -> do
            stateOK <- checkState (stbStatePointer lastFin)
            if stateOK
                then do
                    -- The last finalized block is intact, so check the certified blocks.
                    checkCertified (blockRound lastFin) (stbStatePointer lastFin)
                else do
                    -- The last finalized block is not intact, so roll back all of the
                    -- certified blocks, then roll back finalized blocks.
                    count <- purgeCertified
                    rollFinalized count lastFin
  where
    -- Check the non-finalized certified blocks, from the highest round backwards.
    checkCertified ::
        -- last finalized round
        Round ->
        -- highest surviving block state so far (from last finalized block)
        BlockStateRef pv ->
        -- returns the number of blocks rolled back and the highest surviving block state
        DiskLLDBM pv m (Int, BlockStateRef pv)
    checkCertified lastFinRound bestState = do
        mHighestQC <- asReadTransaction $ \dbh txn ->
            withCursor
                txn
                (dbh ^. nonFinalizedQuorumCertificateStore)
                (getCursor CursorLast)
        case mHighestQC of
            Nothing -> return (0, bestState)
            Just (Left e) -> throwM . DatabaseRecoveryFailure $ e
            Just (Right (_, qc)) -> checkCertifiedWithQC lastFinRound bestState 0 qc
    -- Given the round and QC for a certified block, check that the block's state can be
    -- loaded, and then iterate for the previous round.
    checkCertifiedWithQC ::
        -- last finalized round
        Round ->
        -- highest surviving block state so far
        BlockStateRef pv ->
        -- number of blocks rolled back so far
        Int ->
        -- QC for certified block to check
        QuorumCertificate ->
        -- returns the number of blocks rolled back and the highest surviving block state
        DiskLLDBM pv m (Int, BlockStateRef pv)
    checkCertifiedWithQC lastFinRound bestState !count qc = do
        mBlock <- asReadTransaction $ \dbh txn ->
            loadRecord txn (dbh ^. blockStore) (qcBlock qc)
        case mBlock of
            Nothing ->
                throwM . DatabaseRecoveryFailure $
                    "Missing block entry for quorum certificate"
            Just block -> do
                stateOK <- checkState (stbStatePointer block)
                if stateOK
                    then do
                        checkCertifiedPreviousRound
                            lastFinRound
                            (max bestState (stbStatePointer block))
                            count
                            (qcRound qc - 1)
                    else do
                        -- Delete the block and the QC
                        asWriteTransaction $ \dbh txn -> do
                            void $
                                deleteRecord
                                    txn
                                    (dbh ^. nonFinalizedQuorumCertificateStore)
                                    (qcRound qc)
                            void $
                                deleteRecord txn (dbh ^. blockStore) (qcBlock qc)
                        logEvent LMDB LLDebug $
                            "The block state for certified block "
                                <> show (qcBlock qc)
                                <> " is corrupted. The certified block was deleted."
                        checkCertifiedPreviousRound
                            lastFinRound
                            bestState
                            (count + 1)
                            (qcRound qc - 1)
    -- Step the non-finalized certified block check to the previous round.
    checkCertifiedPreviousRound ::
        -- last finalized round
        Round ->
        -- highest surviving block so far
        BlockStateRef pv ->
        -- number of blocks rolled back so far
        Int ->
        -- round to check for
        Round ->
        -- returns the number of blocks rolled back and the highest surviving block state
        DiskLLDBM pv m (Int, BlockStateRef pv)
    checkCertifiedPreviousRound lastFinRound bestState count currentRound
        | currentRound <= lastFinRound = return (count, bestState)
        | otherwise = do
            mNextQC <- asReadTransaction $ \dbh txn ->
                loadRecord txn (dbh ^. nonFinalizedQuorumCertificateStore) currentRound
            case mNextQC of
                Nothing ->
                    checkCertifiedPreviousRound lastFinRound bestState count (currentRound - 1)
                Just qc ->
                    checkCertifiedWithQC lastFinRound bestState count qc
    -- Purge all of the certified blocks. Returns the number of blocks rolled back.
    purgeCertified = do
        (count, hashes) <- asWriteTransaction $ \dbh txn -> do
            withCursor txn (dbh ^. nonFinalizedQuorumCertificateStore) $ \cursor -> do
                let loop !count hashes Nothing = return (count, hashes)
                    loop _ _ (Just (Left e)) = throwM . DatabaseRecoveryFailure $ e
                    loop !count hashes (Just (Right (_, qc))) = do
                        _ <- deleteRecord txn (dbh ^. blockStore) (qcBlock qc)
                        -- Delete the QC entry.
                        deleteAtCursor cursor
                        loop (count + 1) (qcBlock qc : hashes) =<< getCursor CursorNext cursor
                loop 0 [] =<< getCursor CursorFirst cursor
        logEvent LMDB LLDebug $
            "The block state for the last finalized block was corrupted. \
            \The following certified blocks were deleted: "
                <> intercalate ", " (show <$> hashes)
                <> "."
        return count
    -- Roll back finalized blocks until the last explicitly finalized block where the state
    -- check passes.
    rollFinalized count lastFin = do
        when (blockRound lastFin == 0) $
            throwM . DatabaseRecoveryFailure $
                "Genesis block state could not be recovered."
        (count', hashes, newLastFin) <- asWriteTransaction $ \dbh txn -> do
            let loop ::
                    -- Current count of blocks rolled back
                    Int ->
                    -- Accumulated list of rolled-back finalized blocks
                    [BlockHash] ->
                    -- Block to roll back
                    StoredBlock pv ->
                    -- Quorum certificate on the block
                    QuorumCertificate ->
                    -- Total number of blocks rolled back,
                    -- List of hashes of rolled-back blocks,
                    -- New last finalized block
                    IO (Int, [BlockHash], StoredBlock pv)
                loop !c hashes fin finQC = case stbBlock fin of
                    GenesisBlock _ -> do
                        -- As a special case, the genesis block is self-finalizing.
                        -- We thus remove the latest finalization entry.
                        _ <-
                            deleteRecord
                                txn
                                (dbh ^. latestFinalizationEntryStore)
                                CSKLatestFinalizationEntry
                        return (c, hashes, fin)
                    NormalBlock block -> do
                        -- Remove the block and its transactions
                        let finHash = getHash fin
                        _ <- deleteRecord txn (dbh ^. blockStore) finHash
                        _ <- deleteRecord txn (dbh ^. finalizedBlockIndex) (blockHeight fin)
                        forM_ (blockTransactions fin) $
                            deleteRecord txn (dbh ^. transactionStatusStore) . getHash
                        mparent <- loadRecord txn (dbh ^. blockStore) (blockParent block)
                        case mparent of
                            Nothing ->
                                throwM . DatabaseInvariantViolation $
                                    "Missing parent of finalized block."
                            Just parent ->
                                if isPresent (blockTimeoutCertificate block)
                                    || isPresent (blockEpochFinalizationEntry block)
                                    then do
                                        -- If the block has a timeout certificate or an epoch
                                        -- finalization entry, then the QC on this block does not
                                        -- explicitly finalize the parent block, so we roll back.
                                        -- (Note, the presence of a timeout certificate indicates the
                                        -- block is not in the next round from its parent, and the
                                        -- presence of an epoch finalization entry indicates that block
                                        -- is not in the same epoch as its parent. Both of these are
                                        -- necessary for the QC to imply finalization.)
                                        loop
                                            (c + 1)
                                            (finHash : hashes)
                                            parent
                                            (blockQuorumCertificate block)
                                    else do
                                        -- The block does not have a timeout certificate or epoch
                                        -- finalization entry, so the parent block is explicitly
                                        -- finalized. We thus set the latest finalization entry to
                                        -- finalize the parent block so that the database invariant
                                        -- is restored.
                                        storeReplaceRecord
                                            txn
                                            (dbh ^. latestFinalizationEntryStore)
                                            CSKLatestFinalizationEntry
                                            FinalizationEntry
                                                { feFinalizedQuorumCertificate =
                                                    blockQuorumCertificate block,
                                                  feSuccessorQuorumCertificate = finQC,
                                                  feSuccessorProof = getHash (sbBlock block)
                                                }
                                        return (c + 1, finHash : hashes, parent)
            loadRecord txn (dbh ^. latestFinalizationEntryStore) CSKLatestFinalizationEntry
                >>= \case
                    Nothing ->
                        throwM . DatabaseInvariantViolation $
                            "Missing latest finalization entry."
                    Just lfe
                        | qcBlock (feFinalizedQuorumCertificate lfe) /= getHash lastFin ->
                            throwM . DatabaseInvariantViolation $
                                "Latest finalization entry does not match last finalized block."
                        | otherwise ->
                            loop count [] lastFin (feFinalizedQuorumCertificate lfe)
        logEvent LMDB LLDebug $
            "The state for finalized block "
                <> show (getHash @BlockHash lastFin)
                <> " was corrupted. The following finalized blocks were deleted: "
                <> intercalate ", " (show <$> hashes)
                <> ". The new last finalized block is "
                <> show (getHash @BlockHash newLastFin)
                <> "."
        stateOK <- checkState (stbStatePointer newLastFin)
        if stateOK
            then return (count', stbStatePointer newLastFin)
            else rollFinalized count' newLastFin
