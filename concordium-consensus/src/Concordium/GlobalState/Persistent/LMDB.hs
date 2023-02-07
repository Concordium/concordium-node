{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module provides an abstraction over the operations done in the LMDB database that serves as a backend for storing blocks and finalization records.
module Concordium.GlobalState.Persistent.LMDB (
    DatabaseHandlers (..),
    HasDatabaseHandlers (..),
    FinalizedTransactionStatus (..),
    finalizedToTransactionStatus,
    storeEnv,
    blockStore,
    finalizationRecordStore,
    transactionStatusStore,
    databaseHandlers,
    makeDatabaseHandlers,
    initializeDatabase,
    VersionDatabaseHandlers (..),
    openReadOnlyDatabase,
    closeDatabase,
    addDatabaseVersion,
    checkDatabaseVersion,
    resizeOnResized,
    finalizedByHeightStore,
    StoredBlock (..),
    readBlock,
    readFinalizationRecord,
    readTransactionStatus,
    readFinalizedBlockAtHeight,
    memberTransactionTable,
    memberBlockStore,
    loadBlocksFinalizationIndexes,
    getFinalizedBlockAtHeight,
    getLastBlock,
    getFirstBlock,
    writeBlock,
    writeFinalizationRecord,
    writeTransactionStatus,
    writeTransactionStatuses,
    writeFinalizationComposite,
    unrollTreeStateWhile,
    FixedSizeSerialization,
) where

import Concordium.Common.Version
import Concordium.Crypto.SHA256
import Concordium.Genesis.Data (getGenesisConfiguration)
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.LMDB.Helpers
import Concordium.GlobalState.Persistent.BlobStore (BlobRef)
import Concordium.GlobalState.Persistent.BlockPointer
import qualified Concordium.GlobalState.TransactionTable as T
import Concordium.Logger
import Concordium.Types
import Concordium.Types.Execution (TransactionIndex)
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Control.Arrow ((&&&))
import Control.Concurrent (runInBoundThread)
import Control.Monad
import Control.Monad.Catch (MonadCatch, handleJust, tryJust)
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.Proxy
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory

-- |A (finalized) block as stored in the database.
data StoredBlock pv st = StoredBlock
    { sbFinalizationIndex :: !FinalizationIndex,
      sbInfo :: !BasicBlockPointerData,
      sbBlock :: !(Block pv),
      sbState :: !st
    }

-- Note: 'openReadOnlyDatabase' works on the presumption that the state is always the last part of
-- the serialization, so we can serialize a stored block with any state type and deserialize it
-- with the unit state type.  Any changes to the serialization used here must respect this or
-- be accompanied by corresponding changes there.
putStoredBlock :: forall pv st. (IsProtocolVersion pv, S.Serialize st) => S.Putter (StoredBlock pv st)
putStoredBlock StoredBlock{..} =
    S.put sbFinalizationIndex
        <> S.put sbInfo
        <> putBlock (protocolVersion @pv) sbBlock
        <> S.put sbState

-- |A block store table. A @BlockStore pv st@ stores @StoredBlock pv st@ blocks
-- indexed by 'BlockHash'.
newtype BlockStore (pv :: ProtocolVersion) st = BlockStore MDB_dbi'

-- |A refinement of 'Serialize' that indicates that the size (i.e., amount of
-- bytes used) of the serialized value is independend of the value. This must be
-- exact, not just an upper bound.
class S.Serialize a => FixedSizeSerialization a where
    -- |Return the size of serialized values in bytes.
    serializedSize :: Proxy a -> Int

instance FixedSizeSerialization () where
    serializedSize _ = 0

instance FixedSizeSerialization (BlobRef a) where
    serializedSize _ = 8

-- This instance is needed for paired state.
instance (FixedSizeSerialization a, FixedSizeSerialization b) => FixedSizeSerialization (a, b) where
    serializedSize _ = serializedSize (Proxy @a) + serializedSize (Proxy @b)

-- |Decode a stored block given access to arrival time and the hash of the genesis block.
-- The latter is only used when deserializing genesis blocks.
--
-- It is crucial that the result does not retain any pointers to the byte array
-- from which the data is deserialized.
--
-- Note that in general when given a genesis block this function will not fully
-- consume the input since it only needs to parse the genesis parameters.
decodeStoredBlock :: SProtocolVersion pv -> TransactionTime -> BlockHash -> S.Get (Block pv)
decodeStoredBlock spv arrivalTime genHash = do
    sl <- S.get
    if sl == 0
        then GenesisBlock <$> getGenesisConfiguration spv genHash
        else NormalBlock <$> getBakedBlockAtSlot spv sl arrivalTime

instance (IsProtocolVersion pv, FixedSizeSerialization st) => MDBDatabase (BlockStore pv st) where
    type DBKey (BlockStore pv st) = BlockHash
    type DBValue (BlockStore pv st) = StoredBlock pv st
    encodeKey _ = hashToByteString . blockHash
    encodeValue _ sb = S.runPutLazy (putStoredBlock sb)
    decodeValue _ blockHash mdbVal = do
        -- The use of unsafeByteStringFromMDB_val is OK here since deserialization of blocks
        -- does not retain any pointers to the source. All the data and byte strings are copied
        -- (cf getByteString).
        bs <- unsafeByteStringFromMDB_val mdbVal
        if BS.length bs < serializedSize (Proxy @st)
            then return (Left "Unexpected block value without state.")
            else do
                let (body, stateValue) = BS.splitAt (BS.length bs - serializedSize (Proxy @st)) bs
                    bodyDecoder :: S.Get (FinalizationIndex, BasicBlockPointerData, Block pv)
                    bodyDecoder = do
                        sbFinalizationIndex <- S.get
                        sbInfo <- S.get
                        sbBlock <- S.label "decodeStoredBlock" $ decodeStoredBlock (protocolVersion @pv) (utcTimeToTransactionTime (_bpReceiveTime sbInfo)) blockHash
                        return (sbFinalizationIndex, sbInfo, sbBlock)
                case (S.runGet bodyDecoder body, S.decode stateValue) of
                    (Right (sbFinalizationIndex, sbInfo, sbBlock), Right sbState) -> return (Right StoredBlock{..})
                    (Right _, Left err) -> return (Left $ "Cannot decode state: " ++ err)
                    (Left err, Right _) -> return (Left $ "Cannot decode block: " ++ err)
                    (Left err1, Left err2) -> return (Left $ "Cannot decode block nor state: " ++ err1 ++ ", " ++ err2)

-- |A finalization record store table. A @FinalizationRecordStore@ stores
-- 'FinalizationRecord's indexed by 'FinalizationIndex'.
newtype FinalizationRecordStore = FinalizationRecordStore MDB_dbi'

instance MDBDatabase FinalizationRecordStore where
    type DBKey FinalizationRecordStore = FinalizationIndex
    type DBValue FinalizationRecordStore = FinalizationRecord

-- |A transaction status store table. A @TransactionStatusStore@ stores
-- 'FinalizedTransactionStatus'es indexed by 'TransactionHash'.
newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

-- |Details about a finalized transaction.
data FinalizedTransactionStatus = FinalizedTransactionStatus
    { -- |Slot number of the finalized block in which the transaction occurred.
      ftsSlot :: !Slot,
      -- |Hash of the finalized block in which the transaction occurred.
      ftsBlockHash :: !BlockHash,
      -- |Index of the transaction in the block.
      ftsIndex :: !TransactionIndex
    }
    deriving (Eq, Show)

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

-- |Index of block hashes by finalized height.
newtype FinalizedByHeightStore = FinalizedByHeightStore MDB_dbi'

instance MDBDatabase FinalizedByHeightStore where
    type DBKey FinalizedByHeightStore = BlockHeight

    type DBValue FinalizedByHeightStore = BlockHash
    encodeValue _ = LBS.fromStrict . hashToByteString . blockHash

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

-- |Values used by the LMDBStoreMonad to manage the database.
-- The type is parametrised by the protocol version and the block state type.
data DatabaseHandlers (pv :: ProtocolVersion) st = DatabaseHandlers
    { -- |LMDB store environment with a reader-writer lock..
      _storeEnv :: !StoreEnv,
      -- |Store of blocks by hash.
      _blockStore :: !(BlockStore pv st),
      -- |Store of finalization records by index.
      _finalizationRecordStore :: !FinalizationRecordStore,
      -- |Index of transaction references by transaction hash.
      _transactionStatusStore :: !TransactionStatusStore,
      -- |Index of block hashes by block height.
      _finalizedByHeightStore :: !FinalizedByHeightStore,
      -- |Metadata store.
      _metadataStore :: !MetadataStore
    }

makeLenses ''DatabaseHandlers

-- |Class for a state that includes 'DatabaseHandlers'.
-- The first type parameter is the protocol version.
-- The second type parameter is the block state type.
-- The third parameter is the state that has the 'DatabaseHandlers'.
class HasDatabaseHandlers (pv :: ProtocolVersion) st s | s -> pv st where
    dbHandlers :: Lens' s (DatabaseHandlers pv st)

instance HasDatabaseHandlers pv st (DatabaseHandlers pv st) where
    dbHandlers = id

-- |Name of the block store.
blockStoreName :: String
blockStoreName = "blocks"

-- |Name of the finalization record store.
finalizationRecordStoreName :: String
finalizationRecordStoreName = "finalization"

-- |Name of the finalization-by-height index.
finalizedByHeightStoreName :: String
finalizedByHeightStoreName = "finalizedByHeight"

-- |Name of the transaction status index.
transactionStatusStoreName :: String
transactionStatusStoreName = "transactionstatus"

-- |Name of the metadata store.
metadataStoreName :: String
metadataStoreName = "metadata"

-- |The number of databases in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 5

-- |Database growth size increment.
-- This is currently set at 64MB, and must be a multiple of the page size.
dbStepSize :: Int
dbStepSize = 2 ^ (26 :: Int) -- 64MB

-- |Initial database size.
-- This is currently set to be the same as 'dbStepSize'.
dbInitSize :: Int
dbInitSize = dbStepSize

-- |Initialize database handlers in ReadWrite mode.
-- This simply loads the references and does not initialize the databases.
-- The initial size is set to 64MB.
databaseHandlers :: FilePath -> IO (DatabaseHandlers pv st)
databaseHandlers treeStateDir = makeDatabaseHandlers treeStateDir False dbInitSize

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
    IO (DatabaseHandlers pv st)
makeDatabaseHandlers treeStateDir readOnly initSize = do
    _storeEnv <- makeStoreEnv
    -- here nobody else has access to the environment, so we need not lock
    let env = _storeEnv ^. seEnv
    mdb_env_set_mapsize env (initSize + dbStepSize - initSize `mod` dbStepSize)
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    -- TODO: Consider MDB_NOLOCK
    mdb_env_open env treeStateDir [MDB_RDONLY | readOnly]
    transaction _storeEnv readOnly $ \txn -> do
        _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) [MDB_CREATE | not readOnly]
        _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) [MDB_CREATE | not readOnly]
        _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) [MDB_CREATE | not readOnly]
        _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) [MDB_CREATE | not readOnly]
        _metadataStore <- MetadataStore <$> mdb_dbi_open' txn (Just metadataStoreName) [MDB_CREATE | not readOnly]
        return DatabaseHandlers{..}

-- |'DatabaseHandlers' existentially quantified over the protocol version and without block state.
-- Note that we can treat the state type as '()' soundly when reading, since the state is the last
-- part of the serialization: we just ignore the remaining bytes.
data VersionDatabaseHandlers
    = forall pv.
        IsProtocolVersion pv =>
      VersionDatabaseHandlers (DatabaseHandlers pv ())

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
    -- TODO: Consider MDB_NOLOCK
    mdb_env_open env treeStateDir [MDB_RDONLY]
    (_metadataStore, mversion) <- resizeOnResizedInternal _storeEnv $ transaction _storeEnv True $ \txn -> do
        _metadataStore <- MetadataStore <$> mdb_dbi_open' txn (Just metadataStoreName) []
        mversion <- loadRecord txn _metadataStore versionMetadata
        return (_metadataStore, mversion)
    case mversion of
        Nothing -> Nothing <$ mdb_env_close env
        Just v -> case S.decode v of
            Right VersionMetadata{vmDatabaseVersion = 0, ..} ->
                -- Promote the term level vmProtocolVersion to a type-level value pv, which is
                -- existentially quantified in the return type.  We do not currently match on the
                -- protocol version itself, since the database handlers are parametric in the protocol
                -- version.
                case promoteProtocolVersion vmProtocolVersion of
                    SomeProtocolVersion (_ :: SProtocolVersion pv) ->
                        resizeOnResizedInternal _storeEnv $ transaction _storeEnv True $ \txn -> do
                            _blockStore <- BlockStore <$> mdb_dbi_open' txn (Just blockStoreName) []
                            _finalizationRecordStore <- FinalizationRecordStore <$> mdb_dbi_open' txn (Just finalizationRecordStoreName) []
                            _finalizedByHeightStore <- FinalizedByHeightStore <$> mdb_dbi_open' txn (Just finalizedByHeightStoreName) []
                            _transactionStatusStore <- TransactionStatusStore <$> mdb_dbi_open' txn (Just transactionStatusStoreName) []
                            return (Just (VersionDatabaseHandlers @pv DatabaseHandlers{..}))
            _ -> Nothing <$ mdb_env_close env

-- |Initialize the database handlers creating the databases if needed and writing the genesis block and its finalization record into the disk
initializeDatabase ::
    forall pv st bs.
    (IsProtocolVersion pv, FixedSizeSerialization st) =>
    -- |Genesis block pointer
    PersistentBlockPointer pv bs ->
    -- |Genesis block state
    st ->
    -- |Tree state directory
    FilePath ->
    IO (DatabaseHandlers pv st)
initializeDatabase gb stRef treeStateDir = do
    createDirectoryIfMissing False treeStateDir
    let storedGenesis =
            StoredBlock
                { sbFinalizationIndex = 0,
                  sbInfo = _bpInfo gb,
                  sbBlock = _bpBlock gb,
                  sbState = stRef
                }
    -- The initial mapsize needs to be high enough to allocate the genesis block and its finalization record or
    -- initialization would fail. Since a regenesis block can contain a serialization of the state, which may be
    -- arbitrarily large, to be safe we ensure that we have at least 1MB more than the size of the serialization
    -- of the genesis block.
    let initSize = fromIntegral $ LBS.length (S.runPutLazy (putStoredBlock storedGenesis)) + 1_048_576
    handlers@DatabaseHandlers{..} <- makeDatabaseHandlers treeStateDir False initSize
    let gbh = getHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
    transaction _storeEnv False $ \txn -> do
        storeRecord txn _finalizedByHeightStore 0 gbh
        storeRecord txn _blockStore gbh storedGenesis
        storeRecord txn _finalizedByHeightStore 0 gbh
        storeRecord txn _finalizationRecordStore 0 gbfin
        storeRecord txn _metadataStore versionMetadata $
            S.encode $
                VersionMetadata
                    { vmDatabaseVersion = 0,
                      vmProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
                    }
    return handlers

-- |Add a database version record to a database (if one is not already present).
-- The record indicates database version 0 and protocol version 'P1', which is appropriate when
-- migrating a database from an earlier version.
addDatabaseVersion :: (MonadLogger m, MonadIO m) => FilePath -> m ()
addDatabaseVersion treeStateDir = do
    handlers :: DatabaseHandlers 'P1 () <- liftIO $ makeDatabaseHandlers treeStateDir False dbInitSize
    handlers' <-
        execStateT
            ( resizeOnFull 4096 $ -- This size is mostly arbitrary, but should be enough to store the serialized metadata
                \h -> transaction (_storeEnv h) False $ \txn ->
                    storeRecord
                        txn
                        (_metadataStore h)
                        versionMetadata
                        ( S.encode
                            VersionMetadata
                                { vmDatabaseVersion = 0,
                                  vmProtocolVersion = P1
                                }
                        )
            )
            handlers
    liftIO $ closeDatabase handlers'

-- |Check whether the database version matches the expected version.
-- If the version does not match, the result is a string describing the problem.
checkDatabaseVersion :: forall pv st. IsProtocolVersion pv => DatabaseHandlers pv st -> IO (Either String ())
checkDatabaseVersion db =
    checkVersion
        <$> transaction
            (db ^. storeEnv)
            True
            (\txn -> loadRecord txn (db ^. metadataStore) versionMetadata)
  where
    expectedVersion =
        VersionMetadata
            { vmDatabaseVersion = 0,
              vmProtocolVersion = demoteProtocolVersion (protocolVersion @pv)
            }
    checkVersion Nothing = Left $ "expected " ++ show expectedVersion ++ " but no version was found"
    checkVersion (Just vs) = case S.decode vs of
        Right vm
            | vm == expectedVersion -> Right ()
            | otherwise -> Left $ "expected " ++ show expectedVersion ++ " but found " ++ show vm
        _ -> Left $ "expected " ++ show expectedVersion ++ " but the version could not be deserialized"

-- |Close down the database, freeing the file handles.
closeDatabase :: DatabaseHandlers pv st -> IO ()
closeDatabase db = runInBoundThread $ withWriteStoreEnv (db ^. storeEnv) mdb_env_close

-- The use of withWriteStoreEnv ensures that there are no outstanding transactions and cursors are closed.

-- |Resize the LMDB map if the file size has changed.
-- This is used to allow a secondary process that is reading the database
-- to handle resizes to the database that are made by the writer.
-- The supplied action will be executed. If it fails with an 'MDB_MAP_RESIZED'
-- error, then the map will be resized and the action retried.
resizeOnResized :: (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s, MonadCatch m) => m a -> m a
resizeOnResized a = do
    dbh <- use dbHandlers
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

resizeDatabaseHandlers :: (MonadIO m, MonadLogger m) => DatabaseHandlers pv st -> Int -> m ()
resizeDatabaseHandlers dbh size = do
    envInfo <- liftIO $ mdb_env_info (dbh ^. storeEnv . seEnv)
    let delta = size + (dbStepSize - size `mod` dbStepSize)
        oldMapSize = fromIntegral $ me_mapsize envInfo
        newMapSize = oldMapSize + delta
        _storeEnv = dbh ^. storeEnv
    logEvent LMDB LLDebug $ "Resizing database from " ++ show oldMapSize ++ " to " ++ show newMapSize
    liftIO . withWriteStoreEnv (dbh ^. storeEnv) $ flip mdb_env_set_mapsize newMapSize

-- |Read a block from the database by hash.
readBlock ::
    (MonadIO m, MonadState s m, IsProtocolVersion pv, HasDatabaseHandlers pv st s, FixedSizeSerialization st) =>
    BlockHash ->
    m (Maybe (StoredBlock pv st))
readBlock bh = do
    dbh <- use dbHandlers
    liftIO $
        transaction (dbh ^. storeEnv) True $
            \txn -> loadRecord txn (dbh ^. blockStore) bh

-- |Read a finalization record from the database by finalization index.
readFinalizationRecord ::
    (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    FinalizationIndex ->
    m (Maybe FinalizationRecord)
readFinalizationRecord finIndex = do
    dbh <- use dbHandlers
    liftIO $
        transaction (dbh ^. storeEnv) True $
            \txn -> loadRecord txn (dbh ^. finalizationRecordStore) finIndex

-- |Read the status of a transaction from the database by the transaction hash.
readTransactionStatus ::
    (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    TransactionHash ->
    m (Maybe FinalizedTransactionStatus)
readTransactionStatus txHash = do
    dbh <- use dbHandlers
    liftIO $
        transaction (dbh ^. storeEnv) True $
            \txn -> loadRecord txn (dbh ^. transactionStatusStore) txHash

-- |Get a block from the database by its height.
getFinalizedBlockAtHeight ::
    (IsProtocolVersion pv, FixedSizeSerialization st) =>
    DatabaseHandlers pv st ->
    BlockHeight ->
    IO (Maybe (StoredBlock pv st))
getFinalizedBlockAtHeight dbh bHeight = transaction (dbh ^. storeEnv) True $
    \txn -> do
        mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) bHeight
        join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash

-- |Read a block from the database by its height.
readFinalizedBlockAtHeight ::
    (MonadIO m, MonadState s m, IsProtocolVersion pv, HasDatabaseHandlers pv st s, FixedSizeSerialization st) =>
    BlockHeight ->
    m (Maybe (StoredBlock pv st))
readFinalizedBlockAtHeight bHeight = do
    dbh <- use dbHandlers
    liftIO $ getFinalizedBlockAtHeight dbh bHeight

-- |Check if the given key is in the on-disk transaction table.
memberTransactionTable ::
    (MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    TransactionHash ->
    m Bool
memberTransactionTable th = do
    dbh <- use dbHandlers
    liftIO $
        transaction (dbh ^. storeEnv) True $
            \txn -> isRecordPresent txn (dbh ^. transactionStatusStore) th

-- |Check if a block with the given hash is stored in the LMDB block store.
memberBlockStore ::
    (IsProtocolVersion pv, FixedSizeSerialization st, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    BlockHash ->
    m Bool
memberBlockStore bh = do
    dbh <- use dbHandlers
    liftIO $
        transaction (dbh ^. storeEnv) True $
            \txn -> isRecordPresent txn (dbh ^. blockStore) bh

-- |Build a table of a block finalization indexes for blocks.
loadBlocksFinalizationIndexes :: (IsProtocolVersion pv, FixedSizeSerialization st) => DatabaseHandlers pv st -> IO (Either String (HM.HashMap BlockHash FinalizationIndex))
loadBlocksFinalizationIndexes dbh = transaction (dbh ^. storeEnv) True $ \txn ->
    withPrimitiveCursor txn (mdbDatabase $ dbh ^. blockStore) $ \cursor -> do
        let
            loop Nothing cur = return (Right cur)
            loop (Just (keyv, valv)) cur =
                S.decode <$> byteStringFromMDB_val keyv >>= \case
                    Left s -> return $ Left $ "Failed reading block hash while loading blocks: " ++ s
                    Right bh ->
                        S.runGet S.get <$> byteStringFromMDB_val valv >>= \case
                            Left s -> return $ Left $ "Failed loading block finalization index for block " ++ show bh ++ ": " ++ s
                            Right finIndex -> do
                                let nxt = HM.insert bh finIndex cur
                                nxtRes <- getPrimitiveCursor CursorNext cursor
                                loop nxtRes nxt
        fstRes <- getPrimitiveCursor CursorFirst cursor
        loop fstRes HM.empty

-- |Get the last finalized block by block height.
getLastBlock :: (IsProtocolVersion pv, FixedSizeSerialization st) => DatabaseHandlers pv st -> IO (Either String (FinalizationRecord, StoredBlock pv st))
getLastBlock dbh = transaction (dbh ^. storeEnv) True $ \txn -> do
    mLastFin <- withCursor txn (dbh ^. finalizationRecordStore) $ getCursor CursorLast
    case mLastFin of
        Just (Right (_, finRec)) ->
            loadRecord txn (dbh ^. blockStore) (finalizationBlockPointer finRec) >>= \case
                Just block -> return $ Right (finRec, block)
                Nothing -> return . Left $ "Could not read last finalized block by hash " <> show (finalizationBlockPointer finRec)
        Just (Left s) -> return $ Left $ "Could not read last finalization record: " ++ s
        Nothing -> return $ Left "No last finalized block found"

-- |Get the first block
getFirstBlock :: (IsProtocolVersion pv, FixedSizeSerialization st) => DatabaseHandlers pv st -> IO (Maybe (StoredBlock pv st))
getFirstBlock dbh = transaction (dbh ^. storeEnv) True $ \txn -> do
    mbHash <- loadRecord txn (dbh ^. finalizedByHeightStore) 0
    join <$> mapM (loadRecord txn (dbh ^. blockStore)) mbHash

-- |Perform a database action that may require the database to be resized,
-- resizing the database if necessary. The size argument is only evaluated
-- if the resize actually happens.
resizeOnFull ::
    (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    -- |Additional size
    Int ->
    -- |Action that may require resizing
    (DatabaseHandlers pv st -> IO a) ->
    m a
resizeOnFull addSize a = do
    dbh <- use dbHandlers
    resizeOnFullInternal addSize dbh a

-- |Perform a database action that may require the database to be resized, resizing the database if
-- necessary. The difference with `resizeOnFull` is that this function takes database handlers as an
-- argument, instead of reading their value from `HasDatabaseHandlers`.
resizeOnFullInternal ::
    (MonadIO m, MonadLogger m) =>
    -- |Additional size
    Int ->
    -- |Database handlers
    DatabaseHandlers pv st ->
    -- |Action that may require resizing
    (DatabaseHandlers pv st -> IO b) ->
    m b
resizeOnFullInternal addSize dbh a = inner
  where
    inner = do
        r <- liftIO $ tryJust selectDBFullError (a dbh)
        case r of
            Left _ -> do
                -- Resize the database handlers, and try to add again in case the size estimate
                -- given by lmdbStoreTypeSize is off.
                resizeDatabaseHandlers dbh addSize
                inner
            Right res -> return res
    -- only handle the db full error and propagate other exceptions.
    selectDBFullError = \case
        (LMDB_Error _ _ (Right MDB_MAP_FULL)) -> Just ()
        _ -> Nothing

-- |Write a block to the database. Adds it both to the index by height and
-- by block hash.
writeBlock ::
    (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s, IsProtocolVersion pv, FixedSizeSerialization st) =>
    StoredBlock pv st ->
    m ()
writeBlock block = resizeOnFull blockSize $
    \dbh -> transaction (dbh ^. storeEnv) False $
        \txn -> do
            let b = sbInfo block
            storeReplaceRecord txn (dbh ^. finalizedByHeightStore) (_bpHeight b) (_bpHash b)
            storeReplaceRecord txn (dbh ^. blockStore) (_bpHash b) block
  where
    blockSize = 2 * digestSize + fromIntegral (LBS.length (S.runPutLazy (putStoredBlock block)))

-- |Write a finalization record to the database.
writeFinalizationRecord ::
    (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    FinalizationRecord ->
    m ()
writeFinalizationRecord finRec = resizeOnFull finRecSize $
    \dbh -> transaction (dbh ^. storeEnv) False $
        \txn ->
            storeReplaceRecord txn (dbh ^. finalizationRecordStore) (finalizationIndex finRec) finRec
  where
    finRecSize =
        let FinalizationProof vs _ = finalizationProof finRec
        in  -- key + finIndex + finBlockPointer + finProof (list of Word32s + BlsSignature.signatureSize) + finDelay
            digestSize + 64 + digestSize + (32 * Prelude.length vs) + 48 + 64

-- |Write a single transaction status to the database.
writeTransactionStatus ::
    (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    TransactionHash ->
    FinalizedTransactionStatus ->
    m ()
writeTransactionStatus th ts = resizeOnFull tsSize $
    \dbh -> transaction (dbh ^. storeEnv) False $
        \txn ->
            storeReplaceRecord txn (dbh ^. transactionStatusStore) th ts
  where
    tsSize = 2 * digestSize + 16

-- |Write a collection of transaction statuses to the database.  This occurs
-- as a single transaction which is faster than writing them individually.
writeTransactionStatuses ::
    (MonadLogger m, MonadIO m, MonadState s m, HasDatabaseHandlers pv st s) =>
    [(TransactionHash, FinalizedTransactionStatus)] ->
    m ()
writeTransactionStatuses tss = resizeOnFull tssSize $
    \dbh -> transaction (dbh ^. storeEnv) False $
        \txn -> forM_ tss (\(tHash, tStatus) -> storeReplaceRecord txn (dbh ^. transactionStatusStore) tHash tStatus)
  where
    tssSize = (Prelude.length tss) * (2 * digestSize + 16)

-- |Write a finalization record, a collection of blocks and a collection of transaction statuses to
-- the database. This is a combination of `writeFinalizationRecord`, `writeTransactionStatuses` and
-- an arbitrary number of `writeBlock`s in a single transaction.
writeFinalizationComposite ::
    ( MonadLogger m,
      MonadIO m,
      MonadState s m,
      HasDatabaseHandlers pv st s,
      IsProtocolVersion pv,
      FixedSizeSerialization st
    ) =>
    FinalizationRecord ->
    [(Maybe (StoredBlock pv st), [(TransactionHash, FinalizedTransactionStatus)])] ->
    m ()
writeFinalizationComposite finRec blocktss = resizeOnFull (finRecSize + blocksSize + tssSize) $
    \dbh -> transaction (dbh ^. storeEnv) False $
        \txn -> do
            storeReplaceRecord txn (dbh ^. finalizationRecordStore) (finalizationIndex finRec) finRec
            forM_
                serializedBlocksTss
                ( \((block, b), tss) -> do
                    storeReplaceRecord txn (dbh ^. finalizedByHeightStore) (_bpHeight b) (_bpHash b)
                    storeReplaceBytes txn (dbh ^. blockStore) (_bpHash b) block
                    forM_ tss (uncurry (storeReplaceRecord txn (dbh ^. transactionStatusStore)))
                )
  where
    serializedBlocksTss = (((S.runPutLazy . putStoredBlock &&& sbInfo) . fromJust . fst) &&& snd) <$> filter (isJust . fst) blocktss
    finRecSize =
        let FinalizationProof vs _ = finalizationProof finRec
        in  -- key + finIndex + finBlockPointer + finProof (list of Word32s + BlsSignature.signatureSize) + finDelay
            digestSize + 64 + digestSize + (32 * Prelude.length vs) + 48 + 64
    blocksSize = sum . map (\b -> 2 * digestSize + fromIntegral (LBS.length . fst . fst $ b)) $ serializedBlocksTss
    tssSize = (* (2 * digestSize + 16)) . sum . map (Prelude.length . snd) $ blocktss

-- |If an explicitly finalized block does not satisfy a predicate, delete it, together with its
-- finalization record, all blocks implicitly finalized by its finalization records and all
-- associated transaction statuses. Returns the last finalized block and the corresponding finalized
-- record on success, which allows the use of this function as a drop-in replacement of
-- `getLastBlock`.
--
-- This function is intended to be called during the consensus initialisation, before the database
-- is used by the consensus. Since this function takes a writer lock, one should avoid calling it in
-- a context where other writers might take a lock.
--
-- This function is expected to be called with a consistent treestate.
unrollTreeStateWhile ::
    (MonadLogger m, MonadIO m, IsProtocolVersion pv, FixedSizeSerialization st) =>
    -- |Database handlers
    DatabaseHandlers pv st ->
    -- |A predicate determining if a block should be removed from the database. It is allowed to
    -- perform IO actions, for example, to attempt to read the block state from the blob store.
    (StoredBlock pv st -> m Bool) ->
    m (Either String (FinalizationRecord, StoredBlock pv st))
unrollTreeStateWhile dbh shouldDelete =
    let loopBlocks h txn delbhs finIndex sb | NormalBlock block <- sbBlock sb = do
            let bh = _bpHash . sbInfo $ sb
            -- When deleting a block, also delete the corresponding entry in the block height index
            -- and the statuses of all transactions included in the block. If any of those are not
            -- present in the database, report the treestate corruption, so it can be treated
            -- elsewhere.
            delBlockOK <- deleteRecord txn (h ^. blockStore) bh
            delBlockHashOK <- deleteRecord txn (h ^. finalizedByHeightStore) (_bpHeight . sbInfo $ sb)
            delTxsOK <- mapM (deleteRecord txn (h ^. transactionStatusStore)) ((wmdHash <$>) . blockTransactions $ block)
            if delBlockOK && delBlockHashOK && and delTxsOK
                then do
                    loadRecord txn (h ^. blockStore) (blockPointer block) >>= \case
                        Just sbParent | finIndex == sbFinalizationIndex sbParent -> loopBlocks h txn (bh : delbhs) finIndex sbParent
                        -- In particular, we stop deleting blocks when the current block's parent is the genesis
                        -- block, which is the only block with the finalization index 0.
                        Just sbParent -> return . Right $ (sbParent, bh : delbhs)
                        Nothing -> return . Left $ "Could not read the parent of the block " <> show bh
                else return . Left $ "Could not rollback block " <> show bh
        -- If the rollback reached the genesis block, do nothing. The genesis block state corruption
        -- is to be handled as part of handling global state initialisation failure.
        loopBlocks _ _ delbhs _ sb | otherwise = return . Right $ (sb, delbhs)

        loopFinRecs finIndex sb = do
            isP <- shouldDelete sb
            if isP
                then do
                    let bh = _bpHash . sbInfo $ sb
                    eancestor <- resizeOnFullInternal 4096 dbh $ \h -> transaction (h ^. storeEnv) False $ \txn -> do
                        -- After a finalization record is deleted, we must delete not only the block explicitly
                        -- finalized by it but also all ancestors of that block having the same finalization
                        -- index, even if the `shouldDelete` predicate does not hold for them. Otherwise, the
                        -- last explicitly finalized block will still have descendants in the database without a
                        -- finalization record.
                        delFinRecOK <- deleteRecord txn (h ^. finalizationRecordStore) finIndex
                        if delFinRecOK
                            then loopBlocks h txn [] finIndex sb
                            else
                                return . Left $
                                    "Could not delete the finalization record with index "
                                        <> show finIndex
                                        <> " finalizing block "
                                        <> show bh
                    case eancestor of
                        Left s -> return . Left $ s
                        Right (ancestor, delbhs) -> do
                            logEvent LMDB LLDebug $
                                "The block state for block "
                                    <> show bh
                                    <> " is corrupted. Deleted blocks "
                                    <> intercalate ", " (show <$> delbhs)
                                    <> " and finalization record "
                                    <> show finIndex
                            loopFinRecs (sbFinalizationIndex ancestor) ancestor
                else return . Right $ (finIndex, sb)
    in  liftIO (getLastBlock dbh) >>= \case
            Left s -> return . Left $ s
            Right (finRec, sb) ->
                loopFinRecs (finalizationIndex finRec) sb >>= \case
                    Left s -> return . Left $ s
                    Right (finIndex, sb') ->
                        liftIO
                            ( transaction
                                (dbh ^. storeEnv)
                                True
                                (\txn -> loadRecord txn (dbh ^. finalizationRecordStore) finIndex)
                            )
                            >>= \case
                                Just finRec' -> return . Right $ (finRec', sb')
                                Nothing ->
                                    return . Left $
                                        "Could not read the finalization record for the block "
                                            <> show (_bpHash . sbInfo $ sb')
