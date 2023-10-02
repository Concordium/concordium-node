{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- Here because of the MonadReader instance for AccountMapStoreMonad
-- Revise this.
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes an account map backed by a LMDB database.
--  The ‘AccountMap’ is a simple key/value store where the keys consists
--  of the first 29 bytes of an ‘AccountAddress’ and the values are the
--  associated ‘AccountIndex’.
--
--  The LMDB account map only stores finalized accounts.
--  Non finalized accounts are being kept in a 'DifferenceMap' which
--  is being written to this LMDB account map when a block is finalized.
--
--  As opposed to the account table of the block state this database does not
--  include historical data i.e., the state of this database is from the perspective
--  of the last finalized block always.
--  For querying historical data (e.g. which accounts existed in a given block) then one
--  should use the account table.
--
--  The account map is integrated with the block state “on-the-fly” meaning that
--  whenver the node starts up and the ‘AccountMap’ is not populated, then it will be
--  initialized on startup via the existing ‘PersistentAccountMap’.
--
--  Invariants:
--      * Only finalized accounts are present in the ‘AccountMap’
module Concordium.GlobalState.AccountMap.LMDB where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import Data.Data (Data, Typeable)
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory

import Concordium.GlobalState.AccountMap.DifferenceMap
import Concordium.GlobalState.LMDB.Helpers
import Concordium.Logger
import Concordium.Types
import qualified Data.FixedByteString as FBS

-- * Exceptions

-- | Exception occurring from a violation of database invariants in the LMDB database.
newtype DatabaseInvariantViolation = DatabaseInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseInvariantViolation where
    displayException (DatabaseInvariantViolation reason) =
        "Database invariant violation: "
            ++ show reason

-- | The interface to the LMDB account map.
--  For more information, refer to the module documentation.
--  Invariants:
--      * All accounts in the store are finalized.
class (Monad m) => MonadAccountMapStore m where
    -- | Create and initialize the ‘AccountMap’ via the supplied map of accounts
    --  for the supplied 'BlockHash'.
    --  The provided 'BlockHash' must correspond to the hash of last finalized block
    --  when this function is invoked.
    --  The @[AccountAddress]@ should be obtained by the account table.
    --  Precondition: offset of the @AccountAddress@ in the list must correspond to
    --  the account index of that particular account.
    initialize :: BlockHash -> BlockHeight -> [AccountAddress] -> m ()

    -- | Check whether the ‘AccountMap’ is initialized.
    --  Returns @Just BlockHash@ if the 'AccountMap' is initialized,
    --  the ‘BlockHash’ indicates the last finalized block for the 'AccountMap'.
    --  Returns @Nothing@ if the account map is not initialized.
    isInitialized :: m (Maybe (BlockHash, BlockHeight))

    -- | Adds accounts present in the provided difference maps to the lmdb store.
    --  The argument is a list as multiple blocks can be finalized at the same time.
    --  Implementations should update the last finalized block pointer.
    --
    --  Postcondition: The list of 'AccountMapDifferenceMap' MUST be provided in
    --  ascending order of the block height.
    insert :: BlockHash -> BlockHeight -> [DifferenceMap] -> m ()

    -- | Looks up the ‘AccountIndex’ for the provided ‘AccountAddress’.
    --  Returns @Just AccountIndex@ if the account is present in the ‘AccountMap’
    --  and returns @Nothing@ if the account was not present.
    lookup :: AccountAddress -> m (Maybe AccountIndex)

-- * Database stores

-- | Store that yields the last finalized block from the perspective
--  of the lmdb database.
newtype MetadataStore = MetadataStore MDB_dbi'

-- | Name of the 'MetadataStore'
metadataStoreName :: String
metadataStoreName = "metadata"

-- | Store that retains the account address -> account index mappings.
newtype AccountMapStore = AccountMapStore MDB_dbi'

accountMapStoreName :: String
accountMapStoreName = "accounts"

-- | We only store the first 29 bytes of the account address
--  as these uniquely determine the account.
--  The remaining 3 bytes of an account address are used for the
--  account aliasing feature.
prefixAccountAddressSize :: Int
prefixAccountAddressSize = 29

data PrefixAccountAddressSize
    deriving (Data, Typeable)

instance FBS.FixedLength PrefixAccountAddressSize where
    fixedLength _ = prefixAccountAddressSize

-- | The prefix account address which is used as keys in the underlying store.
newtype PrefixAccountAddress = PrefixAccountAddress (FBS.FixedByteString PrefixAccountAddressSize)

instance S.Serialize PrefixAccountAddress where
    put (PrefixAccountAddress addr) = S.putByteString $ FBS.toByteString addr
    get = PrefixAccountAddress . FBS.fromByteString <$> S.getByteString prefixAccountAddressSize

-- | Create a 'PrefixAccountAddress' from the supplied 'AccountAddress'.
--  The 'PrefixAccountAddress' is the first 29 bytes of the original 'AccountAddress'.
accountAddressToPrefixAccountAddress :: AccountAddress -> PrefixAccountAddress
accountAddressToPrefixAccountAddress (AccountAddress afbs) = toPrefixAccountAddress $ FBS.toByteString afbs
  where
    toPrefixAccountAddress = PrefixAccountAddress . FBS.fromByteString . first29Bytes
    first29Bytes = BS.take prefixAccountAddressSize

instance MDBDatabase AccountMapStore where
    type DBKey AccountMapStore = PrefixAccountAddress
    type DBValue AccountMapStore = AccountIndex

lfbKey :: DBKey MetadataStore
lfbKey = "lfb"

instance MDBDatabase MetadataStore where
    type DBKey MetadataStore = BS.ByteString
    type DBValue MetadataStore = (BlockHash, BlockHeight)

data DatabaseHandlers = DatabaseHandlers
    { _storeEnv :: !StoreEnv,
      _metadataStore :: !MetadataStore,
      _accountMapStore :: !AccountMapStore
    }
makeClassy ''DatabaseHandlers

-- | The number of stores in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 2

-- | Database growth size increment.
--  This is currently set at 64MB, and must be a multiple of the page size.
dbStepSize :: Int
dbStepSize = 2 ^ (25 :: Int) -- 32MB

-- | Maximum step to increment the database size.
dbMaxStepSize :: Int
dbMaxStepSize = 2 ^ (28 :: Int) -- 256mb

-- | Initial database size.
--  This is currently set to be the same as 'dbStepSize'.
dbInitSize :: Int
dbInitSize = dbStepSize

-- ** Helpers

-- TODO: These helper functions below should probably be refactored and moved into LDMBHelpers so
-- they can be used across all lmdb database implementations.

-- | Resize the LMDB map if the file size has changed.
--  This is used to allow a secondary process that is reading the database
--  to handle resizes to the database that are made by the writer.
--  The supplied action will be executed. If it fails with an 'MDB_MAP_RESIZED'
--  error, then the map will be resized and the action retried.
resizeOnResized :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r, MonadCatch m) => m a -> m a
resizeOnResized a = do
    dbh <- view databaseHandlers
    resizeOnResizedInternal (dbh ^. storeEnv) a

-- | Perform a database action and resize the LMDB map if the file size has changed. The difference
--  with `resizeOnResized` is that this function takes database handlers as an argument, instead of
--  reading their value from `HasDatabaseHandlers`.
resizeOnResizedInternal :: (MonadIO m, MonadCatch m) => StoreEnv -> m a -> m a
resizeOnResizedInternal se a = inner
  where
    inner = handleJust checkResized onResized a
    checkResized LMDB_Error{..} = guard (e_code == Right MDB_MAP_RESIZED)
    onResized _ = do
        liftIO (withWriteStoreEnv se $ flip mdb_env_set_mapsize 0)
        inner

-- | Increase the database size by at least the supplied size.
--  The size SHOULD be a multiple of 'dbStepSize', and MUST be a multiple of the page size.
resizeDatabaseHandlers :: (MonadIO m, MonadLogger m) => DatabaseHandlers -> Int -> m ()
resizeDatabaseHandlers dbh delta = do
    envInfo <- liftIO $ mdb_env_info (dbh ^. storeEnv . seEnv)
    let oldMapSize = fromIntegral $ me_mapsize envInfo
        newMapSize = oldMapSize + delta
        _storeEnv = dbh ^. storeEnv
    logEvent LMDB LLDebug $ "Resizing database from " ++ show oldMapSize ++ " to " ++ show newMapSize
    liftIO . withWriteStoreEnv (dbh ^. storeEnv) $ flip mdb_env_set_mapsize newMapSize

-- ** Initialization

-- | Initialize database handlers.
--  The size will be rounded up to a multiple of 'dbStepSize'.
--  (This ensures in particular that the size is a multiple of the page size, which is required by
--  LMDB.)
makeDatabaseHandlers ::
    -- | Path of database
    FilePath ->
    -- | Open read only
    Bool ->
    -- | Initial database size
    Int ->
    IO DatabaseHandlers
makeDatabaseHandlers accountMapDir readOnly initSize = do
    _storeEnv <- makeStoreEnv
    -- here nobody else has access to the environment, so we need not lock
    let env = _storeEnv ^. seEnv
    mdb_env_set_mapsize env (initSize + dbStepSize - initSize `mod` dbStepSize)
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    mdb_env_open env accountMapDir [MDB_RDONLY | readOnly]
    transaction _storeEnv readOnly $ \txn -> do
        _accountMapStore <-
            AccountMapStore
                <$> mdb_dbi_open'
                    txn
                    (Just accountMapStoreName)
                    [MDB_CREATE | not readOnly]
        _metadataStore <-
            MetadataStore
                <$> mdb_dbi_open'
                    txn
                    (Just metadataStoreName)
                    [MDB_CREATE | not readOnly]
        return DatabaseHandlers{..}

-- | Initialize database handlers in ReadWrite mode.
--  This simply loads the references and does not initialize the databases.
--  The initial size is set to 64MB.
openDatabase :: FilePath -> IO DatabaseHandlers
openDatabase accountMapDir = do
    createDirectoryIfMissing False accountMapDir
    makeDatabaseHandlers accountMapDir False dbInitSize

-- | Close the database. The database should not be used after it is closed.
closeDatabase :: DatabaseHandlers -> IO ()
closeDatabase dbHandlers = runInBoundThread $ mdb_env_close $ dbHandlers ^. storeEnv . seEnv

-- ** Monad implementation
newtype AccountMapStoreMonad m a = AccountMapStoreMonad {runAccountMapStoreMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger) via m
    deriving (MonadTrans) via IdentityT

deriving instance (MonadReader r m) => MonadReader r (AccountMapStoreMonad m)

-- | Run a read-only transaction.
asReadTransaction :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r) => (DatabaseHandlers -> MDB_txn -> IO a) -> AccountMapStoreMonad m a
asReadTransaction t = do
    dbh <- view databaseHandlers
    liftIO $ transaction (dbh ^. storeEnv) True $ t dbh

-- | Run a write transaction. If the transaction fails due to the database being full, this resizes
--  the database and retries the transaction.
asWriteTransaction :: (MonadIO m, MonadReader r m, HasDatabaseHandlers r, MonadLogger m) => (DatabaseHandlers -> MDB_txn -> IO a) -> AccountMapStoreMonad m a
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
    ( MonadReader r m,
      HasDatabaseHandlers r,
      MonadIO m,
      MonadLogger m
    ) =>
    MonadAccountMapStore (AccountMapStoreMonad m)
    where
    initialize lfbHash lfbHeight accounts = asWriteTransaction $ \dbh txn -> do
        forM_
            (zip accounts [0 ..])
            ( \(accAddr, accIndex) -> do
                storeRecord txn (dbh ^. accountMapStore) (accountAddressToPrefixAccountAddress accAddr) accIndex
            )
        storeRecord txn (dbh ^. metadataStore) lfbKey (lfbHash, lfbHeight)
    isInitialized = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. metadataStore) lfbKey
    insert lfbHash lfbHeight differenceMaps = asWriteTransaction $ \dbh txn -> do
        forM_ differenceMaps (doInsert dbh txn)
      where
        doInsert dbh txn DifferenceMap{..} = do
            forM_ dmAccounts $ \(accAddr, expectedAccIndex) -> do
                let addr = accountAddressToPrefixAccountAddress accAddr
                accIndex <- AccountIndex . subtract 1 <$> databaseSize txn (dbh ^. accountMapStore)
                when (accIndex /= expectedAccIndex) $
                    throwM . DatabaseInvariantViolation $
                        "The actual account index " <> show accIndex <> "did not match the expected one " <> show expectedAccIndex
                storeRecord txn (dbh ^. accountMapStore) addr accIndex
                storeReplaceRecord txn (dbh ^. metadataStore) lfbKey (lfbHash, lfbHeight)
                return $ Just accIndex

    lookup accAddr = asReadTransaction $ \dbh txn ->
        loadRecord txn (dbh ^. accountMapStore) $ accountAddressToPrefixAccountAddress accAddr
