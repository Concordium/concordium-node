{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes an account map backed by a LMDB database.
--  The ‘AccountMap’ is a simple key/value store where the keys consists of the
--  canonical 'AccountAddress' and the values are the assoicated 'AccountIndex'.
--
--  The LMDB account map only stores  accounts that are persisted (created in a certified or finalized block).
--  Non certified/finalized accounts are being kept in a 'DifferenceMap' which
--  is being written to this LMDB account map when a block is being persisted.
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
--      * Only accounts that are in either certified or finalized blocks are present in the ‘AccountMap’
module Concordium.GlobalState.AccountMap.LMDB where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString as BS
import Data.Data (Typeable)
import qualified Data.FixedByteString as FBS
import Data.Kind (Type)
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory
import Prelude hiding (all, lookup)

import Concordium.GlobalState.Classes
import Concordium.GlobalState.LMDB.Helpers
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Utils.Serialization.Put

-- * Exceptions

-- | Exception occurring from a violation of database invariants in the LMDB database.
newtype DatabaseInvariantViolation = DatabaseInvariantViolation String
    deriving (Eq, Show, Typeable)

instance Exception DatabaseInvariantViolation where
    displayException (DatabaseInvariantViolation reason) =
        "Database invariant violation: "
            ++ show reason

-- | Monad for inserting and looking up accounts in the account map
--  backed by an LMDB database.
--  For more information, refer to the module documentation.
--
--  An implementation should ensure atomicity of operations.
--
--  Invariants:
--      * All accounts in the store are in persisted blocks (finalized or certified).
class (Monad m) => MonadAccountMapStore m where
    -- | Inserts the accounts to the underlying store.
    insert :: [(AccountAddress, AccountIndex)] -> m ()

    -- | Looks up the ‘AccountIndex’ for the provided ‘AccountAddress’ by using the
    --  equivalence class 'AccountAddressEq'.
    --  Returns @Just AccountIndex@ if the account is present in the ‘AccountMap’
    --  and returns @Nothing@ if the account was not present.
    lookup :: AccountAddress -> m (Maybe AccountIndex)

    -- | Return all the canonical addresses of accounts present
    --  in the store.
    all :: m [(AccountAddress, AccountIndex)]

    -- | Checks whether the lmdb store is initialized or not.
    isInitialized :: m Bool

instance (Monad (t m), MonadTrans t, MonadAccountMapStore m) => MonadAccountMapStore (MGSTrans t m) where
    insert = lift . insert
    lookup = lift . lookup
    all = lift all
    isInitialized = lift isInitialized
    {-# INLINE insert #-}
    {-# INLINE lookup #-}
    {-# INLINE all #-}
    {-# INLINE isInitialized #-}

deriving via (MGSTrans (StateT s) m) instance (MonadAccountMapStore m) => MonadAccountMapStore (StateT s m)
deriving via (MGSTrans (ExceptT e) m) instance (MonadAccountMapStore m) => MonadAccountMapStore (ExceptT e m)
deriving via (MGSTrans (WriterT w) m) instance (Monoid w, MonadAccountMapStore m) => MonadAccountMapStore (WriterT w m)

instance (MonadAccountMapStore m) => MonadAccountMapStore (PutT m) where
    insert = lift . insert
    lookup = lift . lookup
    all = lift all
    isInitialized = lift isInitialized

-- * Database stores

-- | Store that retains the account address -> account index mappings.
newtype AccountMapStore = AccountMapStore MDB_dbi'

accountMapStoreName :: String
accountMapStoreName = "accounts"

instance MDBDatabase AccountMapStore where
    type DBKey AccountMapStore = AccountAddress
    type DBValue AccountMapStore = AccountIndex

data DatabaseHandlers = DatabaseHandlers
    { _storeEnv :: !StoreEnv,
      _accountMapStore :: !AccountMapStore
    }
makeClassy ''DatabaseHandlers

-- | The number of stores in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 2

-- | Database growth size increment.
--  This is currently set at 64MB, and must be a multiple of the page size.
dbStepSize :: Int
dbStepSize = 2 ^ (26 :: Int) -- 64MB

-- | Maximum step to increment the database size.
dbMaxStepSize :: Int
dbMaxStepSize = 2 ^ (30 :: Int) -- 1GB

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
        return DatabaseHandlers{..}

-- | Initialize database handlers in ReadWrite mode.
--  This simply loads the references and does not initialize the databases.
--  The initial size is set to 64MB.
--  Note that this function creates the directory for the database if not already present.
openDatabase :: FilePath -> IO DatabaseHandlers
openDatabase accountMapDir = do
    createDirectoryIfMissing False accountMapDir
    makeDatabaseHandlers accountMapDir False dbInitSize

-- | Close the database. The database should not be used after it is closed.
closeDatabase :: DatabaseHandlers -> IO ()
closeDatabase dbHandlers = runInBoundThread $ mdb_env_close $ dbHandlers ^. storeEnv . seEnv

-- ** Monad implementation

-- | The 'AccountMapStoreMonad' for interacting with the LMDB database.
newtype AccountMapStoreMonad (m :: Type -> Type) (a :: Type) = AccountMapStoreMonad {runAccountMapStoreMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadReader r, MonadState s, TimeMonad) via m
    deriving (MonadTrans) via IdentityT

deriving instance (MonadProtocolVersion m) => MonadProtocolVersion (AccountMapStoreMonad m)

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

-- | Delete the provided accounts from the LMDB store.
--
--  This function should only be used when rolling back certified blocks. When rolling back finalized blocks,
--  no accounts should be deleted as they are already confirmed to be finalized.
unsafeRollback :: (MonadIO m, MonadLogger m, MonadReader r m, HasDatabaseHandlers r) => [AccountAddress] -> m ()
unsafeRollback accounts = do
    handlers <- ask
    flip runReaderT handlers $ runAccountMapStoreMonad $ asWriteTransaction $ \dbh txn -> do
        forM_ accounts $ \accAddr -> deleteRecord txn (dbh ^. accountMapStore) accAddr

-- | When looking up accounts we perform a prefix search as we
--  store the canonical account addresses in the lmdb store and we
--  need to be able to lookup account aliases.
prefixAccountAddressSize :: Int
prefixAccountAddressSize = 29

instance
    ( MonadReader r m,
      HasDatabaseHandlers r,
      MonadIO m,
      MonadLogger m
    ) =>
    MonadAccountMapStore (AccountMapStoreMonad m)
    where
    insert differenceMap = asWriteTransaction $ \dbh txn -> doInsert dbh txn differenceMap
      where
        doInsert dbh txn accounts = do
            forM_ accounts $ \(accAddr, accIndex) -> do
                storeRecord txn (dbh ^. accountMapStore) accAddr accIndex

    lookup a@(AccountAddress accAddr) = asReadTransaction $ \dbh txn ->
        withCursor txn (dbh ^. accountMapStore) $ \cursor -> do
            withMDB_val accLookupKey $ \k -> do
                getCursor (CursorMoveTo k) cursor >>= \case
                    Nothing -> return Nothing
                    Just (Left err) -> throwM $ DatabaseInvariantViolation err
                    Just (Right (foundAccAddr, accIdx)) ->
                        if checkEquivalence a foundAccAddr
                            then return $ Just accIdx
                            else return Nothing
      where
        -- The key to use for looking up an account.
        -- We do a prefix lookup on the first 29 bytes of the account address as
        -- the last 3 bytes are reserved for aliases.
        accLookupKey = BS.take prefixAccountAddressSize $ FBS.toByteString accAddr
        checkEquivalence x y = accountAddressEmbed x == accountAddressEmbed y

    all = asReadTransaction $ \dbh txn -> loadAll txn (dbh ^. accountMapStore)

    isInitialized = do
        size <- asReadTransaction $ \dbh txn -> databaseSize txn (dbh ^. accountMapStore)
        return $ size /= 0
