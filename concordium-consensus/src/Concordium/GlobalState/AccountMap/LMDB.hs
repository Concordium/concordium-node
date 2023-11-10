{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module exposes an account map backed by a LMDB database.
--  The account map is a simple key/value store where the keys consists of the
--  canonical 'AccountAddress' and the values are the assoicated 'AccountIndex'.
--
--  The LMDB account map only stores accounts that are finalized.
--  Non finalized accounts are being kept in a 'DifferenceMap' which
--  is written to this LMDB account map when a block is finalized.
--
--  This also means that accounts once put in the account map, then
--  they can never be deleted again (hence they're finalized).
--
--  As opposed to the account table of the block state this database does not
--  include historical data i.e., the state of this database is from the perspective
--  of the last finalized block always.
--
--  The account map is integrated with the block state “on-the-fly” meaning that
--  whenever the node starts up and the account map is not populated, then it will be
--  initialized on startup via the existing ‘PersistentAccountMap’.
--
--  Invariants:
--      * Only accounts that are in finalized blocks are present in the ‘AccountMap’
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
import Data.Word
import Database.LMDB.Raw
import Lens.Micro.Platform
import System.Directory

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
--      * All accounts in the store are finalized.
--      * The store should only retain canonical account addresses.
class (Monad m) => MonadAccountMapStore m where
    -- | Inserts the accounts to the underlying store.
    --  Only canonical addresses should be added.
    insertAccounts :: [(AccountAddress, AccountIndex)] -> m ()

    -- | Looks up the ‘AccountIndex’ for the provided ‘AccountAddressEq’.
    --  Returns @Just AccountIndex@ if the account is present in the ‘AccountMap’
    --  and returns @Nothing@ if the account was not present.
    lookupAccountIndexViaEquivalence :: AccountAddressEq -> m (Maybe AccountIndex)

    -- | Looks up the ‘AccountIndex’ for the provided ‘AccountAddress'.
    --  Returns @Just AccountIndex@ if the account is present in the ‘AccountMap’
    --  and returns @Nothing@ if the account was not present.
    --  Note that this only returns a result for the canonical account address.
    lookupAccountIndexViaExactness :: AccountAddress -> m (Maybe AccountIndex)

    -- | Return all the canonical addresses and their associated account indices of accounts present
    --  in the store where their @AccountIndex@ is less or equal to the provided @AccountIndex@.
    getAllAccounts :: AccountIndex -> m [(AccountAddress, AccountIndex)]

    -- | Get number of entries in the account map.
    getNumberOfAccounts :: m Word64

    -- | Clear and set the accounts to the ones provided.
    reconstruct :: [(AccountAddress, AccountIndex)] -> m ()

instance (Monad (t m), MonadTrans t, MonadAccountMapStore m) => MonadAccountMapStore (MGSTrans t m) where
    insertAccounts accs = lift $ insertAccounts accs
    lookupAccountIndexViaEquivalence = lift . lookupAccountIndexViaEquivalence
    lookupAccountIndexViaExactness = lift . lookupAccountIndexViaExactness
    getAllAccounts = lift . getAllAccounts
    getNumberOfAccounts = lift getNumberOfAccounts
    reconstruct = lift . reconstruct
    {-# INLINE insertAccounts #-}
    {-# INLINE lookupAccountIndexViaEquivalence #-}
    {-# INLINE lookupAccountIndexViaExactness #-}
    {-# INLINE getAllAccounts #-}
    {-# INLINE getNumberOfAccounts #-}
    {-# INLINE reconstruct #-}

deriving via (MGSTrans (StateT s) m) instance (MonadAccountMapStore m) => MonadAccountMapStore (StateT s m)
deriving via (MGSTrans (ExceptT e) m) instance (MonadAccountMapStore m) => MonadAccountMapStore (ExceptT e m)
deriving via (MGSTrans (WriterT w) m) instance (Monoid w, MonadAccountMapStore m) => MonadAccountMapStore (WriterT w m)

instance (MonadAccountMapStore m) => MonadAccountMapStore (PutT m) where
    insertAccounts accs = lift $ insertAccounts accs
    lookupAccountIndexViaEquivalence = lift . lookupAccountIndexViaEquivalence
    lookupAccountIndexViaExactness = lift . lookupAccountIndexViaExactness
    getAllAccounts = lift . getAllAccounts
    getNumberOfAccounts = lift getNumberOfAccounts
    reconstruct = lift . reconstruct
    {-# INLINE insertAccounts #-}
    {-# INLINE lookupAccountIndexViaEquivalence #-}
    {-# INLINE lookupAccountIndexViaExactness #-}
    {-# INLINE getAllAccounts #-}
    {-# INLINE getNumberOfAccounts #-}
    {-# INLINE reconstruct #-}

-- * Database stores

-- | Store that retains the account address -> account index mappings.
newtype AccountMapStore = AccountMapStore MDB_dbi'

-- | Name of the table used for storing the map from account addresses to account indices.
accountMapStoreName :: String
accountMapStoreName = "accounts"

instance MDBDatabase AccountMapStore where
    type DBKey AccountMapStore = AccountAddress
    type DBValue AccountMapStore = AccountIndex

-- | Datbase handlers to interact with the account map lmdb
--  database. Create via 'makeDatabasehandlers'.
data DatabaseHandlers = DatabaseHandlers
    { -- | The underlying lmdb store environment.
      _dbhStoreEnv :: !StoreEnv,
      -- | The only store for this lmdb database.
      --  The account map functions as a persistent @AccountAddress -> Maybe AccountIndex@ mapping.
      _dbhAccountMapStore :: !AccountMapStore
    }

makeClassy ''DatabaseHandlers

-- | The number of stores in the LMDB environment for 'DatabaseHandlers'.
databaseCount :: Int
databaseCount = 1

-- | Database growth size increment.
--  This is currently set at 4MB, and must be a multiple of the page size.
--  For reference: ~ 90k accounts takes up around 7MB, so this should ensure not much resizing required.
dbStepSize :: Int
dbStepSize = 2 ^ (22 :: Int) -- 4MB

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
    IO DatabaseHandlers
makeDatabaseHandlers accountMapDir readOnly = do
    _dbhStoreEnv <- makeStoreEnv' dbStepSize defaultMaxStepSize
    -- here nobody else has access to the environment, so we need not lock
    let env = _dbhStoreEnv ^. seEnv
    mdb_env_set_mapsize env dbStepSize
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    mdb_env_open env accountMapDir [MDB_RDONLY | readOnly]
    transaction _dbhStoreEnv readOnly $ \txn -> do
        _dbhAccountMapStore <-
            AccountMapStore
                <$> mdb_dbi_open'
                    txn
                    (Just accountMapStoreName)
                    [MDB_CREATE | not readOnly]
        return DatabaseHandlers{..}

-- | Create the lmdb stores and return back database handlers for interacting with it.
--  This simply loads the references and does not initialize the databases.
--  The initial environment size is set to 'dbStepSize' (4MB).
--  Note that this function creates the directory for the database if not already present at the provided
--  path and any missing parent directories.
openDatabase :: FilePath -> IO DatabaseHandlers
openDatabase accountMapDir = do
    createDirectoryIfMissing True accountMapDir
    makeDatabaseHandlers accountMapDir False

-- | Close the database. The database should not be used after it is closed.
closeDatabase :: DatabaseHandlers -> IO ()
closeDatabase dbHandlers = runInBoundThread $ mdb_env_close $ dbHandlers ^. dbhStoreEnv . seEnv

-- ** Monad implementation

-- | The 'AccountMapStoreMonad' for interacting with the LMDB database.
newtype AccountMapStoreMonad (m :: Type -> Type) (a :: Type) = AccountMapStoreMonad {runAccountMapStoreMonad :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadLogger, MonadReader r, MonadState s, TimeMonad)
    deriving (MonadTrans) via IdentityT

deriving instance (MonadProtocolVersion m) => MonadProtocolVersion (AccountMapStoreMonad m)

instance
    ( MonadReader r m,
      HasDatabaseHandlers r,
      MonadIO m,
      MonadLogger m
    ) =>
    MonadAccountMapStore (AccountMapStoreMonad m)
    where
    insertAccounts accounts = do
        dbh <- ask
        asWriteTransaction (dbh ^. dbhStoreEnv) $ \txn -> do
            forM_ accounts $ \(accAddr, accIndex) -> do
                storeRecord txn (dbh ^. dbhAccountMapStore) accAddr accIndex

    lookupAccountIndexViaEquivalence a@(AccountAddressEq (AccountAddress accAddr)) = do
        dbh <- ask
        asReadTransaction (dbh ^. dbhStoreEnv) $ \txn ->
            withCursor txn (dbh ^. dbhAccountMapStore) $ \cursor -> do
                withMDB_val accLookupKey $ \k -> do
                    getCursor (CursorMoveTo k) cursor >>= \case
                        Nothing -> return Nothing
                        Just (Left err) -> throwM $ DatabaseInvariantViolation err
                        Just (Right (foundAccAddr, accIdx)) ->
                            -- we need to check equivalence here as we are performing
                            -- prefix lookup in the lmdb database, so if the account does not exist
                            -- then the lmdb query would return the "next" account address
                            -- by lexicographic order of account address.
                            if a == accountAddressEmbed foundAccAddr
                                then return $ Just accIdx
                                else return Nothing
      where
        -- The key to use for looking up an account.
        -- We do a prefix lookup on the first 29 bytes of the account address as
        -- the last 3 bytes are reserved for aliases.
        accLookupKey = BS.take accountAddressPrefixSize $ FBS.toByteString accAddr

    lookupAccountIndexViaExactness addr = do
        dbh <- ask
        asReadTransaction (dbh ^. dbhStoreEnv) $ \txn -> loadRecord txn (dbh ^. dbhAccountMapStore) addr

    getAllAccounts maxAccountIndex = do
        dbh <- ask
        asReadTransaction (dbh ^. dbhStoreEnv) $ \txn ->
            withCursor txn (dbh ^. dbhAccountMapStore) $ \cursor ->
                let go !accum Nothing = return accum
                    go !accum (Just (Right acc@(_, accIdx))) = do
                        -- We only accumulate accounts which have an @AccountIndex@ at most
                        -- the provided one.
                        if accIdx <= maxAccountIndex
                            then go (acc : accum) =<< getCursor CursorNext cursor
                            else go accum =<< getCursor CursorNext cursor
                    go _ (Just (Left err)) = throwM $ DatabaseInvariantViolation err
                in  go [] =<< getCursor CursorFirst cursor

    getNumberOfAccounts = do
        dbh <- ask
        asReadTransaction (dbh ^. dbhStoreEnv) $ \txn -> databaseSize txn (dbh ^. dbhAccountMapStore)

    reconstruct accounts = do
        dbh <- ask
        asWriteTransaction (dbh ^. dbhStoreEnv) $ \txn -> do
            deleteAll txn (dbh ^. dbhAccountMapStore)
            forM_ accounts $ \(accAddr, accIndex) -> do
                storeRecord txn (dbh ^. dbhAccountMapStore) accAddr accIndex
