{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Module for the low level LMDB account map.
module Concordium.GlobalState.AccountMap.LMDB where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Data (Data, Typeable)
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Database.LMDB.Raw
import System.Directory
import Lens.Micro.Platform

import Concordium.Logger
import qualified Data.FixedByteString as FBS
import Concordium.Types
import Concordium.GlobalState.LMDB.Helpers

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

accountAddressToPrefixAccountAddress :: AccountAddress -> PrefixAccountAddress
accountAddressToPrefixAccountAddress = undefined

instance MDBDatabase AccountMapStore where
    type DBKey AccountMapStore = PrefixAccountAddress
    type DBValue AccountMapStore = AccountIndex


lfbKey :: DBKey MetadataStore
lfbKey = "lfb"
  
instance MDBDatabase MetadataStore where
    type DBKey MetadataStore = BS.ByteString
    type DBValue MetadataStore = BlockHash

data DatabaseHandlers = DatabaseHandlers
    {
      _storeEnv :: !StoreEnv,
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
makeDatabaseHandlers treeStateDir readOnly initSize = do
    _storeEnv <- makeStoreEnv
    -- here nobody else has access to the environment, so we need not lock
    let env = _storeEnv ^. seEnv
    mdb_env_set_mapsize env (initSize + dbStepSize - initSize `mod` dbStepSize)
    mdb_env_set_maxdbs env databaseCount
    mdb_env_set_maxreaders env 126
    mdb_env_open env treeStateDir [MDB_RDONLY | readOnly]
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
    
