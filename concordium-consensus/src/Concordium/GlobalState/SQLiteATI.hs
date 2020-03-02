{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQLiteATI where

import Concordium.Types
import Concordium.GlobalState.AccountTransactionIndex

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Conduit
import qualified Data.HashMap.Strict as HM

import qualified Data.Serialize as S
import qualified Data.Aeson as AE
import Data.Text(Text)
import qualified Data.Text as Text

import Control.Monad.Logger
import Control.Monad.Reader

import Data.ByteString
import Data.ByteString.Lazy(toStrict)
import System.Directory

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
  Entry
    account ByteString
    block ByteString
    summary ByteString
    deriving Eq Show
  |]

createTable :: Text -> IO ()
createTable dbName = do
  exists <- doesFileExist (Text.unpack dbName)
  if exists then
    fail "Database file already exists."
  else runSqlite dbName (runMigration migrateAll :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ())

writeEntries :: Text -> BlockHash -> AccountTransactionIndex -> IO ()
writeEntries dbName bh hm = do
  runSqlite dbName c
  where c :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
        c = do
          insertMany_ (fmap (\(k, v) -> Entry (S.encode k) (S.encode bh) (toStrict (AE.encode v))) $ HM.toList hm)
