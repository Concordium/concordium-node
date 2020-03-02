{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQLiteATI where

import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.GlobalState.AccountTransactionIndex

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Conduit

import qualified Data.Serialize as S
import qualified Data.Aeson as AE
import Data.Text(Text)
import qualified Data.Text as Text

import Control.Monad.Logger
import Control.Monad.Reader

import Data.Word
import Data.ByteString
import Data.ByteString.Lazy(toStrict)
import System.Directory

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
  Entry
    account ByteString  maxlen=32
    block ByteString  maxlen=32
    blockHeight Word64
    blockTime Word64
    hash ByteString Maybe
    summary ByteString

    deriving Eq Show
  |]

createTable :: Text -> IO ()
createTable dbName = do
  exists <- doesFileExist (Text.unpack dbName)
  if exists then
    fail "Database file already exists."
  else runSqlite dbName (runMigration migrateAll :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ())

writeEntries :: Text -> BlockContext -> AccountTransactionIndex -> [SpecialTransactionOutcome] -> IO ()
writeEntries dbName BlockContext{..} hm sos = do
  runSqlite dbName c
  where c :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
        c = do
          insertMany_
              . Prelude.reverse -- reverse is because the latest entry is the head of the list
              . fmap (\(k, v) -> Entry{
                         entryAccount = S.encode k,
                         entryBlock = S.encode bcHash,
                         entryBlockHeight = fromIntegral bcHeight,
                         entryBlockTime = fromIntegral bcTime,
                         entryHash = Just (S.encode (tsHash v)),
                         entrySummary = toStrict (AE.encode v)
                         })
              $ hm

          insertMany_
              . fmap (\v@BakingReward{..} -> Entry{
                         entryAccount = S.encode stoBakerAccount,
                         entryBlock = S.encode bcHash,
                         entryBlockHeight = fromIntegral bcHeight,
                         entryBlockTime = fromIntegral bcTime,
                         entryHash = Nothing,
                         entrySummary = toStrict (AE.encode v)
                         })
              $ sos
