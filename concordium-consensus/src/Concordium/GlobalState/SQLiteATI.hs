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
-- import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Database.Persist.TH
import Conduit

import qualified Data.Serialize as S
import qualified Data.Aeson as AE

import Control.Monad.Logger
import Control.Monad.Reader

import Data.Word
import Data.ByteString
import Data.ByteString.Lazy(toStrict)

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

runPostgres :: ConnectionString -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
runPostgres connString = runResourceT . runNoLoggingT . withPostgresqlPool connString 5 . runSqlPool

createTable :: ConnectionString -> IO ()
createTable dbName =
  runPostgres dbName (runMigration migrateAll :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ())

writeEntries :: ConnectionString -> BlockContext -> AccountTransactionIndex -> [SpecialTransactionOutcome] -> IO ()
writeEntries dbName BlockContext{..} hm sos = do
  runPostgres dbName c
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
