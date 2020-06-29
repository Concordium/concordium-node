{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQL.AccountTransactionIndex where

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.SQL

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Postgresql.JSON()
import Database.Persist.TH
import Data.Pool

import qualified Data.Aeson as AE

import Control.Monad.Logger
import Control.Monad.Reader

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Summary sql=summaries
    block (ByteStringSerialized BlockHash)
    timestamp Timestamp
    height BlockHeight
    summary AE.Value
    deriving Eq Show

  Entry sql=ati
    account (ByteStringSerialized AccountAddress)
    summary SummaryId
    deriving Eq Show
  |]

connectPostgres :: ConnectionString -> IO (Pool SqlBackend)
connectPostgres connString = runNoLoggingT (createPostgresqlPool connString 5)

runPostgres :: Pool SqlBackend -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runPostgres pool c = runNoLoggingT (runSqlPool c pool)

createTable :: Pool SqlBackend -> IO ()
createTable pool = runPostgres pool (runMigration migrateAll)

-- |Write the outcomes of the transactions and the special transaction outcomes of a block
-- into the postgresql backend. Note that this will make only one database commit as it uses
-- `runSqlConn` internally.
writeEntries :: Pool SqlBackend -> BlockContext -> AccountTransactionIndex -> [SpecialTransactionOutcome] -> IO ()
writeEntries pool BlockContext{..} ati sto = do
  runPostgres pool c
  where c :: ReaderT SqlBackend (NoLoggingT IO) ()
        c = do
          let createSummary :: forall v. (AE.ToJSON v) => v -> Summary
              createSummary v = Summary {
                summaryBlock = ByteStringSerialized bcHash,
                summarySummary = AE.toJSON v,
                summaryTimestamp = bcTime,
                summaryHeight = bcHeight}
              -- In these collections the transaction outcomes are
              -- mapped to the database values.
              atiWithDatabaseValues = reverse $ -- reverse is because the latest entry is the head of the list
                fmap (\(k, v) -> (createSummary v
                                , Entry (ByteStringSerialized k))) ati
              stoWithDatabaseValues =
                fmap (\v ->  (createSummary v
                            , Entry (ByteStringSerialized $ stoBakerAccount v))) sto

              -- Insert all the Summaries, get the keys in the same query (postgresql does it in one query)
              -- and insert all the entries after adding the correct `Key Summary` to each one of them.
              runInsertion :: [(Summary, Key Summary -> Entry)] -> ReaderT SqlBackend (NoLoggingT IO) ()
              runInsertion xs = do
                xskeys <- insertMany (Prelude.map fst xs)
                insertMany_ (Prelude.zipWith (\x y -> (snd x) y) xs xskeys)

          runInsertion atiWithDatabaseValues
          runInsertion stoWithDatabaseValues
          pure ()
