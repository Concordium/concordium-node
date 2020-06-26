{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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

writeEntries :: Pool SqlBackend -> BlockContext -> AccountTransactionIndex -> [SpecialTransactionOutcome] -> IO ()
writeEntries pool BlockContext{..} hm sos = do
  runPostgres pool c
  where c :: ReaderT SqlBackend (NoLoggingT IO) ()
        c = do
          let hm' =
                fmap (\(k, v) -> (Summary {
                                    summaryBlock = ByteStringSerialized bcHash,
                                    summarySummary = AE.toJSON v,
                                    summaryTimestamp = bcTime,
                                    summaryHeight = bcHeight}
                                , Entry (ByteStringSerialized k))) hm
              sos' =
                fmap (\v ->  (Summary {
                                summaryBlock = ByteStringSerialized bcHash,
                                summarySummary = AE.toJSON v,
                                summaryTimestamp = bcTime,
                                summaryHeight = bcHeight }
                            , Entry (ByteStringSerialized $ stoBakerAccount v))) sos

              runInsertion :: [(Summary, Key Summary -> Entry)] -> ReaderT SqlBackend (NoLoggingT IO) ()
              runInsertion xs = do
                xskeys <- insertMany (Prelude.map fst xs)
                insertMany_ (Prelude.zipWith (\x y -> (snd x) y) xs xskeys)

          runInsertion hm'
          runInsertion sos'
          pure ()
