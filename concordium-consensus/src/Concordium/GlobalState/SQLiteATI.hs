{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, DerivingStrategies, StandaloneDeriving, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQLiteATI where

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.GlobalState.AccountTransactionIndex

import Database.Persist
-- import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Pool

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
          insertMany_
              . Prelude.reverse -- reverse is because the latest entry is the head of the list
              . fmap (\(k, v) -> Entry{
                         entryAccount = S.encode k,
                         entryBlock = S.encode bcHash,
                         entryBlockHeight = fromIntegral bcHeight,
                         entryBlockTime = timestampToSeconds bcTime,
                         entryHash = Just (S.encode (tsHash v)),
                         entrySummary = toStrict (AE.encode v)
                         })
              $ hm

          insertMany_
              . fmap (\v@BakingReward{..} -> Entry{
                         entryAccount = S.encode stoBakerAccount,
                         entryBlock = S.encode bcHash,
                         entryBlockHeight = fromIntegral bcHeight,
                         entryBlockTime = timestampToSeconds bcTime,
                         entryHash = Nothing,
                         entrySummary = toStrict (AE.encode v)
                         })
              $ sos
