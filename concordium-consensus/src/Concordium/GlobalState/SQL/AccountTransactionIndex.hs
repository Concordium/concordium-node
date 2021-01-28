{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQL.AccountTransactionIndex where

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.SQL.Helpers
import Concordium.SQL.AccountTransactionIndex

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Postgresql.JSON()
import Data.Pool
import qualified Data.Aeson as AE
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Control.Monad.Logger
import Control.Monad.Reader

connectPostgres :: ConnectionString -> IO (Pool SqlBackend)
connectPostgres connString = runNoLoggingT (createPostgresqlPool connString 5)

runPostgres :: Pool SqlBackend -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runPostgres pool c = runNoLoggingT (runSqlPool c pool)

createTable :: Pool SqlBackend -> IO ()
createTable pool = runPostgres pool (runMigration migrateAll)

type PersistentTransactionOutcome = Either TransactionSummary SpecialTransactionOutcome

-- |Write the outcomes of the transactions and the special transaction outcomes of a block
-- into the postgresql backend. Note that this will make only one database commit as it uses
-- `runSqlConn` internally.
writeEntries :: Pool SqlBackend -> BlockContext -> AccountTransactionIndex -> Seq.Seq SpecialTransactionOutcome -> IO ()
writeEntries pool BlockContext{..} ati stos = do
  runPostgres pool c
  where c :: ReaderT SqlBackend (NoLoggingT IO) ()
        c = do
          let
              -- In these collections the transaction outcomes are
              -- mapped to the database values.
              atiWithDatabaseValues = reverse $ -- reverse is because the latest entry is the head of the list
                fmap (\(k, v) -> (createSummary (Left v)
                                , Entry (ByteStringSerialized k))) (accountLogIndex ati)

              ctiWithDatabaseValues = reverse $
                fmap (\(k, v) -> (createSummary (Left v)
                                , ContractEntry (contractIndex k) (contractSubindex k))) (contractLogIndex ati)

              -- Insert all the Summaries, get the keys in the same query (postgresql does it in one query)
              -- and insert all the entries after adding the correct `Key Summary` to each one of them.
              runInsertion :: [(Summary, Key Summary -> Entry)] -> ReaderT SqlBackend (NoLoggingT IO) ()
              runInsertion xs = do
                xskeys <- insertMany (map fst xs)
                insertMany_ (zipWith snd xs xskeys)

              runContractInsertion :: [(Summary, Key Summary -> ContractEntry)] -> ReaderT SqlBackend (NoLoggingT IO) ()
              runContractInsertion xs = do
                xsKeys <- insertMany (map fst xs)
                insertMany_ (zipWith snd xs xsKeys)

          runInsertion atiWithDatabaseValues
          runContractInsertion ctiWithDatabaseValues
          mapM_ summarizeSTO stos
        summarizeSTO :: SpecialTransactionOutcome -> ReaderT SqlBackend (NoLoggingT IO) ()
        summarizeSTO sto = do
          -- Insert the summary for the special transaction outcome
          k <- insert $ createSummary (Right sto)
          -- Index the summary for every account mentioned in the STO
          insertMany_ [Entry (ByteStringSerialized acc) k | acc <- stoAccounts sto]
        stoAccounts :: SpecialTransactionOutcome -> [AccountAddress]
        stoAccounts BakingRewards{..} = Map.keys . accountAmounts $ stoBakerRewards
        stoAccounts Mint{..} = [stoFoundationAccount]
        stoAccounts FinalizationRewards{..} = Map.keys . accountAmounts $ stoFinalizationRewards
        stoAccounts BlockReward{..} = [stoBaker, stoFoundationAccount]
        createSummary :: PersistentTransactionOutcome -> Summary
        createSummary v = Summary {
          summaryBlock = ByteStringSerialized bcHash,
          summarySummary = AE.toJSON v,
          summaryTimestamp = bcTime,
          summaryHeight = bcHeight}
