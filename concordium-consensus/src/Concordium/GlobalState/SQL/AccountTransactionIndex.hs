{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.SQL.AccountTransactionIndex where

import Concordium.Types
import Concordium.Types.Block
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
import qualified Data.Text as Text

import Control.Monad.Logger
import Control.Monad.Reader

-- |Type marker for the SQL account transaction index
data SQLTransactionLog

-- |Context for transaction logging to an SQL database.
data SQLTransactionLogContext = SQLTransactionLogContext {
    -- |The database connection pool.
    connectionPool :: !(Pool SqlBackend),
    -- |The absolute height of the genesis block.
    genesisAbsoluteHeight :: !AbsoluteBlockHeight
  }

type instance ATIValues SQLTransactionLog = AccountTransactionIndex
type instance ATIContext SQLTransactionLog = SQLTransactionLogContext

connectPostgres :: ConnectionString -> IO (Pool SqlBackend)
connectPostgres connString = runNoLoggingT (createPostgresqlPool connString 5)

runPostgres :: Pool SqlBackend -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runPostgres pool c = runNoLoggingT (runSqlPool c pool)

-- | Create the database tables that are used when inserting transaction outcomes.
-- Three tables are created, "ati", "cti" and "summaries", with the first two having foreign keys
-- in the third.
--
-- The ati and cti tables have the pair of address and id as the primary key.
-- The way these tables are used, which is to lookup of some transactions for a
-- given address, this provides reasonable performance.
createStatement :: Text.Text
createStatement = Text.unlines [
  "CREATE TABLE summaries(id SERIAL8 PRIMARY KEY UNIQUE, block BYTEA NOT NULL, timestamp INT8 NOT NULL, height INT8 NOT NULL, summary JSONB NOT NULL);",
  "CREATE TABLE ati(id SERIAL8, account BYTEA NOT NULL, summary INT8 NOT NULL, CONSTRAINT ati_pkey PRIMARY KEY (account, id), CONSTRAINT ati_summary_fkey FOREIGN KEY(summary) REFERENCES summaries(id) ON DELETE RESTRICT  ON UPDATE RESTRICT);",
  "CREATE TABLE cti(id SERIAL8, index INT8 NOT NULL,subindex INT8 NOT NULL,summary INT8 NOT NULL, CONSTRAINT cti_pkey PRIMARY KEY (index, subindex, id), CONSTRAINT cti_summary_fkey FOREIGN KEY(summary) REFERENCES summaries(id) ON DELETE RESTRICT  ON UPDATE RESTRICT);"
  ]

-- |Result of checking existence of SQL tables needed for transaction logging.
data CheckTableResult =
  Ok
  -- | There are no tables with the given names
  | NoTables
  -- | Some of the tables exist, but they have an incorrect format.
  | IncorrectFormat

-- |Check whether correct tables exists in the connected database.
checkTablesExist :: Pool SqlBackend -> IO CheckTableResult
checkTablesExist pool = runPostgres pool $ do
  (numAti, noExtraAti) <- checkTableExists atiExistsStmt
  (numCti, noExtraCti) <- checkTableExists ctiExistsStmt
  (numSummaries, noExtraSummaries) <- checkTableExists summariesExistsStmt
  if numAti == 0 && numCti == 0 && numSummaries == 0 then return NoTables
  else if numAti == 3 && numCti == 4 && numSummaries == 5 && noExtraAti && noExtraCti && noExtraSummaries then return Ok
  else return IncorrectFormat
  where checkTableExists :: Text.Text -> ReaderT SqlBackend (NoLoggingT IO) (Int, Bool)
        checkTableExists stmt = do
          res <- rawSql stmt []
          let numRows = length res
          return (numRows, all unSingle res)

        atiExistsStmt = "SELECT (column_name, data_type) IN (('id', 'bigint'), ('account', 'bytea'), ('summary', 'bigint')) FROM information_schema.columns WHERE table_name='ati';"
        ctiExistsStmt = "SELECT (column_name, data_type) IN (('id', 'bigint'), ('index', 'bigint'), ('subindex', 'bigint'), ('summary', 'bigint')) FROM information_schema.columns WHERE table_name='cti';"
        summariesExistsStmt = "SELECT (column_name, data_type) IN (('id', 'bigint'), ('block', 'bytea'), ('timestamp', 'bigint'), ('height', 'bigint'), ('summary', 'jsonb')) FROM information_schema.columns WHERE table_name='summaries';"


createTables :: Pool SqlBackend -> IO ()
createTables pool = runPostgres pool (rawExecute createStatement [])

type PersistentTransactionOutcome = Either TransactionSummary SpecialTransactionOutcome

-- |Write the outcomes of the transactions and the special transaction outcomes of a block
-- into the postgresql backend. Note that this will make only one database commit as it uses
-- `runSqlConn` internally.
writeEntries :: SQLTransactionLogContext -> BlockContext -> AccountTransactionIndex -> Seq.Seq SpecialTransactionOutcome -> IO ()
writeEntries SQLTransactionLogContext{..} BlockContext{..} ati stos = do
  runPostgres connectionPool c
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
        stoAccounts PaydayFoundationReward{..} = [stoFoundationAccount]
        stoAccounts PaydayAccountReward{..} = [stoAccount]
        stoAccounts BlockAccrueReward{} = []
        stoAccounts PaydayPoolReward{} = []
        createSummary :: PersistentTransactionOutcome -> Summary
        createSummary v = Summary {
          summaryBlock = ByteStringSerialized bcHash,
          summarySummary = AE.toJSON v,
          summaryTimestamp = bcTime,
          summaryHeight = localToAbsoluteBlockHeight genesisAbsoluteHeight bcHeight}
