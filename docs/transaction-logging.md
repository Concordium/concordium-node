# Running a node with finalized transaction logging

The node can be started in a configuration where it logs outcomes of finalized blocks in a Postgres database, and creates an index by affected account and smart contract.
This can be used for monitoring incoming transactions on an account.
Logging into this database is relatively expensive so it is not enabled in the default configuration.

To enable it the following configuration options must be given. We list environment variables here, but equivalent command-line flags are available, see `concordium-node --help` for their names.

- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING=true`
  This enables transaction outcome logging. If this is set the remaining options must be set.
- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_NAME`, the name of the database to log into. The database must exist, otherwise the node will fail to start.
- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_HOST`, host name or IP of the database, e.g., `127.0.0.1`
- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_PORT`, port on which the Postgres database is available, e.g., `5432`
- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_USERNAME`, username to log in to the database
- `CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING_PASSWORD`, password to log in to the database

As mentioned above the database must exist, otherwise the node will fail to start. If correct tables exist in the database then they will be used, otherwise the following will be executed upon startup
```sql
CREATE TABLE "summaries"("id" SERIAL8  PRIMARY KEY UNIQUE,"block" BYTEA NOT NULL,"timestamp" INT8 NOT NULL,"height" INT8 NOT NULL,"summary" JSONB NOT NULL)
CREATE TABLE "ati"("id" SERIAL8  PRIMARY KEY UNIQUE,"account" BYTEA NOT NULL,"summary" INT8 NOT NULL)
CREATE TABLE "cti"("id" SERIAL8  PRIMARY KEY UNIQUE,"index" INT8 NOT NULL,"subindex" INT8 NOT NULL,"summary" INT8 NOT NULL)
ALTER TABLE "ati" ADD CONSTRAINT "ati_summary_fkey" FOREIGN KEY("summary") REFERENCES "summaries"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT
ALTER TABLE "cti" ADD CONSTRAINT "cti_summary_fkey" FOREIGN KEY("summary") REFERENCES "summaries"("id") ON DELETE RESTRICT  ON UPDATE RESTRICT
```

which creates three tables, `ati`, `cti`, and `summaries`. The `ati` and `cti` stand for **a**ccount, respectively **c**ontract, **t**ransaction **i**ndex. They contain an index so that a transaction affecting a given contract or account can be quickly looked up. The outcome of each transaction is in the `summaries` table.

The summary that is stored in the `summary` column of the `summaries` table is stored as a JSON value. The contents is either of the form
```json
{
    "Left": ...
}
```
where `...` is a transaction outcome in the same format as it appears in block summaries, or
```json
{
    "Right": ...
}
```
where `...` is a special transaction outcome in the same format as it appears in block summaries.

## Account transaction index

The meaning of the `(id, account, summary_id)` row in the `ati` table is that account `account` was affected by transaction pointed to by `summary_id`. **Affected** here means that either the account sent the transaction, or it was the target of it, for example another account sent a transfer to it. Note that accounts are stored in binary format, so as 32-byte arrays, and not in their Base58check encoding.

The data is written to the table upon each finalization from oldest to newest block finalized by that round.
For each block transactions are written in the order they appear in the block, that is, from start to end of the block.
The ids in all tables are automatically generated. Note that they should not be relied upon to be strictly sequential. Postgres does not guarantee this. It only guarantees that they will be strictly increasing, but there might be gaps.

The node will never update any rows in the database, it only ever appends data to the tables.

## Contract transaction index

The meaning is analogous to the account transaction index, except here the node logs transactions that affect smart contracts.

# Caveats

There are a number caveats to keep in mind when running with transaction logging enabled.

## Performance impact

Transaction logging happens upon each finalization. In case that there are many transactions in blocks that are finalized, or if finalization is slow, it can take a lot of time to log all transactions. Currently transaction logging is done synchronously, meaning consensus is blocked until all transactions are logged. For baker nodes this can cause significant delays and cause missed opportunities for producing blocks. For this reason baker nodes generally should not have transaction logging enabled.

## Consistency

Transaction logging, saving blocks to the node's database, and storing block state happen in separate databases, and consequently there is no atomicity. The order of writes is
1. save block state
2. save the block and any finalization records
3. log affected transactions

If the node experiences failure between steps 2 and 3 then upon restart it will not log transactions finalized in the affected block. The node will not detect this.

# Examples

## Most recent transactions

The database can be polled for transactions affecting a given account, for example to get the most recent transactions affecting an account a query like the following could be used

```sql
SELECT summaries.block, summaries.timestamp, summaries.summary
FROM ati JOIN summaries ON ati.summary = summaries.id
WHERE ati.account = $1
ORDER BY ati.id DESC LIMIT $2
```

where `$1` would be the given account address, and `$2` the number of desired transactions.

## Notifications

Postgres supports [Notifications](https://www.postgresql.org/docs/current/sql-notify.html) and [Listening](https://www.postgresql.org/docs/current/sql-listen.html) on channels. This can be used to replace polling for updates, in some cases, with subscriptions.

One way to achieve this is to register triggers on, `ati` and `cti` tables (or just one of them).
For example, a trigger that would send notifications to `listen_channel` for each row inserted into the `ati` table would look as follows.

```sql
CREATE FUNCTION pg_temp.notify_insert ()
RETURNS trigger
LANGUAGE plpgsql
as $$
BEGIN
  PERFORM (
    WITH summary AS
    (
      SELECT summaries.summary FROM summaries WHERE summaries.id = NEW.summary
    )
    SELECT pg_notify(TG_ARGV[0], summary::text) from summary
  );
  RETURN NULL;
END;
$$;

CREATE TRIGGER notify_on_insert_into_ati
AFTER INSERT
ON ati
FOR EACH ROW
EXECUTE PROCEDURE pg_temp.notify_insert('listen_channel');

LISTEN listen_channel;
```
Note that the use of the `pg_temp` schema means that these triggers will only apply to the current Postgres session, i.e., they will be dropped upon disconnect. If this is not desired then use a different schema (or no schema at all).

For each row that is inserted this will notify the `listen_channel` with the summary. Note that it is not guaranteed that there will be one notification for each insertion. If summaries are equal then Postgres is allowed to coalesce multiple notifications into one. To get one notification per insertion adjust the summary to include the row id, for example.

### Caveats

- Notifications are not queued if there are no listeners on a given channel. If the client disconnects and reconnects any insertions that happened during the time the client was offline will not be sent on the channel. Listeners must put additional recovery logic on top to make sure they do not miss events in such cases.

- Postgres has a limited buffer for notifications (by default 8GB). If the client that is listening is too slow in processing them then eventually this buffer will get full, and notifications will start being dropped.

- The size of a notification payload is limited to `8kB`, so notifications cannot be used as a general push mechanism. Instead they are best used to notify of events, so that the client knows to make relevant queries as a result. This might be more efficient if it replaces frequent polling for events that happen rarely.
