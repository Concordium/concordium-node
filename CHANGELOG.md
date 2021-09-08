# Changelog

## Unreleased changes

- Fix response of the transaction status query. Due to incorrect serialization
  the response was incorrectly reported as finalized even if transaction was
  only committed.

## concordium-node 1.1.0

- Node improvements and new features
  - Update dependencies, code cleanup, and removal of the staging_net feature.
  - Fix a bug in average throughput calculation that was triggered in some cases
    of bad connectivity.
  - Add support for configuring the node and the collector via environment
    variables in addition to command line arguments. This is a breaking change in
    that flags now need to have an explicit argument.
  - Remove use of `unbound` for DNS resolution. This also removes the dnssec functionality, and the
    flag `--no-dnssec` is no longer supported. The command-line option `--resolv-conf` is also
    removed, as the system resolver will be used.
  - Introduce protocol P2 that supports transfers with memo.
  - Implement protocol updates, allowing migration to a new protocol. Two protocol updates are
    implemented. One which does not switch to a new protocol version, but allows for updating a number
    of genesis parameters that would otherwise be immutable, while retaining the
    state. A second protocol update updates from P1 to P2.
  - Reduced logging of certain events. Received blocks, finalization messages/records, and
    transactions are only logged at Trace level. GRPC queries are not logged.
  - Support [log4rs](https://docs.rs/log4rs/1.0.0/log4rs/) logging, by specifying a configuration file
    (in toml or yaml format) with the `--log-config` argument or `CONCORDIUM_NODE_LOG_CONFIG`
    environment variable.
  - A Windows node runner service and installer.
    See [service/windows/README.md](service/windows/README.md).

- Network API changes
  - Remove unused `--bootstrap-server` flag.
  - Support for a new wire-protocol version (version 1) that adds a genesis index to non-transaction
    messages, a version header to catch-up messages. Version 0 is still supported for communication
    with legacy nodes during migration.
  - Relax compatibility check so that the node only checks a lower bound on the
    peer major version, in contrast to requiring an exact major version match.

- Database changes
  - Global state database now includes version metadata. The treestate directory and blockstate file
    names are suffixed with "-*n*" to indicate genesis index *n*.
    A legacy database will automatically be migrated by renaming and adding version metadata.
  - Change the automatically created indices on the transaction logging database.
    Instead of an index on the `id` column on `ati` and `cti` tables there are now
    multi-column indices that better support the intended use-cases. This only
    affects newly created databases.
  - The block export format has been revised to a new version (version 3) which allows for
    protocol updates. Version 2 is no longer supported.

- GRPC API changes
  - In the GetRewardStatus GRPC call, the amounts that were previously represented as integers are now
    represented as strings in the JSON serialization. This is in line with how amounts are serialized
    elsewhere.
  - The behaviour of GetAccountList, GetInstances, and GetModuleList have changed in the case
    where the block hash is ill-formed. Instead of returning the JSON string "Invalid block hash.",
    these functions will now return the JSON null value.
  - GetConsensusStatus return value has additional fields to indicate the protocol
    version and effected protocol updates.
  - GetBlockInfo return value has additional fields indicating the genesis index and local height
    (relative to the genesis block at the genesis index) of the block. The block height reported
    is absolute. The reported parent of a re-genesis block will be the last final block of the
    preceding chain, so the absolute height indicates how many times the parent pointer should be
    followed to reach the initial genesis.
  - GetBlocksAtHeight supports specifying a genesis index, with the supplied height being treated as
    relative to the genesis block at that index. An additional flag allows restricting the query to
    just that index.

## concordium-node 1.0.1

- Check that baker keys are consistent (private key matches the public one) on startup.
- Prevent rebroadcast of catch-up status messages.

## concordium-node 1.0.0

- Expose accountIndex in the `getAccountInfo` query.
- Fix serialization bug in smart contracts get_receive_sender when the address
  is that of an account.

## concordium-node 0.7.1

- Make sure to not rebroadcast transactions that consensus marked as not
  rebroadcastable.
- Fix command-line parsing issue in concordium-node related to --import-blocks-from.

## concordium-node 0.7.0

Start of changelog.
