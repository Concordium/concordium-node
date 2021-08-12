# Changelog

## Unreleased changes

- Update dependencies, code cleanup, and removal of the staging_net feature. No functional changes.
- Fix a bug in average throughput calculation that was triggered in some cases
  of bad connectivity.
- Add support for configuring the node and the collector via environment
  variables in addition to command line arguments. This is a breaking change in
  that flags now need to have an explicit argument.
- Disable dnssec by default. This replaces the flag `--no-dnssec` with
  `--require-dnssec`, and the environment variable
  `CONCORDIUM_NODE_CONNECTION_NO_DNSSEC` with `CONCORDIUM_NODE_CONNECTION_REQUIRE_DNSSEC`.
- Global state database now includes version metadata. The treestate directory and blockstate file
  names are suffixed with "-0" to indicate genesis index 0, for compatibility with protocol updates.
  A legacy database will automatically be migrated by renaming and adding version metadata.
- Remove unused CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_SERVER option and the
  corresponding `--bootstrap-server` flag.
- Change the automatically created indices on the transaction logging database.
  Instead of an index on the `id` column on `ati` and `cti` tables there are now
  multi-column indices that better support the intended use-cases. This only
  affects newly created databases.
- In the GetRewardStatus GRPC call, the amounts that were previously represented as integers are now
  represented as strings in the JSON serialization. This is in line with how amounts are serialized
  elsewhere.
- Support [log4rs](https://docs.rs/log4rs/1.0.0/log4rs/) logging, by specifying a configuration file
  (in toml or yaml format) with the `--log-config` argument or `CONCORDIUM_NODE_LOG_CONFIG`
  environment variable.
- A Windows node runner service and installer.
  See [service/windows/README.md](service/windows/README.md).

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
