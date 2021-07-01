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
  `CONCORIDUM_NODE_CONNECTION_NO_DNSSEC` with `CONCORDIUM_NODE_CONNECTION_REQUIRE_DNSSEC`.
- Global state database now includes version metadata. The treestate directory and blockstate file
  names are suffixed with "-0" to indicate genesis index 0, for compatibility with protocol updates.
  A legacy database will automatically be migrated by renaming and adding version metadata.

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
