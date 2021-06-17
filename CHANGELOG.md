# Changelog

## Unreleased changes

- Update dependencies, code cleanup, and removal of the staging_net feature. No functional changes.
- Fix a bug in average throughput calculation that was triggered in some cases
  of bad connectivity.
- Add support for configuring the node and the collector via environment
  variables in addition to command line arguments.

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
