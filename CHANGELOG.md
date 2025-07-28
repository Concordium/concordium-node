# Changelog

## Unreleased changes
- Protocol-level tokens:
  - Additional check in transaction verification asserting that the effective
    time equals zero of CreatePLT update transactions.

- Add P8 -> P9 update.
- Update GHC version to 9.10.2 (lts-24.0).

- Protocol-level tokens:
  - Change energy charging to occur as early as possible in the token module.
  - Added `touch` kernel method. The `touch` method initializes the token state
    of an account by setting its balance to zero. This method only affects
    accounts that have no existing state for the token.

## 9.0.6 (DevNet)

- Protocol-level tokens:
  - Adjusted energy cost of mint/burn from 100 to 50
  - PLTs can now only be created with the module reference:
    5c5c2645db84a7026d78f2501740f60a8ccb8fae5c166dc2428077fd9a699a4a
  - Support for token pause/unpause operations.

## 9.0.5 (DevNet)

- Protocol-level tokens:
  - Simplified token transaction handling: Consolidated all token holder and
    governance operations for a single `TokenUpdateTransaction` type.
  - Moved authorization from the scheduler to the token module.
  - Fix a bug in checking authorization for CreatePLT update.

## 9.0.4 (DevNet)

- Protocol-level tokens:
  - Changes to event representation and serialization.

## 9.0.3 (DevNet)

- Protocol-level tokens:
  - `GetAccountInfo` reports the token module-defined state as CBOR-encoded, rather than directly
    exposing the allow/deny list membership at the GRPC level.
  - `metadata` in token module state (reported in `GetTokenInfo`) can now include the metadata
    hash as well as the URL.
  - A new set of authorized level-2 keys for the `CreatePLT` chain update.

## 9.0.2 (DevNet)

- Protocol-level tokens:
  - Token operations emit events. This includes `CreatePLT`.
  - Support for energy charging for token module execution.

## 9.0.1 (DevNet)

- Protocol-level tokens:
  - Support for token governance transactions (mint, burn, modify allow/deny lists).
  - Enforcement of allow and deny lists in transfer operations.
  - `GetTokenInfo` returns state from token module.
  - `GetAccountInfo` reports state of allow/deny lists.
  - Changes to the caching and storage of account-level token state.


## 9.0.0 (DevNet)

- Preliminary support for protocol-level tokens (as part of protocol version 9), including:
  - Support for `CreatePLT` chain update for creating a new protocol-level token.
  - Support for transferring protocol-level tokens between accounts with the `TokenHolder`
    transaction type.
  - API support:
    - Add `GetTokenList` query for getting a list of all protocol-level tokens.
    - Add `GetTokenInfo` query for getting details about a specific protocol-level token.
    - `GetAccountInfo` query displays balances of protocol-level tokens held by an account.

## 8.1.0

- Replace `BufferedRef` with `HashedBufferedRef` in `PoolRewards`
  `bakerPoolRewardDetails::LFMBTree` field to cache computed hashes.
- Improvements to the loading of modules. This particularly improves the performance of
  `GetModuleSource` in certain cases, and can also reduce start-up time.
- Use a persistent LMDB-backed store to track the finalized module map.
- Fix a bug that affects setting up the account map correctly for non-finalized certified blocks
  that contain account creations (#1329).
- Fix a bug that can occasionally result in a crash if `GetBlockInfo` is invoked during a protocol
  update ([#1352](https://github.com/Concordium/concordium-node/issues/1352)). The fix delays
  executing the on-block and on-finalization handlers until after the state update has been
  committed. This also should also result in better consistency in the gRPC API (i.e. if a client
  is notified that a block has arrived, `GetBlockInfo` should not result in `NOT_FOUND` for thatb
  block).

## 8.0.3

- Fix a bug where, after a protocol update in consensus version 1 (P6 onwards), a node may
  miscalculate the absolute height of blocks when it is restarted. (#1319)
- Fix a bug where `GetBlockInfo` reports the parent block of a genesis block to be the last
  finalized block of the previous genesis index, instead of the terminal block.

## 8.0.2

- Fix a bug where the P7->P8 protocol update affects payday timing.

## 8.0.1

- Fix a bug in computing the number of missed rounds in the event of a timeout.
- Fix a bug where the suspended status of a baker pool would be omitted when it was suspended.
- Fix a bug where `GetBlockChainParameters` returns a `ChainParametersV2` in cases where it should
  return `ChainParametersV3`.

## 8.0.0

- Add P7 -> P8 update.
- Automatically suspend validators from the consensus that missed too many
  rounds in the previous payday.
- Add support for suspend/resume to validator configuration updates.
- Add support to add a validator in a suspended state.
- Validators that are suspended are paused from participating in the consensus algorithm.
- Add suspension info to `BakerPoolStatus` / `CurrentPaydayBakerPoolStatus` query results.
- Add `GetConsensusDetailedStatus` gRPC endpoint for getting detailed information on the status
  of the consensus, at consensus version 1.
- Update Rust version to 1.82.
- Update GHC version to 9.6.6 (LTS-22.39).
- Add `GetScheduledReleaseAccounts` endpoint for querying the list of accounts that
  have scheduled releases.
- Add `GetCooldownAccounts`, `GetPreCooldownAccounts` and `GetPrePreCooldownAccounts`
  endpoints for querying the lists of accounts that have pending cooldowns in protocol
  version 7 onwards.
- gRPC endpoints `DryRun`, `GetBlockItemStatus` and `GetBlockTransactionEvents` now report the
  parameter used to initialize a smart contract instance as part of a `ContractInitializedEvent`.

## 7.0.5

- Fix inconsistent handling of valid contract names.

## 7.0.4

- Fix a bug where the next payday time reported by the `GetTokenomicsInfo` query was
  incorrect (#1240).

## 7.0.3

- Fix a bug in the computation of the genesis height after the second protocol update. (#1237)
- Fix a bug where an error was incorrectly thrown when loading the consenus state immediately
  after a protocol update (in the new consensus version) (#1236).

## 7.0.2

- Fix the timing of paydays after protocol update from version 6 to 7.
- Improve consensus behaviour in the event of an unrecoverable exception.

## 7.0.1

- Fix a bug in migration from protocol version 6 to 7.
- Support "reboot" protocol update at protocol version 7.

## 7.0.0

- Fix a bug where `GetBakersRewardPeriod` returns incorrect data (#1176).
- Fix a bug where `GetPoolInfo` returns incorrect data (#1177).
- Change the severity of logs for failed gRPC API requests to DEBUG level.
- Add support for new `invoke` calls from smart contracts in protocol version 7:
  - query the contract module reference for a given contract address
  - query the contract name for a given contract address
- Update Rust version to 1.73.
- Integrate new Wasm execution engine. Migration of old execution artifacts in
  the node's database to the new format is done on-demand, which means node
  startup can be a bit slower when a lot of modules exist. All Wasm modules will
  be migrated to the new format when the protocol is updated to P7.
- Remove support for encrypted transfers in protocol version 7. Transactions with
  payload types `TransferToEncrypted`, `EncryptedAmountTransfer` and
  `EncryptedAmountTransferWithMemo` are disabled in this protocol version, which
  prevents encrypting further CCDs or transferring encrypted CCDs.
  `TransferToPublic` remains enabled, allowing existing encrypted balances to be
  decrypted.
- Improve logging around protocol update events.
- Changes to stake cooldown behavior in protocol version 7:
  - When stake is reduced or removed from a validator or delegator, it becomes
    inactive, and is not counted for future stake calculations. The inactive
    stake is not spendable, but is released after a cooldown period elapses.
  - Changes to validators and delegators can be made while stake is in cooldown,
    including changing the stake, or changing directly between validator and
    delegator.
- Fix a bug where a configure-validator transaction that is rejected for having
  a duplicate aggregation key would report the old key for the validator,
  rather than the key that is a duplicate.

## 6.3.1

- Fix a bug where a node may fail to produce a timeout certificate due to
  incorrectly computing the total weight of finalizers that have signed
  timeout messages.

## 6.3.0

- Fix a bug where `GetBlockPendingUpdates` fails to report pending updates to the finalization
  committee parameters.
- Run GRPC queries in dedicated threads. This improves node resource management
  and increases responsiveness of the GRPC server in cases of high number of
  concurrent queries. To support this a new option `--grpc2-max-threads`
  (environment variable `CONCORDIUM_NODE_GRPC2_MAX_THREADS`) is
  added which specifies the number of threads that the node should use for
  processing gRPC requests. If not set this defaults to the number of (logical)
  CPUs.
- The option `--grpc2-max-concurrent-streams` now defaults to `200` from the
  previous unbounded value. This makes the node defaults safer.
- Update GHC version to 9.6.4 (lts-22.9).

## 6.2.3

- Fix a bug that caused the node to crash on Windows when processing a protocol update.

## 6.2.2

- The transaction table is only used for tracking next available account nonce
  for accounts that have non-finalized transactions. This reduces memory usage
  and startup time.

- Add options `CONCORDIUM_NODE_VALIDATOR_CREDENTIALS_FILE` and
  `CONCORDIUM_NODE_VALIDATOR_DECRYPT_CREDENTIALS` that alias
  `CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE`
  `CONCORDIUM_NODE_BAKER_DECRYPT_CREDENTIALS`, respectively
  The latter two options are still available, but hidden in the help text.
- Support `validatorId` in addition to `bakerId` in the credentials to start a validator.

## 6.2.1

- The account map is now kept solely on disk in a separate lmdb database and it is no longer part of the internal block state database.
  This change results in less memory usage per account and a decrease in the growth of the database.
- Remove V1 GRPC API from the node. This removes configuration options
  `CONCORDIUM_NODE_RPC_SERVER_PORT`, `CONCORDIUM_NODE_RPC_SERVER_ADDRESS`, 
  `CONCORDIUM_NODE_RPC_SERVER_TOKEN`, `CONCORDIUM_NODE_DISABLE_RPC_SERVER_NODE_ENDPOINTS`
  and their command line equivalents.

## 6.2.0

- Add an additional health-check service to the V2 GRPC API.
  This service conforms to the [standard GRPC health service API](https://github.com/grpc/grpc-proto/blob/master/grpc/health/v1/health.proto).
- New `DryRun` endpoint that allows simulating the execution of transactions.

## 6.1.7

- Add load-shedding to the V2 GRPC API. In particular, if at the time of the
  request the node is already handling more than
  `CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_REQUESTS` requests then the incoming
  request will be immediately rejected.

## 6.1.6

- Fix a regression in the start up time. When upgrading from an earlier version, the first start-up
  time may be longer than usual, as the genesis state hashes are computed. Subsequent restarts
  will not suffer this penalty.

## 6.1.5

- Enable out of band catchup by default in all distributions.

## 6.1.4

- Expose the health check service via grpc-web when grpc-web is enabled.
 Support for out-of-band catch-up for protocol versions beyond P6.
- Extend Prometheus exporter with metric `grpc_connected_clients`, see
  [docs/prometheus-exporter.md](https://github.com/Concordium/concordium-node/blob/main/docs/prometheus-exporter.md) for more details.
- Add configuration options for imposing resource limits to the V2 gRPC server.
  The following environment variables are added
  - `CONCORDIUM_NODE_GRPC2_KEEPALIVE_INTERVAL`
  - `CONCORDIUM_NODE_GRPC2_KEEPALIVE_TIMEOUT`
  - `CONCORDIUM_NODE_GRPC2_TCP_KEEPALIVE`
  - `CONCORDIUM_NODE_GRPC2_INVOKE_MAX_ENERGY`
  - `CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_REQUESTS`
  - `CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_REQUESTS_PER_CONNECTION`
  - `CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_STREAMS`
  - `CONCORDIUM_NODE_GRPC2_MAX_CONNECTIONS`
  - `CONCORDIUM_NODE_GRPC2_REQUEST_TIMEOUT`

  See
  [docs/grpc2.md](https://github.com/Concordium/concordium-node/blob/main/docs/grpc2.md)
  for an explanation of the options.
 
## 6.1.3

- Fix a bug where stored peers are removed incorrectly.
- Fix incorrect `peer_bucket_size` metric calculation exposed by the
  bootstrapper. What was counted was not the number of peers in the bucket, but
  rather, roughly, how many times peers that are in the bucket have
  reconnected.

## 6.1.2

- Fix a bug where the block state hash was not returned properly for the genesis block.
- Do not reset banned peers on startup by default.
- The node remembers peers across restarts. When starting up it will try to connect to stored peers in addition to any supplied bootstrap and given nodes. 
  Use the new flag `--clear-persisted-peers` (environment variable  `CONCORDIUM_NODE_CLEAR_PERSISTED_PEERS`) to clear stored peers on startup.
- Renamed the flag `--no-clear-bans` to `clear-bans`. When set it will clear the banned peers on startup.
- Fix a bug where credential registration IDs for genesis accounts were not
  correctly recorded. As a result, the index of accounts by credential ids was
  incorrect if the chain was started from genesis by node versions 5.1.3 up to
  and including 6.0. If a chain was started by an older node version and then
  the node was upgraded the index is loaded correctly. This index is used when
  checking for duplicate credential registration IDs, and when looking up an
  account via a credential registration ID.
- Fix a bug in the `InvokeInstance` endpoint where the amount sent was
  used incorrectly. The consequence was that in some cases the calls would fail
  with an error indicating insufficient amount on the account where the amount
  was sufficient for the transaction.
- Fix a bug where it was not possible to use the `collector` with a node configured with TLS.
  One has to configure the `grpc-host` flag of the `collector` with domain stated in the certificate that the
  node is configured with.

## 6.1.1

- Apply fix for processing of chain parameter updates when they occur at the same time
  retroactively to all protocol versions. This may break compatibility with any local/private
  chains on which the bug occurs.
- Remove the concept of pending blocks.

## 6.1.0

- `GetPoolInfo` now also returns the commission rates for the current reward period.
- Add `GetBakersRewardPeriod` to GRPCV2 API. Provided a block, then it returns information about bakers
  for the reward period of the block.
- Add endpoint `GetBlockCertificates` to GRPCV2 API. Provided a block, then it returns 
  quorum certificate, timeout certificate and epoch finalization entry contained in the block (where present).
- Add endpoint `GetBakerEarliestWinTime` to GRPCV2 API. Provided a baker ID, it returns the
  earliest time at which the node projects that the baker could be required to bake a block.
- Add endpoint `GetFirstBlockEpoch` to GRPCV2 API. It returns the block hash of the first block in
  a given epoch.
- Add endpoint `GitWinningBakersEpoch` to GRPCV2 API. It returns a list of the bakers that won
  rounds in a specified (finalized) epoch. This only supports consensus version 1.
- Fix a bug in how the last timeout certificate is recovered at start-up.
- Fix the behaviour of the block last finalized pointer in the `GetBlockInfo` so that it
  consistently returns the last finalized block at the time the block was baked.
- Add debug-level logging when a round is advanced, either due to a quorum certificate or a
  timeout certificate.

## 6.0.4

- Fix a bug in how timeout certificates across epoch boundaries are handled in catch-up.

## 6.0.3

- Update specification hash for protocol 5 to protocol 6 update.
- Fix processing of chain parameter updates when they occur at the same time.
- Fix a bug where out-of-band catch-up fails in P6 when processing blocks that have already been
  processed.
- Fix a bug where certified blocks are not written to disk in some circumstances.

## 6.0.2

- Fix a bug where the LMDB map was not resized when exporting the database. 
  This could cause the database exporter to fail when used on a running node.
- Fix a bug where the database exporter creates files in the wrong path when invoked with a
  relative `--exportpath`.
- Fix a bug where a setup with a single baker and a minimum block time of 0s would result in an
  unresponsive node in protocol version 6.
- Fix a bug where receiving a duplicate of an invalid block could be spuriously reported as double
  signing.
- Fix a bug where database roll-back could fail on Windows.
- Fix a bug where catch-up for ConcordiumBFT can loop or result in incorrect soft-banning of peers.
- Prevent the baker thread from starting when it is not required.
- Implement a "reboot" protocol update from P6 to P6 as a test of the protocol update mechanism.

## 6.0.1

- Remove configuration option `no-rpc-server` and environment variable
  `CONCORDIUM_NODE_DISABLE_RPC_SERVER`, as well as default values of
  `rpc-server-port` (`CONCORDIUM_NODE_RPC_SERVER_PORT`) and `rpc-server-addr`
  (`CONCORDIUM_NODE_RPC_SERVER_ADDR`). The V1 gRPC server is only started if
  both of these options are supplied.
- Fix a bug which caused account nonces and sequence numbers to not be migrated to P6 correctly.
- Add support for out-of-band export files for ConcordiumBFT (protocol version 6).
- Fix a network layer bug where initial messages after the handshake could be
  dropped in some circumstances.
- Fix a bug which caused the first epoch of the new protocol to be shorter than expected.
- Fix a bug that caused an incorrect reporting of total stake in the first
  payday just after genesis when the node started from genesis at protocols 4 or 5.
- Revise the behaviour of rollbacks in P6.
- Changes in Wasm validation and execution in P6
  - Disallow globals in initialization sections for V1 contracts in P6.
  - Support sign extension instructions in Wasm in P6.
  - Do not count custom sections towards module size when executing contracts.
  - Support new `invoke` operations for retrieving account keys and checking signatures.
- Shut down consensus upon a protocol update updating from protocol version 6.
- Revised persistent state for P6 with changes to startup and catch-up handling.

## 6.0.0

- Support the new ConcordiumBFT consensus (protocol version 6).
- Fix a bug that causes bakers in genesis to restake their earnings when they should not. This
  affects genesis data at protocol version P5; P1-P4 genesis data are not affected. This breaks
  compatibility with chains started with P5 genesis data, where some genesis bakers are not set to
  restake earnings. Other chains (including mainnet and testnet) are not affected.
- Changes to the `GetConsensusStatus` endpoint:
  * Slot duration only returned in protocol versions 0-5.
  * Endpoint extended to return current timeout duration, current round, current epoch and trigger
    block time in protocol version 6.
- Changes to the `GetBlockInfo` endpoint:
  * Block slot only returned in protocol versions 0-5.
  * In protocol version 6, the returned finalized block is the last finalized block until itself
    is finalized. Then it is itself.
  * Endpoint extended to return block round and epoch in protocol version 6.
- Changes to the `ElectionInfo` endpoint:
  * Election difficulty only returned in protocol versions 0-5.

## 5.4.2

- Revert a change in getModuleSource of the V1 GRPC API.

## 5.4.0

- Support using block height as block identifiers in gRPC v2 API.
- Extend gRPC v2 API call `GetBlockInfo` with the protocol version of the block.
- Do not use peer buckets when running as a normal node (not as a bootstrapper).
- Enable CORS support in grpc-web. This only applies when grpc-web is enabled.

## 5.3.2

- Extend Prometheus exporter with metric `peer_bucket_size`, see
  [docs/prometheus-exporter.md](https://github.com/Concordium/concordium-node/blob/main/docs/prometheus-exporter.md) for more details.

## 5.3.1

- Add an option `--grpc2-health-min-peers` (environment variable `CONCORDIUM_NODE_GRPC2_HEALTH_MIN_PEERS`)
  that causes the grpc V2 health endpoint to check minimum number of peers.
- Extend the node health check so that if the node is configured with baker
  credentials then it is required to be in the baking committee for it to be
  considered healthy.
- Add a new option `--grpc2-invoke-max-energy` (environment variable
  `CONCORDIUM_NODE_GRPC2_INVOKE_MAX_ENERGY`) that allows the node runner to
  control the maximum amount of energy allowed by an `InvokeInstance` (and the
  V1 GRPC `InvokeContract`) call. The behaviour of the endpoint is slightly
  changed as well. The `energy` is no longer required in the request, and the
  effective energy used by the call will be `min(request.energy,
  grpc-invoke-max-energy)`. This differs from the previous behaviour where a
  request would fail if the request either omitted the `energy`, or supplied an
  excessive value.
- Fix a bug that could cause the node to hang indefinitely during the out-of-
  band-catchup when the node is a finalizer.
- Fix an additional bug in `GetAccountInfo` endpoint in GRPCv2 where
  `incoming_amounts` field of encrypted amounts was not always set correctly.
- The node collector is migrated to a separate package and now uses the V2 GRPC API.


## 5.3.0

- Extend Prometheus exporter with metrics: `grpc_request_response_time_seconds`, `grpc_in_flight_requests`, `consensus_baking_committee`, `consensus_finalization_committee`, `consensus_baking_lottery_power`, `consensus_baked_blocks_total`, `consensus_finalized_baked_blocks_total`, `network_soft_banned_peers_total`, `consensus_non_finalized_transactions` and `consensus_unsupported_pending_protocol_version` see [docs/prometheus-exporter.md](https://github.com/Concordium/concordium-node/blob/main/docs/prometheus-exporter.md) for more details.

## 5.2.4

- Fix incorrect labelling of some catchup messages as invalid in Prometheus metrics.

## 5.2.3

- Internal refactoring to support the future consensus.

## 5.2.2

- Rename a number of metrics exposed by the Prometheus exporter:
  - `peer_number` is now `network_connected_peers`.
  - `conn_received` is now `network_connections_received_total`.
  - `packets_received` is now `network_packets_received_total`.
  - `packets_sent` is now `network_packets_sent_total`.
  - `inbound_high_priority_consensus_drops` is now `network_inbound_high_priority_message_drops_total`.
  - `inbound_low_priority_consensus_drops` is now `network_inbound_low_priority_message_drops_total`.
  - `inbound_high_priority_consensus_counter` is now `network_inbound_high_priority_messages_total`.
  - `inbound_low_priority_consensus_counter` is now `network_inbound_low_priority_messages_total`.
  - `inbound_high_priority_consensus_size` is now `network_inbound_high_priority_message_queue_size`.
  - `inbound_low_priority_consensus_size` is now `network_inbound_low_priority_message_queue_size`.
  - `outbound_high_priority_consensus_size` is now `network_outbound_high_priority_message_queue_size`.
  - `outbound_low_priority_consensus_size` is now `network_outbound_low_priority_message_queue_size`.
  - `bytes_received` is now `network_received_bytes`.
  - `bytes_sent` is now `network_sent_bytes`.
- Remove `last_throughput_measurement_timestamp`, `avg_bps_in` and `avg_bps_out` metrics exposed by the Prometheus exporter.
- Change behavior of Prometheus metrics `network_sent_bytes` and `network_received_bytes`. Before this change these metrics were calculated as a sum of all the bytes sent/received to peers, which causes the metrics to drop when a peer is dropped. They were only updated during the scheduled "housekeeping" (every 30 secons by default). The new behavior is to update the metric every time a message is sent/received to a peer.
- Extend Prometheus exporter with metrics: `consensus_last_finalized_block_height`, `consensus_last_finalized_block_timestamp`, `consensus_last_arrived_block_height`, `consensus_last_arrived_block_timestamp`, `consensus_received_messages_total`, `consensus_sent_messages_total`, `network_soft_banned_peers`, `network_peers_total`, `node_info` and `node_startup_timestamp` see [docs/prometheus-exporter.md](https://github.com/Concordium/concordium-node/blob/main/docs/prometheus-exporter.md) for more details.
- Remove metrics `network_inbound_high_priority_message_drops_total`, `network_inbound_low_priority_message_drops_total`, `network_inbound_high_priority_messages_total` and `network_inbound_high_priority_messages_total` as they can be derived using the labels of `consensus_received_messages_total`.

## 5.2.1

- Fix a bug in `GetAccountInfo` endpoint in GRPCv2 where `incoming_amounts`
  field of encrypted amounts was not set correctly.
- Remove `current_queue_size`,  `resend_queue_size`, `packets_dropped`, `invalid_packets_received`
  `unknown_packets_received`, `invalid_network_packets_received`,
  `packets_resend` metrics from the Prometheus server since they were never updated.
- Internal refactoring to support new consensus chain parameters.
- Internal refactoring to simplify configuration of the node's databases.

## 5.2.0

- Fix an issue where the node configuration file (`main.config.json`) was
  sometimes corrupted.
- Add an option to disable only the node specific grpc V1 endpoints that can be
  used to control the node. All the endpoints that are consensus related are
  kept allowing the node to be used as a gateway to the chain. The mentioned can
  be disabled by setting `CONCORDIUM_NODE_DISABLE_RPC_SERVER_NODE_ENDPOINTS`
  or using the flag `--no-rpc-server-node-endpoints`.

## 5.1.3

- Fix a bug in the `GetAccountInfo` endpoint in GRPCv2 where the `ar_data` field
  always would be empty.

## 5.1.2

- Avoid deadlocks during node shutdown in specific scenarios.
- The node will now shut down to start if an error occurs in a required service
  (e.g., grpc server). In particular, the node will shut down if a required
  service could not be started.
- Add timeout to downloading out of band catchup files when block indices and
  catch-up chunk files are specified by an URL. The timeout is controlled
  by the option `--download-blocks-timeout` (environment variable
  `CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_TIMEOUT`) and defaults to 5 min.
  timeout is 5 now minutes per chunk instead of waiting indefinitely.
- Remove the "instrumentation" feature of the node and build the node with
  Prometheus support enabled by default.
  - Remove the `CONCORDIUM_NODE_PROMETHEUS_SERVER` environment variable.
    The prometheus server is now started if
    `CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT` is set.

## 5.1.1

- Relay blocks earlier. In particular this means that blocks are now processed in
  two steps, `block receive` and `block execute`. The former performs verification of block meta data
  while the latter adds the block to the tree.
  Blocks are now enqueued in the outgoing message queue in between the the two steps.
- Removed the configuration option 'no_rebroadcast_consensus_validation'.

## 5.1.0

- Improvements to allow greater concurrency with transaction processing.
  (Checking transaction signatures is done without acquiring the global
  state lock.)

## 5.0.6

- Fix persistent state implementation of contract modifications.
  In certain cases the cached part of the instance was not correctly updated.
- Change the cost of the exchange rate query to be more aligned with other operations.
- Fix the behaviour of a smart contract upgrade when upgrading to a contract
  without entrypoints.

## 5.0.5

- Fix bug in persistent state implementation of contract modification.
- Fix bug in the contract balance query.
- Do not forget logs emitted by smart contracts.

## 5.0.3

- Fix P4->P5 state migration issues.
  - Delegators were not correctly copied over, references were retained to the
    P4 database.
  - Account stake was incorrectly recreated, all account balance was staked.
  - Next payday was incorrectly migrated.

## 5.0.2

- Fix an issue in the node GRPC V2 API where a baker transaction was encoded
  in an unintended way.
- Enforce parameter limits in `InvokeContract` endpoint.

## 5.0.1

- Fix an issue where accounts with scheduled releases would be incorrectly migrated
  on a protocol update.

## 5.0.0

- Add support for protocol version 5. This adds the following features:
  - Support for smart contract upgradability.
  - Query the current exchange rates, account balances and contract balances from a smart contract.
  - Relax restrictions on smart contracts
    - Parameter size limit: 1kiB -> 65kiB
    - Return value size limit: 16kiB -> no limit (apart from energy)
    - Number of logs per invocation: 64 -> no limit (apart from energy)
  - A new representation of accounts that is better optimised for common operations.
  - Revised the hashing scheme for transaction outcomes in protocol version 5.
    In particular the exact reject reasons are no longer part of the computed hash.
    Further the transaction outcomes are being stored in a merkle tree for P5 resulting 
    in some queries being faster.
- More efficient indexing of accounts with scheduled releases.
- Fix an issue where the catch-up downloader would fail at a protocol update.

## 4.5.0

- The node is now able to recover after crashes which leave only treestate or
  only blockstate usable.
- Fix a memory leak that could occur in certain usage scenarios involving smart
  contracts.
- Support for a new GRPC API which uses typed proto definitions. This adds a
  number of new configuration options for the node. These are detailed in
  [grpc2.md](https://github.com/Concordium/concordium-node/blob/main/docs/grpc2.md)

## 4.4.4

- Fix a bug in database recovery where the node would hang when truncating the block state database
  on Windows.

## 4.4.3

- Fix a bug in database recovery where corruption was not always correctly detected.
- Fix typo in environment variable `CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESSS` (remove trailing `S`).

## 4.4.2

- Speed up and reduce memory overhead during protocol updates.

- Smart contract modules are no longer retained in memory. Module artifacts are loaded as needed
  during contract execution. Metadata is cached for a limited number of smart contract modules.
  By default, the cache will retain metadata for at most 1000 smart contract modules, and this is
  configurable via the `--modules-cache-size` command line argument or by using the 
  `CONCORDIUM_NODE_CONSENSUS_MODULES_CACHE_SIZE` environment variable.

- Smart contract state is no longer cached on startup and is not cached after
  finalization.

- Partial node database recovery. The node is now able to recover from the most
  common causes of its database corruption.

## 4.4.1

- Verify pending blocks earlier when possible.
- Do not relay pending blocks.

## 4.4.0

- Fix a bug in Ctrl-C signal handling where a node would fail to stop if
  interrupted early on in the startup if out-of-band catchup was enabled.
- `database-exporter` now produces a collection of export files, instead of a single file. The new
  `--chunksize` option specifies the size of export files in blocks.
- The `--download-blocks-from` option now takes the URL to the catchup _index file_, permitting to
  only download and import catchup files containing blocks not already present in the database.

## 4.3.1

- Improvements to start-up time that fix regressions introduced by the account caching.

## 4.3.0

- Account records are no longer constantly retained in memory. Instead a limited
  number are retained in a cache. The number of cached accounts defaults to 10000,
  and can be configured by the `--accounts-cache-size` command line argument or the
  `CONCORDIUM_NODE_CONSENSUS_ACCOUNTS_CACHE_SIZE` environment variable.
- Reduce startup time and memory use further by reducing the amount of block
  data retained in memory. In particular finalized blocks are no longer stored
  in memory.
- Optimize node data structures related to accounts. This reduces node memory
  use and improves performance.
- Added the ability to download the catch-up file using the
  `--download-blocks-from` option (or `CONCORDIUM_NODE_CONSENSUS_DOWNLOAD_BLOCKS_FROM` environment variable).
- The gRPC API now reports correctly when the sender of a transaction did
  not have enough funds to cover the transaction costs.
- Remove obsolete and unused option `--max-expiry-duration`
- Remove transaction logging functionality from the node. It is replaced by an
  external service. As a consequence the `transaction-outcome-logging` family of
  command-line options are removed from the node.

## 4.2.3

- Fix a bug in the scheduler which would cause the node to crash when executing
  certain transactions. [Security advisory](https://github.com/Concordium/concordium-node/security/advisories/GHSA-44wx-3q8j-r8qr)

## 4.2.1

- Decrease node startup time and memory use by avoiding needless checks when
  loading the database.
- Improve startup time by avoiding processing already processed protocol
  updates.
- Decrease memory usage by not storing genesis blocks. This has the effect that
  the database produced by node versions >= 4.2.* cannot be used by node
  versions <= 4.1. The other direction works.
- Increase precision of block arrive and block receive times in the
  `GetBlockInfo` query.

## 4.1.1
- The `SendTransaction` function exposed via the gRPC interface now provides the caller with detailed error messages if the 
  transaction was rejected instead of just `False`. The function still returns `True` if 
  the transaction was accepted.
  The following gRPC error codes can be returned.
  - 'SUCCESS' The transaction was succesfully relayed to consensus.
  - 'INVALID_ARGUMENT' The transaction was deemed invalid or exceeds the maximum size allowed (the raw size of the transaction).
     In addition the error message contains information as to why the transaction was deemed invalid.
  - 'FAILED_PRECONDITION' The network was stopped due to an unrecognized protocol update.
  - 'DUPLICATE_ENTRY' The transaction was a duplicate.
  - 'INTERNAL' An internal error happened and as such the transaction could not be processed.
  The server will return a gRPC status if the transaction was deemed invalid. 
- Support for wire-protocol version 0 is dropped, meaning that the node cannot
  connect to peers that do not support wire-protocol version 1, which is supported
  since version 1.1.0.
- The macOS installer no longer overwrites the service files when reinstalling.
- Cache smart contract modules on startup from existing state to improve smart
  contract execution.
- Make consensus queries more robust, by validating input more extensively.
  This affects all queries whose input was a block or transaction hash.
  These queries now return `InvalidArgument` error, as opposed to `Unknown`
  which they returned previously.
- Fix issue #244: Collector to keep querying. Remove the parameter for maximum allowed
  times a gRPC call can fail and keeps `node-collector` querying forever.
- `GetAccountInfo` endpoint supports querying the account via the account index.
- Mac installer: Users now can leave one (but not both) of the net configurations empty
  when they don't want to configure a node for it.
  - On the initial installation, leaving a net configuration empty means that
    the start/stop app shortcuts and the application support folder for that net won't be installed.
- Implement baker pools and stake delegation for the P4 protocol version.
  - New gRPC endpoint: `GetBakerList` retrieves a JSON list of the baker IDs of the bakers
    registered in a known block. (Returns `null` for an unknown block.)
  - New gRPC endpoint: `GetPoolStatus` retrieves a status record for a baker pool, or for
    the set of passive delegators.
  - The `bakerStakeThreshold` level-2 keys are renamed as the `poolParameters` keys; two
    additional access structures are defined: `cooldownParameters` and `timeParameters`.
  - The following changes are made to the chain parameters in P4:
    - The mint distribution no longer includes the mint per slot rate.
    - Pool parameters are added, governed by the `poolParameters` keys, that determine
      commission rates and ranges, bounds and other factors affecting baker pools.
    - Time parameters, governed by `timeParameters`, are added that determine the
      duration of a payday (in epochs) and the mint rate per payday.
    - Cooldown parameters, governed by `cooldownParameters`, are added that determine
      the required cooldown periods when bakers and delegators reduce their stakes.
  - ConfigureBaker and ConfigureDelegator transactions are added (with the old baker
    transactions becoming obsolete in P4). These permit adding, modifying and removing bakers
    and delegators respectively from an account. Delegators can delegate to a baker, or
    delegate passively (effectively to all bakers).
  - The reward mechanism is overhauled, with minting and rewarding being done once per
    'payday' (a number of epochs, nominally one day). Baker rewards are shared with
    delegators to the baker's pool, with some commission being paid to the baker.
    Block rewards (i.e. transaction fee rewards), baking rewards and finalization rewards
    are accumulated over the reward period and paid out at the payday.
- Implement V1 smart contracts with the following key features
  - unlimited contract state size
  - synchronous contract calls
  - fallback entrypoints
  - increased smart contract module size limit, 512kB
  - a number of cryptographic primitives
- Node can now be stopped during out of band catchup by using signals, SIGINT and SIGTERM.

## concordium-node 3.0.1

- Fix a starvation bug in some cases of parallel node queries.

## concordium-node 3.0.0

- Fix a bug due to incorrect use of LMDB database environments where a node
  would crash if queried at specific times.
- Faster state queries by avoiding locking the block state file when reading.
- Fix a bug by shutting down RPC before the node, which caused the node to crash
  when attempting a graceful shutdown while processing RPC requests.
- Introduce support for account aliases via protocol P3. Accounts can be queried
  in `GetAccountInfo`, `GetAccountNonFinalizedTransactions`,
  `GetNextAccountNonce` by any alias.
- `GetAccountInfo` object has an additional field `accountAddress` that contains
  the canonical address of the account.
- The node now drops all connections on an unrecognized protocol update and
  refuses to accept new transactions.

## concordium-node 1.1.3

- Fix a number of bugs that led to node crashes due to failed block lookup in some situations.
- Support custom path for genesis data via `CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE`.

## concordium-node 1.1.2

- Fix regression where expired transactions were not immediately rejected.

## concordium-node 1.1.1

- Fix response of the transaction status query. Due to incorrect serialization
  the response was incorrectly reported as finalized even if transaction was
  only committed.
- For macOS:
  - Added option to use the native mac logging system. To enable it, provide
    the new flag `--use-mac-log <subsystem-name>`. The flag is available on both
    the node and collector with corresponding, respective, environment variables
    `CONCORDIUM_NODE_MACOS_USE_MAC_LOG` and `CONCORDIUM_NODE_COLLECTOR_USE_MAC_LOG`.
  - When the flag is provided, you can view the logs with `log show --predicate
    'subsystem == "<subsystem-name>"'`, or by using Console.app. 

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
