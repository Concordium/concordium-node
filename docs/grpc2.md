# GRPC API V2

This document describes the node configuration pertaining to the V2 GRPC
interface.

The V2 GRPC interface differs from the original mainly in that the encoding of
the responses uses [protocol
buffers](https://developers.google.com/protocol-buffers) compared to a mix of
JSON and protobuf for the original interface.

The type and service definition files are located in the
[concordium-grpc-api](https://github.com/Concordium/concordium-grpc-api)
repository in the `v2` directory.

By default the node does not enable the V2 API. The server can be enabled by
using `--grpc2-listen-addr` to specify the IP address to listen on, and
`--grpc-listen-port` to specify the port, options. The corresponding environment
variables are `CONCORDIUM_NODE_GRPC2_LISTEN_ADDRESS` and
`CONCORDIUM_NODE_GRPC2_LISTEN_PORT`.

If these are enabled then the following options become available

- `--grpc2-x509-cert` (`CONCORDIUM_NODE_GRPC2_X509_CERT`) which should point to
  the path containing a x509 certificate for the server. If this is set so must
  be the following option and if both are enabled then TLS is enabled on the
  grpc server.
- `--grpc2-cert-private-key` (`CONCORDIUM_NODE_GRPC2_CERT_PRIVATE_KEY`) path to
  the file containing the private key corresponding to the certificate provided
  in the preceding option. Both the certificate and the private key should be
  PEM encoded.
- `--grpc2-enable-grpc-web` (`CONCORDIUM_NODE_GRPC2_ENABLE_GRPC_WEB`) if set,
  enables the server support for [grpc-web](https://github.com/grpc/grpc-web)
  over HTTP 1.1. This allows the node's API to be used directly from a browser.
- `--grpc2-health-max-finalized-delay` (default is 5min)
  (`CONCORDIUM_NODE_GRPC2_HEALTH_MAX_FINALIZED_DELAY`) is a configuration for the
  `GetNodeHealth` endpoint. It specifies (in seconds) the maximum delay in last
  finalized block time before the health check fails.
- `--grpc2-health-min-peers`
  (`CONCORDIUM_NODE_GRPC2_HEALTH_MIN_PEERS`) is a configuration for the
  `GetNodeHealth` endpoint. It specifies the minimum number of peers the node
  should have for it to be considered healthy. If this is not set then the
  number of peers does not affect the health response.
- `--grpc2-endpoint-config` (`CONCORDIUM_NODE_GRPC2_ENDPOINT_CONFIG`) if
  supplied, it should point to a `.toml` file with the configuration of
  endpoints. If this option is not supplied all endpoints are enabled. If it is
  supplied then only the endpoints explicitly enabled in the configuration are
  available.

  The format of the file is a simple key-value list, with values being booleans.
  Keys are names of endpoints in snake_case. For example the following configuration file
  would enable all available endpoints except the ones flagged with `false` i.e. `get_account_info`,
  `shutdown`, `dump_start` and `dump_end`.

  ```toml
  get_finalized_blocks = true
  get_blocks = true
  get_account_list = true
  get_account_info = false
  get_token_info = true
  get_token_list = true
  get_module_list = true
  get_module_source = true
  get_instance_list = true
  get_instance_info = true
  get_instance_state = true
  instance_state_lookup = true
  get_next_account_sequence_number = true
  get_consensus_info = true
  get_consensus_detailed_status = true
  get_ancestors = true
  get_block_item_status = true
  invoke_instance = true
  get_cryptographic_parameters = true
  get_block_info = true
  get_baker_list = true
  get_pool_info = true
  get_passive_delegation_info = true
  get_blocks_at_height = true
  get_tokenomics_info = true
  get_pool_delegators = true
  get_pool_delegators_reward_period = true
  get_passive_delegators = true
  get_passive_delegators_reward_period = true
  get_branches = true
  get_election_info = true
  get_identity_providers = true
  get_anonymity_revokers = true
  get_account_non_finalized_transactions = true
  get_block_transaction_events = true
  get_block_special_events = true
  get_block_pending_updates = true
  get_next_update_sequence_numbers = true
  get_scheduled_release_accounts = true
  get_cooldown_accounts = true
  get_pre_cooldown_accounts = true
  get_pre_pre_cooldown_accounts = true
  get_block_chain_parameters = true
  get_block_finalization_summary = true
  get_baker_earliest_win_time = true
  shutdown = false
  peer_connect = true
  peer_disconnect = true
  get_banned_peers = true
  ban_peer = true
  unban_peer = true
  dump_start = false
  dump_stop = false
  get_peers_info = true
  get_node_info = true
  send_block_item = true
  get_account_transaction_sign_hash = true
  get_block_items = true
  get_bakers_reward_period = true
  get_block_certificates = true
  get_baker_earliest_win_time = true
  get_first_block_epoch = true
  get_winning_bakers_epoch = true
  dry_run = true
  ```

### Configuration options for checking client liveness

The following configuration options for the GRPC2 server can be used to ensure
connected clients are alive and responding in desired time.

- `--grpc2-keep-alive-interval` (`CONCORDIUM_NODE_GRPC2_KEEPALIVE_INTERVAL`)
  Enable HTTP2 keepalive, and set the interval (in seconds) at which Ping frames are sent.

- `--grpc2-keep-alive-timeout` (`CONCORDIUM_NODE_GRPC2_KEEPALIVE_TIMEOUT`)
  Timeout (in seconds) for responses to HTTP2 Ping frames. If the client does not respond in time then the
  connection will be closed. This has no effect unless `grpc2-keep-alive-interval` is set.

- `--grpc2-tcp-keep-alive-interval` (`CONCORDIUM_NODE_GRPC2_TCP_KEEPALIVE`)
  The interval (in seconds) at which TCP keepalive probes are sent.

### Configuration options for limiting resource usage

- `--grpc2-invoke-max-energy` (`CONCORDIUM_NODE_GRPC2_INVOKE_MAX_ENERGY`)
  Maximum amount of energy allowed for a call to the InvokeInstance (and also
  InvokeContract of the V1 API) endpoint. If the caller supplies the maximum
  energy as part of the call then the energy used for the execution of
  the call is the **minimum** of `CONCORDIUM_NODE_GRPC2_INVOKE_MAX_ENERGY` and
  the caller supplied energy.

- `--grpc2-max-concurrent-requests` (`CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_REQUESTS`)
  Global (i.e., not per connection) number of concurrent requests allowed.
  Defaults to 100. Note that when a request has a streaming response, the
  request counts as completed once the stream is constructed.

- `--grpc2-max-concurrent-requests-per-connection` (`CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_REQUESTS_PER_CONNECTION`)
  Maximum number of concurrent requests **per connection**. Defaults to 10.

- `--grpc2-max-concurrent-streams` (`CONCORDIUM_NODE_GRPC2_MAX_CONCURRENT_STREAMS`)
  Maximum number of concurrently open streams **per connection**. For requests with streaming
  responses this limits the amount of unconsumed streams that the node will maintain.

- `--grpc2-max-connections` (`CONCORDIUM_NODE_GRPC2_MAX_CONNECTIONS`)
  Maximum number of **connections** that the GRPC server will allow at any given
  time. Defaults to 500. This value should be set low enough so that the node
  process will never run out of file descriptors for consensus and P2P
  connection handling. This value should be set so that together with
  `hard-connection-limit` and `grpc2-max-concurrent-requests` it still leaves at
  least 50 open file descriptors for consensus.

- `--grpc2-request-timeout` (`CONCORDIUM_NODE_GRPC2_REQUEST_TIMEOUT`)
  Maximum amount of time to allow for processing a request (in seconds). Defaults
  to 30s. Note that as for `grpc2-max-concurrent-requests`, for streaming
  responses, this does not mean that the stream must be consumed in 30s. It only
  means that the time until the initial response must be less than 30s.

- `--grpc2-invoke-max-energy` (`CONCORDIUM_NODE_GRPC2_INVOKE_MAX_ENERGY`)
  Maximum amount of energy allowed for a call to InvokeInstance/InvokeContract
  or a dry-run session. Defaults to 1000000. For a dry-run session, each
  request in the session uses a certain amount of energy, and once the limit is
  reached, the session will be terminated with a `RESOURCE_EXHAUSTED` status.

- `--grpc2-dry-run-timeout` (`CONCORDIUM_NODE_GRPC2_DRY_RUN_TIMEOUT`)
  Maximum duration in milliseconds for a dry-run session to complete. Defaults
  to 30000 (30s). If the timeout is reached before the session completes, it
  will be terminated with a `DEADLINE_EXCEEDED` status.

- `--grpc2-dry-run-concurrency` (`CONCORDIUM_NODE_GRPC2_DRY_RUN_CONCURRENCY`)
  Maximum number of concurrent invocations of the `DryRun` endpoint. There is no
  limit by default. If this limit is reached, the node will respond to further
  `DryRun` requests with `RESOURCE_EXHAUSTED` until existing invocations complete.