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
- `--grpc2-health-max-finalized-delay`
  (`CONCORDIUM_NODE_GRPC2_HEALTH_MAX_FINALIZED_DELAY`) is a configuration for the
  `GetNodeHealth` endpoint. It specifies (in seconds) the maximum delay in last
  finalized block time before the health check fails.
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
  get_module_list = true
  get_module_source = true
  get_instance_list = true
  get_instance_info = true
  get_instance_state = true
  instance_state_lookup = true
  get_next_account_sequence_number = true
  get_consensus_info = true
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
  get_block_chain_parameters = true
  get_block_finalization_summary = true
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
  ```
