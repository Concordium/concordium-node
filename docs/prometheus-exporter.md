# Prometheus exporter

This document describes how to configure `concordium-node` to run a [Prometheus exporter](https://prometheus.io/) and documentation for what each of the exported metrics mean.

## Enable the exporter

Concordium node does not run the Prometheus exporter by default. To enable it at node startup, the listening port for the exporter must be provided either through a command line argument `--prometheus-listen-port` or the environment variable `CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT`.

Likewise, the IP address to listen on can be specified using command line argument `--prometheus-listen-addr` or environment variable `CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESS`. Defaults to `127.0.0.1` when not provided.

To verify whether the exporter is running, then open the provided listen address and port in a browser. Which should display the text "Operational".

## Push metrics to a Pushgateway

Concordium node also supports pushing metrics to a Prometheus Pushgateway. This is enabled by providing the URL for the Pushgateway using either the command line argument `--prometheus-push-gateway` or by setting the environment variable `CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY`.

The following options are available for configuring the pushing of metrics:

- `--prometheus-instance-name` (`CONCORDIUM_NODE_PROMETHEUS_INSTANCE_NAME`)
  Set the instance label to include in metrics pushed to Pushgateway. If not present the id of the node is used.
- `--prometheus-job-name` (`CONCORDIUM_NODE_PROMETHEUS_JOB_NAME`)
  Set the instance label to include in metrics pushed to Pushgateway. If not present, `concordium_node_push` is used.
- `--prometheus-push-gateway-interval` (`CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_INTERVAL`)
  Duration in seconds between pushes to Pushgateway. If not present `2` is used.
- `--prometheus-push-gateway-password` (`CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_PASSWORD`)
  Password to use for push gateway, both username or password must be provided to enable authentication.
- `--prometheus-push-gateway-username` (`CONCORDIUM_NODE_PROMETHEUS_PUSH_GATEWAY_USERNAME`)
  Username to use for push gateway, both username or password must be provided to enable authentication.

## Metrics

All of the following accumulated metrics are relative to the startup of the node, in other words, restarting the node resets the metrics.

### `network_received_bytes`

Total number of bytes received over the network. Only network message received from connected peers are accounted.

### `network_sent_bytes`

Total number of bytes sent over the network. Only network message sent to connected peers are accounted.

### `network_packets_received_total`

Total number of network packets received from peers. This is accounted before the any form of deduplication.

### `network_packets_sent_total`

Total number of network packets sent to peers.

### `network_connected_peers`

Current number of connected peers. This is incremented when a peer completes a handshake and decremented again when the connection is dropped.

###  `network_connections_received_total`

Total number of connections received. Incremented everytime someone tries to establish a new connection, meaning even the failed connections are accounted, such as when the address is banned, duplicate connection or the node is at its limit on number of connections.

### `network_inbound_high_priority_message_queue_size`

Current number of consensus messages in the inbound high priority queue. Start dropping messages when larger than 16 * 1024.

High priority messages are blocks, finalization messages and catch-up status messages.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_inbound_low_priority_message_queue_size`

Current number of consensus messages in the inbound low priority queue. Start dropping messages when larger than 32 * 1024.

Low priority messages are transaction messages.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_outbound_high_priority_message_queue_size`

Current number of consensus messages in the outbound high priority queue. Start dropping messages when larger than 8 * 1024.

High priority messages are blocks, finalization messages and catch-up status messages.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_outbound_low_priority_message_queue_size`

Current number of consensus messages in the outbound low priority queue. Start dropping messages when larger than 16 * 1024.

Low priority messages are transaction messages.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `consensus_last_finalized_block_height`

The block height of the last finalized block.

### `consensus_last_finalized_block_timestamp`

Timestamp for processing the last finalized block (as Unix time in milliseconds).

Note that this is the time the node has last finalized a block. It is **not** the timestamp of when the block was produced, or the slot time of the block.

### `consensus_last_arrived_block_height`

The block height of the last arrived block.

### `consensus_last_arrived_block_timestamp`

Timestamp for the processed last arrived block (as Unix time in milliseconds).

Note that this is the time when the node has last processed a new block. It is **not** the objective timestamp of the block (i.e., slot time).

### `consensus_received_messages_total`

Total number of consensus messages received. Labelled with message type (`message=<type>`) and the outcome (`result=<outcome>`).

Possible values of `message` are:
- `"block"`
- `"transaction"`
- `"finalization message"`
- `"catch-up status message"`

Possible values of `result` are:
- `"valid"` Successful outcome.
- `"invalid"` Messages being rejected as invalid.
- `"dropped"` Messages being dropped due to a full queue. See either `network_inbound_low_priority_message_queue_size` or `network_inbound_high_priority_message_queue_size` for more on this.
- `"duplicate"` Duplicate consensus messages. These are duplicate messages determined so by consensus, **after** the message has already been deduplicated at the network layer.

### `consensus_sent_messages_total`

Total number of consensus messages sent. Labelled with message type (`message=<type>`).

Possible values of `message` are:
- `"block"`
- `"transaction"`
- `"finalization message"`
- `"catch-up status message"`

### `network_soft_banned_peers`

Current number of soft banned peers. The node temporarily bans peers if they fail to follow the protocol.

### `network_soft_banned_peers_total`

The total number of soft banned peers since startup. The node temporarily bans peers if they fail to follow the protocol.

### `network_peers_total`

Total number of peers connected since startup.

### `node_info`

Information of the node software. Provides the node version using a label (`version=<version>`).
Always has the value 1.

### `node_startup_timestamp`

Timestamp of starting up the node (Unix time in milliseconds).

### `grpc_request_response_time_seconds`

Histogram tracking the total number of gRPC requests received and the time it took to provide a response. Labelled with the gRPC method name (`method=<name>`) and the gRPC response status (`status=<status>`).

The duration tracked is the time it takes for the handler to construct a response, which does not include the time it takes to stream the response to the client.
As a result for streaming gRPC methods, the duration represents the time it takes for the node to first respond, which is not nescessarily same as duration as for providing the first item in the stream.

The size of the buckets be configured using the `prometheus-grpc-response-time-buckets` (`CONCORDIUM_NODE_PROMETHEUS_GRPC_RESPONSE_TIME_BUCKETS`) and is provided as a list of decimal numbers separated by ",". Each value represents the upper inclusive bound of a bucket (in seconds) and a bucket with +Infinity is always added. The values must be sorted in strictly increasing order.
The default value of the configuration is `"0.050,0.100,0.200,0.500,1.000"`.

For a complete list of possible method names refer to the gRPC API documentation.

Possible values of `status` are:
- `"ok"` The operation completed successfully.
- `"cancelled"` The operation was cancelled.
- `"unknown"` Unknown error.
- `"invalid argument"` Client specified an invalid argument.
- `"deadline exceeded"` Deadline expired before operation could complete.
- `"not found"` Some requested entity was not found.
- `"already exists"` Some entity that we attempted to create already exists.
- `"permission denied"` The caller does not have permission to execute the specified operation.
- `"resource exhausted"` Some resource has been exhausted.
- `"failed precondition"` The system is not in a state required for the operation's execution.
- `"aborted"` The operation was aborted.
- `"out of range"` Operation was attempted past the valid range.
- `"unimplemented"` Operation is not implemented or not supported.
- `"internal"` Internal error.
- `"unavailable"` The service is currently unavailable.
- `"data loss"` Unrecoverable data loss or corruption.
- `"unauthenticated"` The request does not have valid authentication credentials.

### `grpc_in_flight_requests`

Current number of gRPC requests being handled by the node.
Streaming gRPC methods are counted as in flight until the response stream is closed.

### `consensus_baking_committee`

The baking committee status of the node for the current best block.

The value is mapped to a status:
- `0` The node is in the baking committee and is actively baking.
- `1` The node is not in the baking committee.
- `2` The node is added to the upcoming the baking committee, and is not baking actively yet.
- `3` The node is setup with baker keys not matching the keys currently registered in the baking committee.

### `consensus_finalization_committee`

The finalization committee status of the node for the current finalization round. The metric will have a value of 1 if and only if the node is a member of the finalization committee.

### `consensus_baking_lottery_power`

Baking lottery power for the current epoch of the best block.
The value is a number between 0 and 1, and is representing the fraction of baking stake (combined stake of the baker and delegators) to the total baking stake of the baking committee.
Is only non-zero when active member of the baking committee.

### `consensus_non_finalized_transactions`

The current number of non-finalized transactions stored by the node.


### `consensus_baked_blocks_total`

Total number of blocks baked by the node since startup.

Blocks received as part of catchup are also counted, when the block was baked by the same baker ID as the node is configured with.

### `consensus_finalized_baked_blocks_total`

Total number of finalized blocks baked by the node since startup.

Finalized blocks received as part of catchup are also counted, when the block was baked by the same baker ID as the node is configured with.

### `consensus_unsupported_pending_protocol_version`

Indicator for unsupported pending protocol updates where a non-zero value indicates the effective time (Unix time in milliseconds) of a pending unsupported protocol update.

This metric is intended for setting up alerts to catch outdated nodes.

### `peer_bucket_size`

The number of recently connected peers used to generate the peer list included in handshake responses of the bootstrapper. Labelled by the number of the bucket in which the peer is maintained (`bucket=<number>`).