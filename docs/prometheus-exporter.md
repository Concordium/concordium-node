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

### `network_inbound_high_priority_message_drops_total`

Total inbound high priority messages dropped due to the queue for high priority messages being full.
See `network_inbound_high_priority_message_queue_size` for the current size of the queue.

### `network_inbound_low_priority_message_drops_total`

Total inbound low priority messages dropped due to the queue for low priority messages being full.
See `network_inbound_low_priority_message_queue_size` for the current size of the queue.

### `network_inbound_high_priority_messages_total`

Total inbound high priority messages received. This is incremented when a consensus message is enqueue in the high priority queue. Note: Does not include the dropped messages, see `network_inbound_high_priority_message_drops_total`.

### `network_inbound_low_priority_messages_total`

Total inbound low priority messages received. This is incremented when a consensus message is enqueue in the low priority queue. Note: Does not include the dropped messages, see `network_inbound_low_priority_message_drops_total`.

### `network_inbound_high_priority_message_queue_size`

Current number of consensus messages in the inbound high priority queue. Start dropping messages when larger than 16 * 1024.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_inbound_low_priority_message_queue_size`

Current number of consensus messages in the inbound low priority queue. Start dropping messages when larger than 32 * 1024.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_outbound_high_priority_message_queue_size`

Current number of consensus messages in the outbound high priority queue. Start dropping messages when larger than 8 * 1024.

The value of this metric should average around 0. There can be spikes, but generally large numbers over extended periods mean the node is struggling to keep up.

### `network_outbound_low_priority_message_queue_size`

Current number of consensus messages in the outbound low priority queue. Start dropping messages when larger than 16 * 1024.

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
