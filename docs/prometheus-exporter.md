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

- `network_received_bytes` Total number of bytes received.
- `network_sent_bytes` Total number of bytes sent.
- `network_packets_received_total` Total number of network packets received.
- `network_packets_sent_total` Total number of network packets sent.
- `network_connected_peers` Current number of connected peers.
- `network_connections_received_total` Total number of connections received.
- `consensus_inbound_high_priority_message_drops_total` Total inbound high priority consensus messages dropped due to a full queue.
- `consensus_inbound_low_priority_message_drops_total` Total inbound low priority consensus messages dropped due to a full queue.
- `consensus_inbound_high_priority_messages_total` Total inbound high priority consensus messages received.
- `consensus_inbound_low_priority_messages_total` Total inbound low priority consensus messages received.
- `consensus_inbound_high_priority_message_queue_size` Current number of inbound high priority messages in queue.
- `consensus_inbound_low_priority_message_queue_size` Current number of inbound low priority messages in queue.
- `consensus_outbound_high_priority_message_queue_size` Current number of outbound high priority messages in queue.
- `consensus_outbound_low_priority_message_queue_size` Current number of outbound low priority messages in queue.
