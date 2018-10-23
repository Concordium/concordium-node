#!/bin/bash

# Create dirs
mkdir -p $CONFIG_DIR
mkdir -p $DATA_DIR

cd $DATA_DIR

heaptrack /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --desired-nodes $DESIRED_PEERS --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE $EXTRA_ARGS

