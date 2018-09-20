#!/bin/bash

# Create dirs
mkdir -p $CONFIG_DIR
mkdir -p $DATA_DIR

./target/release/p2p_bootstrapper-cli --listen-port $LISTEN_PORT --external-ip $EIP --external-port $EXTERNAL_PORT --id $NODE_ID --max-nodes $MAX_NODES --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR $EXTRA_ARGS
