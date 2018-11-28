#!/bin/bash

# Create dirs
mkdir -p $CONFIG_DIR
mkdir -p $DATA_DIR

cd $DATA_DIR

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

heaptrack /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --desired-nodes $DESIRED_PEERS --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE $EXTRA_ARGS

