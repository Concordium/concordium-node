#!/bin/bash

# Create dirs
mkdir -p $CONFIG_DIR
mkdir -p $DATA_DIR

cd $DATA_DIR
echo $BAKER_ID

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

if [ "$TPS_RECV" == "true" ]; then
	echo "Receiver!"
    /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR $EXTRA_ARGS
elif [ "$TPS_SEND" == "true" ]; then
	echo "Sender!"
    /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR $EXTRA_ARGS
fi




