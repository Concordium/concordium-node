#!/bin/bash

# Create dirs
mkdir -p $CONFIG_DIR
mkdir -p $DATA_DIR
mkdir -p $DATA_DIR/tps_test

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

if [ "$TPS_RECV" == "true" ]; then
	echo "Receiver!"
    cd $DATA_DIR
    /build-project/target/debug/p2p_client-cli --id 9000000000000000000000000000000000000000000000000000000000000000 --enable-tps-test-recv --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --external-ip 10.96.0.15 $EXTRA_ARGS
elif [ "$TPS_SEND" == "true" ]; then
	echo "Sender!\n"
    echo "Generating data\n"
    cp ./gen_data.sh $DATA_DIR/tps_test/
    cd $DATA_DIR/tps_test/
    ./gen_data.sh
    cd $DATA_DIR

    /build-project/target/debug/p2p_client-cli --id 9000000000000000000000000000000000000000000000000000000000000001 --tps-test-recv-id 9000000000000000000000000000000000000000000000000000000000000000 --tps-test-data-dir $DATA_DIR/tps_test --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --connect-to 10.96.0.15 --external-ip 10.96.0.16 $EXTRA_ARGS
fi




