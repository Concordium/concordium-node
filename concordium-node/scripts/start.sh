#!/bin/bash

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts

if [ "$MODE" == "tps_receiver" ]; then
	echo "Receiver!"
    cd $DATA_DIR
    /build-project/target/debug/p2p_client-cli --id 9000000000000000000000000000000000000000000000000000000000000000 --enable-tps-test-recv --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --external-ip 10.96.0.15 $EXTRA_ARGS
elif [ "$MODE" == "tps_sender" ]; then
	echo "Sender!\n"
    echo "Generating data\n"
    cp ./gen_data.sh $DATA_DIR/tps_test/
    cd $DATA_DIR/tps_test/
    ./gen_data.sh
    cd $DATA_DIR

    # Echo to cron file
    #/build-project/target/debug/p2p_client-cli --id 9000000000000000000000000000000000000000000000000000000000000001 --tps-test-recv-id 9000000000000000000000000000000000000000000000000000000000000000 --tps-test-data-dir $DATA_DIR/tps_test --listen-port $LISTEN_PORT --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --connect-to 10.96.0.15 --external-ip 10.96.0.16 $EXTRA_ARGS
    cron -f
elif [ "$MODE" == "basic" ]; then
# Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR

    cd $DATA_DIR
    echo $BAKER_ID

    heaptrack /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --desired-nodes $DESIRED_PEERS --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE $EXTRA_ARGS

elif [ "$MODE" == "ipdiscovery" ]; then

    /build-project/target/debug/ip_discovery --listen-port $LISTEN_PORT --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP $EXTRA_ARGS

elif [ "$MODE" == "bootstrapper" ]; then

    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR

    cd $DATA_DIR

    heaptrack /build-project/target/debug/p2p_bootstrapper-cli --listen-port $LISTEN_PORT --external-ip $EIP --external-port $EXTERNAL_PORT --id $NODE_ID --max-nodes $MAX_NODES --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR $EXTRA_ARGS

elif [ "$MODE" == "testrunner" ]; then

    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR
    
    cd $DATA_DIR
    
    heaptrack /build-project/target/debug/testrunner --listen-port $LISTEN_PORT --listen-http-port $LISTEN_HTTP_PORT --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE $EXTRA_ARGS 

fi