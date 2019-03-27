#!/bin/bash

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/rts

if [ "$MODE" == "tps_receiver" ]; then
    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR
	
    echo "Receiver!"
    
    cd $DATA_DIR
    
    /build-project/target/debug/p2p_client-cli \
    --id 9000000000000000000000000000000000000000000000000000000000000000 \
    --enable-tps-test-recv \
    --listen-port $LISTEN_PORT \
    --num-bakers $NUM_BAKERS \
    --baker-id 0 \
    --prometheus-server $PROMETHEUS_METRICS_SERVER \
    --prometheus-listen-port $PROMETHEUS_METRICS_PORT \
    --prometheus-listen-addr $PROMETHEUS_METRICS_IP \
    --override-config-dir $CONFIG_DIR \
    --override-data-dir $DATA_DIR \
    --external-ip 10.96.0.15 \
    --tps-message-count $TPS_MESSAGE_COUNT \
    --desired-nodes $DESIRED_PEERS \
    $EXTRA_ARGS

elif [ "$MODE" == "tps_sender" ]; then
	echo "Sender!\n"
    
    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR
    mkdir -p $DATA_DIR/tps_test

    echo "Generating data\n"
    cd $DATA_DIR/tps_test

    for i in `seq 0 $(($TPS_MESSAGE_COUNT - 1))`;
    do
	    echo $i
	    dd if=/dev/urandom of=test-$i bs=1 count=1024 > /dev/null 2>&1
    done

    cd $DATA_DIR

    # Echo to cron file

    /build-project/target/debug/p2p_client-cli \
    --id 9000000000000000000000000000000000000000000000000000000000000001 \
    --tps-test-recv-id 9000000000000000000000000000000000000000000000000000000000000000 \
    --tps-test-data-dir $DATA_DIR/tps_test \
    --listen-port $LISTEN_PORT \
    --num-bakers $NUM_BAKERS \
    --baker-id 1 \
    --prometheus-server $PROMETHEUS_METRICS_SERVER \
    --prometheus-listen-port $PROMETHEUS_METRICS_PORT \
    --prometheus-listen-addr $PROMETHEUS_METRICS_IP \
    --override-config-dir $CONFIG_DIR \
    --override-data-dir $DATA_DIR \
    --connect-to 10.96.0.15:8888 \
    --external-ip 10.96.0.16 \
    --tps-message-count $TPS_MESSAGE_COUNT \
    --desired-nodes $DESIRED_PEERS \
    $EXTRA_ARGS
    # cron -f
elif [ "$MODE" == "basic" ]; then
# Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR
    
    /build-project/target/debug/p2p_client-cli --listen-port $LISTEN_PORT --desired-nodes $DESIRED_PEERS --num-bakers $NUM_BAKERS --baker-id $(echo $BAKER_ID | cut -d'-' -f2) --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE --rpc-server-addr $RPC_SERVER_ADDR $EXTRA_ARGS

elif [ "$MODE" == "bootstrapper" ]; then

    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR

    /build-project/target/debug/p2p_bootstrapper-cli --listen-port $LISTEN_PORT --external-ip $EIP --external-port $EXTERNAL_PORT --id $NODE_ID --max-nodes $MAX_NODES --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP --override-config-dir $CONFIG_DIR --override-data-dir $DATA_DIR $EXTRA_ARGS

elif [ "$MODE" == "testrunner" ]; then

    # Create dirs
    mkdir -p $CONFIG_DIR
    mkdir -p $DATA_DIR
    
    /build-project/target/debug/testrunner --listen-port $LISTEN_PORT --listen-http-port $LISTEN_HTTP_PORT --bootstrap-node $BOOTSTRAP_FIRST_NODE --bootstrap-node $BOOTSTRAP_SECOND_NODE $EXTRA_ARGS 

elif [ "$MODE" == "local_basic" ]; then
    export BAKER_ID=`curl http://baker_id_gen:8000/next_id`
    echo "Using BAKER_ID $BAKER_ID"


    /build-project/p2p-client/target/debug/p2p_client-cli \
        --private-node \
        --no-dnssec \
        --testrunner-url http://testrunner:8950 \
        --desired-nodes $DESIRED_PEERS \
        --external-port $EXTERNAL_PORT \
        --bootstrap-node $BOOTSTRAP_NODE \
        --baker-id $BAKER_ID \
        --num-bakers $NUM_BAKERS \
        --rpc-server-addr 0.0.0.0

elif [ "$MODE" == "local_bootstrapper" ]; then
    export NODE_ID=`awk 'END{ print $1}' /etc/hosts | sha256sum | awk '{print $1}'`
    heaptrack /build-project/p2p-client/target/debug/p2p_bootstrapper-cli \
        --id $NODE_ID \
        --listen-port 8888 \
        --private-node

elif [ "$MODE" == "local_testrunner" ]; then
    /build-project/p2p-client/target/debug/testrunner \
        --private-node \
        --no-dnssec \
        --desired-nodes $DESIRED_PEERS \
        --external-port $EXTERNAL_PORT \
        --bootstrap-node $BOOTSTRAP_NODE 
fi
