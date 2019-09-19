#!/usr/bin/env bash

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib

ARGS=""

# Determine what arguments to pass to the binary
if [ -n "$ID" ];
then
    ARGS="$ARGS --id $ID"
elif [ -n "$PERSISTENT_ID_BASED_ON_BAKER_ID" ];
then
    ID=$(printf "%016d\n" $(echo $BAKER_ID | cut -d'-' -f2))
    ARGS="$ARGS --id $ID"
elif [ -n "$PERSISTENT_BOOTSTRAPPER_ID_BASED_ON_NODE_ID" ];
then
    ID=$(printf "%016d\n" $(($(echo $(hostname) | cut -d'-' -f2)+1000000)))
    ARGS="$ARGS --id $ID"
fi

if [ -n "$LISTEN_PORT" ];
then
    ARGS="$ARGS --listen-port $LISTEN_PORT"
fi

if [ -n "$DESIRED_PEERS" ];
then
    ARGS="$ARGS --desired-nodes $DESIRED_PEERS"
fi

if [ -n "$BAKER_ID" ];
then
    REAL_BAKER_ID=$(echo $BAKER_ID | cut -d'-' -f2)
    ARGS="$ARGS --baker-id $REAL_BAKER_ID"
    if [[ -n "$ELASTIC_SEARCH_LOGGING" && "$REAL_BAKER_ID" == "0" ]];
    then
        ARGS="$ARGS --elastic-logging --scheduler-outcome-logging"
        if [ -n "$ELASTIC_SEARCH_URL" ]
        then
            ARGS="$ARGS --elastic-logging-url $ELASTIC_SEARCH_URL"
        fi
    fi
fi

if [ -n "$PROMETHEUS_METRICS_SERVER" ];
then
    ARGS="$ARGS --prometheus-server"
fi

if [ -n "$PROMETHEUS_METRICS_PORT" ];
then
    ARGS="$ARGS --prometheus-listen-port $PROMETHEUS_METRICS_PORT"
fi

if [ -n "$PROMETHEUS_METRICS_IP" ];
then
    ARGS="$ARGS --prometheus-listen-addr $PROMETHEUS_METRICS_IP"
fi

if [ -n "$CONFIG_DIR" ];
then
    ARGS="$ARGS --override-config-dir $CONFIG_DIR"
    mkdir -p $CONFIG_DIR
fi

if [ -n "$DATA_DIR" ];
then
    ARGS="$ARGS --override-data-dir $DATA_DIR"
    mkdir -p $DATA_DIR
    cd $DATA_DIR
fi

if [ -n "$NUM_BAKERS" ];
then
    if [ -n "$DATA_DIR" ];
    then
        cd /genesis-data
        tar -xvf $NUM_BAKERS-bakers.tar.gz
        cd genesis_data/
        cp * $DATA_DIR/
        cd $DATA_DIR
    fi
fi

if [ -n "$BOOTSTRAP_FIRST_NODE" ];
then
    ARGS="$ARGS --bootstrap-node $BOOTSTRAP_FIRST_NODE"
fi

if [ -n "$BOOTSTRAP_SECOND_NODE" ];
then
    ARGS="$ARGS --bootstrap-node $BOOTSTRAP_SECOND_NODE"
fi

if [ -n "$RPC_SERVER_ADDR" ];
then
    ARGS="$ARGS --rpc-server-addr $RPC_SERVER_ADDR"
fi

if [ -n "$TPS_MESSAGE_COUNT" ];
then
    ARGS="$ARGS --tps-message-count $TPS_MESSAGE_COUNT"
fi

if [ -n "$TPS_RECEIVER_ID" ];
then
    ARGS="$ARGS --tps-test-recv-id $TPS_RECEIVER_ID"
fi

if [ -n "$MAX_NODES" ];
then
    ARGS="$ARGS --max-nodes $MAX_NODES"
fi

if [ -n "$LISTEN_HTTP_PORT" ];
then
    ARGS="$ARGS --listen-http-port $LISTEN_HTTP_PORT"
fi

if [ -n "$MAX_ALLOWED_NODES" ];
then
    ARGS="$ARGS --max-allowed-nodes $MAX_NODES"
fi

if [ -n "$MAX_ALLOWED_NODES_PERCENTAGE" ];
then
    ARGS="$ARGS --max-allowed-nodes-percentage $MAX_NODES_PERCENTAGE"
fi

if [ -n "$EXTRA_ARGS" ];
then
    ARGS="$ARGS $EXTRA_ARGS"
fi

if [ -n "$ARTIFICIAL_DELAY" ];
then
    sleep $ARTIFICIAL_DELAY
fi

if [ -n "$SEEN_MESSAGE_IDS_SIZE" ];
then
    ARGS="$ARGS --gossip-seen-message-ids-size $SEEN_MESSAGE_IDS_SIZE"
fi

if [ -n "$MAX_RESEND_ATTEMPTS" ];
then
    ARGS="$ARGS --max-resend-attempts $MAX_RESEND_ATTEMPTS"
fi

if [ -n "$RELAY_BROADCAST_PERCENTAGE" ];
then
    ARGS="$ARGS --relay-broadcast-percentage $RELAY_BROADCAST_PERCENTAGE"
fi

if [ -n "$GLOBAL_STATE_CATCH_UP_REQUESTS" ];
then
    ARGS="$ARGS --global-state-catch-up-requests"
fi

if [ -n "$NOISE_CRYPTO_DH_ALGORITHM" ];
then
    ARGS="$NOISE_ARGS --dh-algorithm $NOISE_CRYPTO_DH_ALGORITHM"
fi

if [ -n "$CRYPTO_CIPHER_ALGORITHM" ];
then
    ARGS="$ARGS --cipher-algorithm $NOISE_CRYPTO_CIPHER_ALGORITHM"
fi

if [ -n "$NOISE_CRYPTO_HASH_ALGORITHM" ];
then
    ARGS="$ARGS --hash-algorithm $NOISE_CRYPTO_HASH_ALGORITHM"
fi

if [ -n "$PROFILING_ARGS" ];
then
    ARGS="$ARGS $PROFILING_ARGS"
fi

if [ -n "$EXTERNAL_IP" ];
then
    ARGS="$ARGS --external-ip $EXTERNAL_IP"
fi

if [ -n "$EXTERNAL_PORT" ];
then
    ARGS="$ARGS --external-port $EXTERNAL_PORT"
fi

if [ -n "$NOISE_CRYPTO_HASH_ALGORITHM" ];
then
    ARGS="$ARGS --hash-algorithm $NOISE_CRYPTO_HASH_ALGORITHM"
fi

if [ -n "$BOOTSTRAPPER_WAIT_UNTIL_MINIMUM_NODES" ];
then
    ARGS="$ARGS --wait-until-minimum-nodes $BOOTSTRAPPER_WAIT_UNTIL_MINIMUM_NODES"
fi

if [ "$MODE" == "tps_receiver" ]; then
    echo "Receiver!"
    /p2p_client-cli \
    --enable-tps-test-recv \
    --external-ip 10.96.0.15 \
    $ARGS
elif [ "$MODE" == "tps_sender" ]; then
    echo "Sender!\n"
    mkdir -p $DATA_DIR/tps_test
    echo "Generating data\n"
    cd $DATA_DIR/tps_test
    for i in `seq 0 $(($TPS_MESSAGE_COUNT - 1))`;
    do
        echo $i
        dd if=/dev/urandom of=test-$i bs=1 count=1024 > /dev/null 2>&1
    done
    # Echo to cron file
    /p2p_client-cli \
    --connect-to 10.96.0.15:8888 \
    --external-ip 10.96.0.16 \
    $ARGS
elif [ "$MODE" == "basic" ]; then
    /p2p_client-cli $ARGS
    if [ -n "$DONT_CRASH" ];
    then
        while [ 1 ]
        do
            echo "I crashed!"
            sleep 5m
        done
    fi
elif [ "$MODE" == "bootstrapper" ]; then
    /p2p_bootstrapper-cli $ARGS
elif [ "$MODE" == "local_basic" ]; then
    export BAKER_ID=`curl http://baker_id_gen:8000/next_id`
    echo "Using BAKER_ID $BAKER_ID"
    if [[ -n "$ELASTIC_SEARCH_LOGGING" && "$BAKER_ID" == "0" ]];
    then
        ARGS="$ARGS --elastic-logging --scheduler-outcome-logging --elastic-logging-url http://elasticsearch:9200"
        echo "Sleeping 20s for ES to warmup"
        sleep 20
    fi
    /p2p_client-cli --baker-id $BAKER_ID --no-dnssec $ARGS --id $(printf "%016d\n" $BAKER_ID)
elif [ "$MODE" == "local_bootstrapper" ]; then
    export NODE_ID="0000000001000000"
    /p2p_bootstrapper-cli \
        --id $NODE_ID \
        --listen-port 8888 \
        $EXTRA_ARGS
else
    echo "No matching MODE was found. Please check!"
fi
