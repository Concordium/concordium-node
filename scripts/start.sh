#!/usr/bin/env bash

# SIGTERM handler
_term() {
  kill -TERM ${CHILD_PID} 2>/dev/null
  wait ${CHILD_PID}
}

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib

ARGS=""

# Set overrides for configuration and data store paths
if [ -n "$CONFIG_DIR" ];
then
    ARGS="$ARGS --config-dir $CONFIG_DIR"
    mkdir -p $CONFIG_DIR
fi

if [ -n "$DATA_DIR" ];
then
    ARGS="$ARGS --data-dir $DATA_DIR"
    mkdir -p $DATA_DIR
    cd $DATA_DIR
fi

# Unwrap proper genesis bundle, and swap to one of the benchmarks if set
if [ -n "$NUM_BAKERS" ];
then
    if [ -n "$DATA_DIR" ];
    then
        if [ -n "$FINBENCH_NUM" ]; 
        then
            cd /genesis-data
            tar -xzf finbench-bakers.tar.gz
            cd genesis_data/
            cp * $DATA_DIR/
            cd $DATA_DIR
            cp "genesis-finbench-${FINBENCH_NUM}.dat" genesis.dat
        elif [ -n "$TPS_NUM" ];
        then
            cd /genesis-data
            tar -xzf tps-bakers.tar.gz
            cd genesis_data/
            cp * $DATA_DIR/
            cd $DATA_DIR
            cp "genesis-tps-${TPS_NUM}.dat" genesis.dat
        elif [ -n "$CATCHUP_NUM" ];
        then
            cd /genesis-data
            tar -xzf catchup-bakers.tar.gz
            cd genesis_data/
            cp * $DATA_DIR/
            cd $DATA_DIR
            cp "genesis-catchup-${CATCHUP_NUM}.dat" genesis.dat
        else
            cd /genesis-data
            tar -xvf $NUM_BAKERS-bakers.tar.gz
            cd genesis_data/
            cp * $DATA_DIR/
            cd $DATA_DIR
        fi
    fi
fi

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

# If BAKER_CREDENTIALS_FILENAME is provided, get that one.
# Otherwise we assume we are given a baker number, from which we construct the credential
# file `baker-n-credentials.json`. This is used for testing where we spawn a number of
# bakers at the same time. The id's are provided by the baker_id_gen tool.
if [ -n "$BAKER_CREDENTIALS_FILENAME" ];
then
    BAKER_CREDENTIALS_FILE="${DATA_DIR}/${BAKER_CREDENTIALS_FILENAME}"
    if [ -f $BAKER_CREDENTIALS_FILE ];
    then
        ARGS="$ARGS --baker-credentials-file $BAKER_CREDENTIALS_FILE"
    fi
elif [ -n "$BAKER_ID" ];
then
    REAL_BAKER_ID=$(echo $BAKER_ID | cut -d'-' -f2)
    BAKER_CREDENTIALS_FILE="${DATA_DIR}/baker-${REAL_BAKER_ID}-credentials.json"
    if [ -f $BAKER_CREDENTIALS_FILE ];
    then
        ARGS="$ARGS --baker-credentials-file $BAKER_CREDENTIALS_FILE"
    fi

    if [ -n "$LOGGING_SPLIT_HALF_TRACE_HALF_INFO" ]; then
        if [ $(($REAL_BAKER_ID % 2 )) == 0 ]; then
            ARGS="$ARGS --trace"
        else
            ARGS="$ARGS --info"
        fi
    fi
    if [ -n "$ARGS_SPLIT_HALF_AND_HALF" ]; then
        if [[ $(($REAL_BAKER_ID % 2 )) == 0 && -n "$ARGS_SPLIT_HALF_AND_HALF_ARG_ONE" ]]; then
            ARGS="$ARGS $ARGS_SPLIT_HALF_AND_HALF_ARG_ONE"
        elif [ -n "$ARGS_SPLIT_HALF_AND_HALF_ARG_TWO" ]; then
            ARGS="$ARGS $ARGS_SPLIT_HALF_AND_HALF_ARG_TWO"
        fi
    fi
    if [[ -n "$TRANSACTION_OUTCOME_LOGGING" && "$REAL_BAKER_ID" == "0" ]];
    then
        ARGS="$ARGS --transaction-outcome-logging"
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_NAME" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-name $TRANSACTION_OUTCOME_LOGGING_NAME"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_HOST" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-host $TRANSACTION_OUTCOME_LOGGING_HOST"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_PORT" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-port $TRANSACTION_OUTCOME_LOGGING_PORT"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_USERNAME" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-username $TRANSACTION_OUTCOME_LOGGING_USERNAME"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_PASSWORD" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-password $TRANSACTION_OUTCOME_LOGGING_PASSWORD"
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

if [ -n "$BOOTSTRAPPER_WAIT_UNTIL_MINIMUM_NODES" ];
then
    ARGS="$ARGS --max-nodes $MAX_NODES"
fi

if [ -n "$LISTEN_HTTP_PORT" ];
then
    ARGS="$ARGS --listen-http-port $LISTEN_HTTP_PORT"
fi

if [ -n "$MAX_ALLOWED_NODES" ];
then
    ARGS="$ARGS --max-allowed-nodes $MAX_ALLOWED_NODES"
fi

if [ -n "$MAX_ALLOWED_NODES_PERCENTAGE" ];
then
    ARGS="$ARGS --max-allowed-nodes-percentage $MAX_NODES_PERCENTAGE"
fi

if [ -n "$THREAD_POOL_SIZE" ];
then
    ARGS="$ARGS --thread-pool-size $THREAD_POOL_SIZE"
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

if [ -n "$MAX_LATENCY" ];
then
    ARGS="$ARGS --max-latency $MAX_LATENCY"
fi

if [ -n "$HARD_CONNECTION_LIMIT" ];
then
    ARGS="$ARGS --hard-connection-limit $HARD_CONNECTION_LIMIT"
fi

if [ -n "$COLLECTOR_INTERVAL" ];
then
    ARGS="$ARGS --collect-interval $COLLECTOR_INTERVAL"
fi

if [ -n "$COLLECTOR_URL" ];
then
    ARGS="$ARGS --collector-url $COLLECTOR_URL"
fi

if [ -n "$COLLECTOR_NODE_NAME" ];
then
    ARGS="$ARGS --node-name $COLLECTOR_NODE_NAME"
fi

if [ -n "$COLLECTOR_GRPC_HOST" ];
then
    ARGS="$ARGS --grpc-host $COLLECTOR_GRPC_HOST"
fi

if [ -n "$COLLECTOR_ARTIFICIAL_START_DELAY" ];
then
    ARGS="$ARGS --artificial-start-delay $COLLECTOR_ARTIFICIAL_START_DELAY"
fi

if [ -n "$COLLECTOR_MAX_GRPC_FAILURES_ALLOWED" ];
then
    ARGS="$ARGS --max-grpc-failures-allowed $COLLECTOR_MAX_GRPC_FAILURES_ALLOWED"
fi

if [ -n "$RPC_PASSWORD" ];
then
    ARGS="$ARGS --rpc-server-token $RPC_PASSWORD"
fi

if [ -n "$STAGING_NET_TOKEN" ];
then
    ARGS="$ARGS --staging-net-token $STAGING_NET_TOKEN"
fi

if [ -n "$DISTRIBUTION_CLIENT" ];
then
    cp /genesis.dat $DATA_DIR
fi

if [ -n "$COLLECTOR_BACKEND_PORT" ];
then
    ARGS="$ARGS --listen-port $COLLECTOR_BACKEND_PORT"
fi

if [ -n "$COLLECTOR_BACKEND_HOST" ];
then
    ARGS="$ARGS --listen-address $COLLECTOR_BACKEND_HOST"
fi

if [ -n "$COLLECTOR_BACKEND_STALE_TIME_ALLOWED" ];
then
    ARGS="$ARGS --stale-time-allowed $COLLECTOR_BACKEND_STALE_TIME_ALLOWED"
fi

if [ -n "$COLLECTOR_BACKEND_CLEANUP_INTERVAL" ];
then
    ARGS="$ARGS --cleanup-interval $COLLECTOR_BACKEND_CLEANUP_INTERVAL"
fi

if [ -n "$BUCKET_CLEANUP_INTERVAL" ];
then
    ARGS="$ARGS --bucket_cleanup_interval $BUCKET_CLEANUP_INTERVAL"
fi

if [ -n "$TIMEOUT_BUCKET_ENTRY_PERIOD" ];
then
    ARGS="$ARGS --timeout-bucket-entry-period $TIMEOUT_BUCKET_ENTRY_PERIOD"
fi

if [ -n "$BOOTSTRAPPER_TIMEOUT_BUCKET_ENTRY_PERIOD" ];
then
    ARGS="$ARGS --bootstrapper-timeout-bucket-entry-period $BOOTSTRAPPER_TIMEOUT_BUCKET_ENTRY_PERIOD"
fi

if [ -n "$NO_REBROADCAST_CONSENSUS_VALIDATION" ];
then
    ARGS="$ARGS --no-rebroadcast-consensus-validation"
fi

if [ -n "$BOOTSTRAP_SERVER" ];
then
    ARGS="$ARGS --bootstrap-server $BOOTSTRAP_SERVER"
fi

if [ -n "$IMPORT_BLOCKS_FROM" ];
then
    ARGS="$ARGS --import-blocks-from $IMPORT_BLOCKS_FROM"
fi

if [ "$MODE" == "tps_receiver" ]; then
    echo "Receiver!"
    /concordium-node \
    --enable-tps-test-recv \
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
    /concordium-node \
    --connect-to 10.96.0.15:8888 \
    $ARGS
elif [ "$MODE" == "basic" ]; then
    if [ -n "$ENABLE_TERM_HANDLER" ];
    then
        trap _term SIGTERM
        /concordium-node $ARGS &
        CHILD_PID=$!
        wait $CHILD_PID
    else
        /concordium-node $ARGS
    fi

    if [ -n "$DONT_CRASH" ];
    then
        while [ 1 ]
        do
            echo "I crashed!"
            sleep 5m
        done
    fi
elif [ "$MODE" == "bootstrapper" ]; then
    /p2p_bootstrapper-cli --regenesis-block-hashes-file $DATA_DIR/genesis_hash $ARGS
elif [ "$MODE" == "collector" ]; then
    /node-collector $ARGS
elif [ "$MODE" == "collector_backend" ]; then
    /node-collector-backend $ARGS
elif [ "$MODE" == "local_collector_backend" ]; then
    /node-collector-backend $ARGS
elif [ "$MODE" == "local_collector" ]; then
    if [ -n "$COLLECTOR_SLEEP" ];
    then
        echo "Sleeping for $COLLECTOR_SLEEP"
        sleep $COLLECTOR_SLEEP
    fi
    for i in `seq 1 $NUM_BAKERS`
    do
        COLLECTOR_NODE_URLS="$COLLECTOR_NODE_URLS --grpc-host http://docker-compose_baker_$i:10000 --node-name baker_$i"
    done
    ARGS="$ARGS $COLLECTOR_NODE_URLS"
    /node-collector $ARGS
elif [ "$MODE" == "local_basic" ]; then
    export BAKER_ID=`curl http://baker_id_gen:8000/next_id`
    echo "Using BAKER_ID $BAKER_ID"
    if [[ -n "$TRANSACTION_OUTCOME_LOGGING" && "$BAKER_ID" == "0" ]];
    then
        ARGS="$ARGS --transaction-outcome-logging"
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_NAME" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-name $TRANSACTION_OUTCOME_LOGGING_NAME"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_HOST" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-host $TRANSACTION_OUTCOME_LOGGING_HOST"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_PORT" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-port $TRANSACTION_OUTCOME_LOGGING_PORT"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_USERNAME" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-username $TRANSACTION_OUTCOME_LOGGING_USERNAME"
        fi
        if [ -n "$TRANSACTION_OUTCOME_LOGGING_PASSWORD" ]
        then
            ARGS="$ARGS --transaction-outcome-logging-database-password $TRANSACTION_OUTCOME_LOGGING_PASSWORD"
        fi
        if [ -n "$DB_SLEEP" ];
        then
            echo "Sleeping for $DB_SLEEP"
            sleep $DB_SLEEP
        fi
    fi
    /concordium-node --baker-credentials-file "${DATA_DIR}/baker-${BAKER_ID}-credentials.json" --no-dnssec $ARGS --id $(printf "%016d\n" $BAKER_ID)
elif [ "$MODE" == "local_bootstrapper" ]; then
    export NODE_ID="0000000001000000"
    /p2p_bootstrapper-cli \
        --id $NODE_ID \
        --listen-port 8888 \
        --regenesis-block-hashes-file $DATA_DIR/genesis_hash $EXTRA_ARGS
elif [ "$MODE" == "local_wallet_proxy" ]; then
    if [ -n "$WALLET_PROXY_GRPC_IP" ];
    then
        ARGS="$ARGS --grpc-ip $WALLET_PROXY_GRPC_IP"
    fi
    if [ -n "$WALLET_PROXY_GRPC_PORT" ];
    then
        ARGS="$ARGS --grpc-port $WALLET_PROXY_GRPC_PORT"
    fi
    if [ -n "$WALLET_PROXY_DATABASE" ];
    then
        ARGS="$ARGS --db $WALLET_PROXY_DATABASE"
    fi
    if [ -n "$WALLET_PROXY_ACCOUNT_FILE" ];
    then
        ARGS="$ARGS --drop-account $WALLET_PROXY_ACCOUNT_FILE"
    else
        ARGS="$ARGS --drop-account /genesis-complementary-bundle/additional_accounts/gtu-drop-account-0.json"
    fi
    if [ -n "$WALLET_PROXY_IPS_METADATA_JSON" ]; 
    then
        ARGS="$ARGS --ip-data $WALLET_PROXY_IPS_METADATA_JSON"
    else
        ARGS="$ARGS --ip-data /genesis-complementary-bundle/identity-providers-with-metadata.json"
    fi
    if [ -n "$DB_SLEEP" ];
    then
        echo "Sleeping for $DB_SLEEP"
        sleep $DB_SLEEP
    fi
    eval "/wallet-proxy$ARGS"
else
    echo "No matching MODE was found. Please check!"
fi
