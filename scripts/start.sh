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
if [ -n "$CONCORDIUM_NODE_CONFIG_DIR" ]; then
    mkdir -p $CONCORDIUM_NODE_CONFIG_DIR
fi

if [ -n "$CONCORDIUM_NODE_DATA_DIR" ]; then
    mkdir -p $CONCORDIUM_NODE_DATA_DIR
    cd $CONCORDIUM_NODE_DATA_DIR
fi

# FIXME: This is obsolete. We do not build these testing genesis setups at the moment.
# Unwrap proper genesis bundle, and swap to one of the benchmarks if set
if [ -n "$NUM_BAKERS" ]; then
    if [ -n "$DATA_DIR" ]; then
        if [ -n "$FINBENCH_NUM" ]; then
            cd /genesis-data
            tar -xzf finbench-bakers.tar.gz
            cd genesis_data/
            cp * $CONCORDIUM_NODE_DATA_DIR/
            cd $CONCORDIUM_NODE_DATA_DIR
            cp "genesis-finbench-${FINBENCH_NUM}.dat" genesis.dat
        elif [ -n "$TPS_NUM" ]; then
            cd /genesis-data
            tar -xzf tps-bakers.tar.gz
            cd genesis_data/
            cp * $CONCORDIUM_NODE_DATA_DIR/
            cd $CONCORDIUM_NODE_DATA_DIR
            cp "genesis-tps-${TPS_NUM}.dat" genesis.dat
        elif [ -n "$CATCHUP_NUM" ]; then
            cd /genesis-data
            tar -xzf catchup-bakers.tar.gz
            cd genesis_data/
            cp * $CONCORDIUM_NODE_DATA_DIR/
            cd $CONCORDIUM_NODE_DATA_DIR
            cp "genesis-catchup-${CATCHUP_NUM}.dat" genesis.dat
        else
            cp /genesis-data/genesis-$NUM_BAKERS-bakers/{genesis.dat,genesis_hash} $CONCORDIUM_NODE_DATA_DIR
        fi
    fi
fi


# Use CONCORDIUM_NODE_ID if present.
# Otherwise fallback to deduce CONCORDIUM_NODE_ID from baker id or hostname respectively.
if [ -n "$CONCORDIUM_NODE_PERSISTENT_ID_BASED_ON_BAKER_ID" && -z "$CONCORDIUM_NODE_ID"]; then
    export CONCORDIUM_NODE_ID=$(printf "%016d\n" $(echo $BAKER_ID | cut -d'-' -f2))
elif [ -n "$PERSISTENT_BOOTSTRAPPER_ID_BASED_ON_NODE_ID" && -z "$CONCORDIUM_NODE_ID"]; then
    export CONCORDIUM_NODE=$(printf "%016d\n" $(($(echo $(hostname) | cut -d'-' -f2)+1000000)))
fi


# Use CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE if present.
# Or if CONCORDIUM_NODE_BAKER_CREDENTIALS_FILENAME is provided, use this for deducing
# CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE together with CONCORDIUM_NODE_DATA_DIR. 
# Otherwise we assume we are given a baker number, from which we construct the credential
# file `baker-n-credentials.json`. This is used for testing where we spawn a number of
# bakers at the same time. The id's are provided by the baker_id_gen tool.
if [ -n "$CONCORDIUM_NODE_BAKER_CREDENTIALS_FILENAME" && -z "$CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE" ]; then
    BAKER_CREDENTIALS_FILE="${CONCORDIUM_NODE_DATA_DIR}/${CONCORDIUM_NODE_BAKER_CREDENTIALS_FILENAME}"
    if [ -f $BAKER_CREDENTIALS_FILE ]; then
        export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE=$BAKER_CREDENTIALS_FILE
    fi
elif [ -n "$CONCORDIUM_NODE_BAKER_ID" && -z "$CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE" ]; then
    REAL_BAKER_ID=$(echo $BAKER_ID | cut -d'-' -f2)
    BAKER_CREDENTIALS_FILE="${DATA_DIR}/baker-${REAL_BAKER_ID}-credentials.json"
    if [ -f $BAKER_CREDENTIALS_FILE ]; then
        export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE=$BAKER_CREDENTIALS_FILE
    fi

    if [ -n "$LOGGING_SPLIT_HALF_TRACE_HALF_INFO" ]; then
        if [ $(($REAL_BAKER_ID % 2 )) == 0 ]; then
            export CONCORDIUM_NODE_LOG_LEVEL_TRACE=1
        else
            export CONCORDIUM_NODE_LOG_LEVEL_INFO=1
        fi
    fi

    # hmm
    if [[ -n "$CONCORDIUM_NODE_TRANSACTION_OUTCOME_LOGGING" && "$REAL_BAKER_ID" == "0" ]]; then
        ARGS="$ARGS --transaction-outcome-logging"
        
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


# can't find reference
if [ -n "$TPS_MESSAGE_COUNT" ];
then
    ARGS="$ARGS --tps-message-count $TPS_MESSAGE_COUNT"
fi

# can't find reference
if [ -n "$TPS_RECEIVER_ID" ];
then
    ARGS="$ARGS --tps-test-recv-id $TPS_RECEIVER_ID"
fi

# can't find reference
if [ -n "$LISTEN_HTTP_PORT" ];
then
    ARGS="$ARGS --listen-http-port $LISTEN_HTTP_PORT"
fi

if [ -n "$EXTRA_ARGS" ];
then
    ARGS="$ARGS $EXTRA_ARGS"
fi

if [ -n "$ARTIFICIAL_DELAY" ];
then
    sleep $ARTIFICIAL_DELAY
fi

# can't find reference
if [ -n "$SEEN_MESSAGE_IDS_SIZE" ];
then
    ARGS="$ARGS --gossip-seen-message-ids-size $SEEN_MESSAGE_IDS_SIZE"
fi

# can't find reference
if [ -n "$MAX_RESEND_ATTEMPTS" ];
then
    ARGS="$ARGS --max-resend-attempts $MAX_RESEND_ATTEMPTS"
fi

# can't find reference
if [ -n "$RELAY_BROADCAST_PERCENTAGE" ];
then
    ARGS="$ARGS --relay-broadcast-percentage $RELAY_BROADCAST_PERCENTAGE"
fi

if [ -n "$GLOBAL_STATE_CATCH_UP_REQUESTS" ];
then
    ARGS="$ARGS --global-state-catch-up-requests"
fi

# can't find reference. 
if [ -n "$NOISE_CRYPTO_DH_ALGORITHM" ];
then
    ARGS="$NOISE_ARGS --dh-algorithm $NOISE_CRYPTO_DH_ALGORITHM"
fi

# can't find reference. 
if [ -n "$CRYPTO_CIPHER_ALGORITHM" ];
then
    ARGS="$ARGS --cipher-algorithm $NOISE_CRYPTO_CIPHER_ALGORITHM"
fi

# can't find reference. 
if [ -n "$NOISE_CRYPTO_HASH_ALGORITHM" ];
then
    ARGS="$ARGS --hash-algorithm $NOISE_CRYPTO_HASH_ALGORITHM"
fi

# can't find reference. 
if [ -n "$PROFILING_ARGS" ];
then
    ARGS="$ARGS $PROFILING_ARGS"
fi


# can't find reference. 
if [ -n "$NOISE_CRYPTO_HASH_ALGORITHM" ];
then
    ARGS="$ARGS --hash-algorithm $NOISE_CRYPTO_HASH_ALGORITHM"
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
    /concordium-node --baker-credentials-file "/genesis-data/genesis-${NUM_BAKERS}-bakers/bakers/baker-${BAKER_ID}-credentials.json" --no-dnssec $ARGS --id $(printf "%016d\n" $BAKER_ID)
elif [ "$MODE" == "local_bootstrapper" ]; then
    export NODE_ID="0000000001000000"
    /p2p_bootstrapper-cli \
        --id $NODE_ID \
        --listen-port 8888 \
        --regenesis-block-hashes-file $DATA_DIR/genesis_hash $ARGS $EXTRA_ARGS
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
