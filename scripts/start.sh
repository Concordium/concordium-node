#!/usr/bin/env bash

# SIGTERM handler
_term() {
  kill -TERM ${CHILD_PID} 2>/dev/null
  wait ${CHILD_PID}
}

if [ -z "$CONCORDIUM_NODE_CONFIG_DIR" ]; then
    echo "CONCORDIUM_NODE_CONFIG_DIR must be set."
    exit 1
else
    mkdir -p $CONCORDIUM_NODE_CONFIG_DIR
fi

if [ -z "$CONCORDIUM_NODE_DATA_DIR" ]; then
    echo "CONCORDIUM_NODE_DATA_DIR must be set."
    exit 1
else
    mkdir -p $CONCORDIUM_NODE_DATA_DIR
fi

if [ -n "$DISTRIBUTION_CLIENT" ];
then
    cp /genesis.dat $CONCORDIUM_NODE_DATA_DIR
fi

# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib

if [ -n "$ENABLE_TERM_HANDLER" ];
then
    trap _term SIGTERM
    /concordium-node $ARGS &
    CHILD_PID=$!
    wait $CHILD_PID
else
    /concordium-node $ARGS
fi
