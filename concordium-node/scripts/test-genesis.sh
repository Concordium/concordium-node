#!/usr/bin/env bash
set -e
GENESIS_TESTER_BIN="target/debug/genesis_tester"

rm -rf genesis-data-test
mkdir genesis-data-test
tar xzf genesis-data/$1 -C genesis-data-test

if [ -n "$2" ]; then
    echo "Testing genesis $1 and private key $2"
    $GENESIS_TESTER_BIN --genesis-file genesis-data-test/genesis_data/genesis.dat --private-key-file genesis-data-test/genesis_data/baker-$2-credentials.json
else
    echo "Testing genesis $1"
    $GENESIS_TESTER_BIN --genesis-file genesis-data-test/genesis_data/genesis.dat
fi
