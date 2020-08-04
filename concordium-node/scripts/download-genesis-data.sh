#!/usr/bin/env bash

# Enable all errors
set -e

# Retrieve genesis data sha
if [ -n "$1" ]; then
    GENESIS_VERSION=$1
else
    GENESIS_VERSION=$(cat scripts/GENESIS_DATA_VERSION)
fi

echo "Downloading genesis data SHA $GENESIS_VERSION"

# Setup directory we expect them to be in
mkdir -p genesis-data

# Download all files in archive and the benchmark bundles
(
    cd genesis-data &&
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/finbench-bakers-${GENESIS_VERSION}.tar.gz > finbench-bakers.tar.gz
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/tps-bakers-${GENESIS_VERSION}.tar.gz > tps-bakers.tar.gz
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/catchup-bakers-${GENESIS_VERSION}.tar.gz > catchup-bakers.tar.gz
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/CONTENTS-${GENESIS_VERSION} | while read BAKER_SIZE; do
        curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/${BAKER_SIZE}-bakers-${GENESIS_VERSION}.tar.gz > ${BAKER_SIZE}-bakers.tar.gz
    done
)