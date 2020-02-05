#!/usr/bin/env bash

# Enable all errors
set -e

# Retrieve genesis data sha
GENESIS_VERSION=$(cat scripts/GENESIS_DATA_VERSION)

# Setup directory we expect them to be in
mkdir -p genesis-data

# Download all files in archive
(
    cd genesis-data &&
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/CONTENTS-${GENESIS_VERSION} | while read BAKER_SIZE; do
        curl -s https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/${BAKER_SIZE}-bakers-${GENESIS_VERSION}.tar.gz > ${BAKER_SIZE}-bakers.tar.gz
    done
)