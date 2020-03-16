#!/usr/bin/env bash

# Enable all errors
set -e

# Retrieve genesis data sha
GENESIS_VERSION=$(cat scripts/GENESIS_DATA_VERSION)

# Setup directory we expect them to be in
mkdir -p genesis-complementary-bundle

# Download all files in archive
(
    cd genesis-complementary-bundle &&
    curl -s https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com/complementary-bundle-${GENESIS_VERSION}.tar.gz | tar xzf -
)