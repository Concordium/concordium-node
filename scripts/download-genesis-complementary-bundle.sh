#!/usr/bin/env bash

set -euxo pipefail

genesis_version="$(cat scripts/GENESIS_DATA_VERSION)"
dir=genesis-complementary-bundle

mkdir -p "$dir"

s3_bucket_url="https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com"

curl -sSf "$s3_bucket_url/complementary-bundle-${genesis_version}.tar.gz" |
	tar -C "$dir" -xzf -
