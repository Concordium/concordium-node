#!/usr/bin/env bash

set -euxo pipefail

if [ "$#" -eq 1 ]; then
    genesis_version="$1"
else
    genesis_version="$(cat scripts/GENESIS_DATA_VERSION)"
fi

mkdir -p genesis-data
cd genesis-data

s3_bucket_url="https://s3-eu-west-1.amazonaws.com/genesis-data.concordium.com"

#curl -sSf "$s3_bucket_url/finbench-bakers-${genesis_version}.tar.gz" > finbench-bakers.tar.gz
curl -sSf "$s3_bucket_url/tps-bakers-${genesis_version}.tar.gz" > tps-bakers.tar.gz
curl -sSf "$s3_bucket_url/catchup-bakers-${genesis_version}.tar.gz" > catchup-bakers.tar.gz

curl -sSf "$s3_bucket_url/CONTENTS-${genesis_version}" |
	while read baker_size; do
		curl -sSf "$s3_bucket_url/${baker_size}-bakers-${genesis_version}.tar.gz" > "${baker_size}-bakers.tar.gz"
	done
