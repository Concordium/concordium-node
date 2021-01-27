#!/usr/bin/env bash

# Temporary build script for Jenkins

set -ex

./scripts/download-genesis-data.sh

VERSION=$(awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml) # extract and unquote value of the first occurrence of a 'version' key in Cargo.toml

docker build --ssh default -t concordium/staging-client:$VERSION -f scripts/distribution/staging-net-client.Dockerfile . --no-cache

docker save concordium/staging-client:$VERSION | gzip > staging-client-$VERSION.tar.gz
echo $VERSION > VERSION

aws s3 cp staging-client-$VERSION.tar.gz s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
aws s3 cp VERSION s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
