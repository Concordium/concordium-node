#!/usr/bin/env bash

set -e 

CONSENSUS_VERSION=$(cat scripts/CONSENSUS_VERSION)

ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

if [[ -n "$CONSENSUS_TYPE" && "$CONSENSUS_TYPE" != "default" ]; then
    curl -o static-consensus-$CONSENSUS_VERSION.tar.gz https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$CONSENSUS_VERSION-$CONSENSUS_TYPE.tar.gz
else
    curl -o static-consensus-$CONSENSUS_VERSION.tar.gz https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$CONSENSUS_VERSION.tar.gz
fi

tar -xf static-consensus-$CONSENSUS_VERSION.tar.gz
rm -rf deps/static-libs/linux/*

mv target/* deps/static-libs/linux/

rm -r target static-consensus-$CONSENSUS_VERSION.tar.gz

ldconfig
