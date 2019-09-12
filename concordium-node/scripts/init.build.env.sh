#!/usr/bin/env bash

set -e 

if [ -f "CONSENSUS_VERSION" ] ; then
    CONSENSUS_VERSION=$(cat CONSENSUS_VERSION)
else
    CONSENSUS_VERSION=`git submodule | grep consensus | head -n1 | awk '{print $1}'`
fi

ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

curl -o static-consensus-$CONSENSUS_VERSION.tar.gz https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$CONSENSUS_VERSION.tar.gz
tar -xf static-consensus-$CONSENSUS_VERSION.tar.gz

rm -rf deps/static-libs/linux/*

mv target/* deps/static-libs/linux/

rm -r target static-consensus-$CONSENSUS_VERSION.tar.gz

ldconfig
