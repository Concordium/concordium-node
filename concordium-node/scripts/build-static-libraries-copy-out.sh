#!/usr/bin/env bash

set -e

cd /build
mv static-consensus-$GHC_VERSION.tar.gz /out
mv static-consensus-binaries-$GHC_VERSION.tar.gz /out