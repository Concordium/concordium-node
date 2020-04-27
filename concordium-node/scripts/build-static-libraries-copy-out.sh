#!/usr/bin/env bash

set -e

GHC_VERSION="8.8.3"

cd /build
mv static-consensus-$GHC_VERSION.tar.gz /out
mv static-consensus-binaries-$GHC_VERSION.tar.gz /out
mv static-consensus-$GHC_VERSION-sc.tar.gz /out
mv static-consensus-binaries-$GHC_VERSION-sc.tar.gz /out
