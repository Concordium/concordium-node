#!/usr/bin/env bash

set -euxo pipefail

rm -rf /out/*
cp /build/*.deb /out

#TODO: Fix permissions on output
#chown -R $EXTERNAL_UID:$EXTERNAL_GID /out
