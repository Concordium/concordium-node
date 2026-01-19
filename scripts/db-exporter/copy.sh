#!/usr/bin/env bash

set -euxo pipefail

rm -rf /out/*
cp /build/*.deb /out

# Change ownership of build artifacts.
if [[ ! -z ${EXTERNAL_UID+x} ]] && [[ ! -z ${EXTERNAL_GID+x} ]]
then
    chown -R $EXTERNAL_UID:$EXTERNAL_GID /out
fi
