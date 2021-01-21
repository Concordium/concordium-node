#!/usr/bin/env bash

FEATURES=$1

if [[ -n "$CONSENSUS_PROFILING" && "$CONSENSUS_PROFILING" == "true" ]]; then
    FEATURES="$FEATURES,profiling"
else
    FEATURES="$FEATURES,static"
fi

if [[ -n "$2" && "$2" == "release" ]]; then
    echo "Building release build with $FEATURES"
    cargo build --manifest-path concordium-node/Cargo.toml --release --features=$FEATURES
else
    echo "Building debug build with $FEATURES"
    cargo build --manifest-path concordium-node/Cargo.toml --features=$FEATURES
fi
