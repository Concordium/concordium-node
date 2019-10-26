#!/usr/bin/env bash

FEATURES=$1

if [[ -z "$CONSENSUS_TYPE" && "$CONSENSUS_TYPE" == "no-rgs" ]]; then
    FEATURES="$FEATURES,no_rgs"
fi

if [[ -n "$2" && "$2" == "release" ]]; then
    echo "Building release build with $FEATURES"
    cargo build --release --features=$FEATURES
else
    echo "Building debug build with $FEATURES"
    cargo build --features=$FEATURES
fi