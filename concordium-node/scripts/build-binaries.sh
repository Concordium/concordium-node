#!/usr/bin/env bash

FEATURES=$1

if [[ -z "$CONSENSUS_TYPE" && "$CONSENSUS_TYPE" == "no-rgs" ]]; then
    FEATURES="$FEATURES,no_rgs"
fi

MODE=""
if [[ -z "$2" && "$2" == "release" ]]; then
    MODE="--release"
fi

cargo build $MODE --features=$FEATURES