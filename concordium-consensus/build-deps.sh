#!/usr/bin/env bash

RUST_SRC=./crypto/rust-src

if [ -d $RUST_SRC ]; then
    cargo build --release --manifest-path $RUST_SRC/Cargo.toml
    mkdir -p ./extra-libs
    cp $RUST_SRC/target/release/*.so ./extra-libs
fi

