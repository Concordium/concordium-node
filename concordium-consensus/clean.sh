#!/usr/bin/env bash

git submodule update --init --recursive crypto
git submodule update --init --recursive globalstate-mockup

rm -r crypto/rust-src/target
rm -r globalstate-mockup/globalstate-rust/target

for d in $(find . -type d -name \*.stack\*); do
    rm -r $d
done
