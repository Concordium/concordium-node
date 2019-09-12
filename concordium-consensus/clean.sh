#!/usr/bin/env bash

rm -r crypto/rust-src/target
rm -r globalstate-mockup/deps/concordium-global-state-sys/target
(cd globalstate-mockup/
 git submodule deinit --force deps/concordium-global-state-sys
 git submodule update --init deps/concordium-global-state-sys
 cd deps/concordium-global-state-sys
 git submodule update --init deps/p2p-client
 cd deps/p2p-client
 git submodule update --init deps/internal/crypto)

for d in $(find . -type d -name \*.stack\*); do
    rm -r $d
done
