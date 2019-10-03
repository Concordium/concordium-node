#!/usr/bin/env bash

git submodule update --init crypto
git submodule update --init globalstate-mockup

rm -r crypto/rust-src/target
rm -r globalstate-mockup/deps/concordium-global-state-sys/target

(cd globalstate-mockup/
 git submodule deinit --force deps/concordium-global-state-sys
 git submodule update --init deps/concordium-global-state-sys
 cd deps/concordium-global-state-sys
 git submodule update --init deps/p2p-client
 cd deps/p2p-client
 git submodule update --init deps/internal/consensus
 cd deps/internal/consensus
 git submodule update --init crypto)

for d in $(find . -type d -name \*.stack\*); do
    rm -r $d
done
