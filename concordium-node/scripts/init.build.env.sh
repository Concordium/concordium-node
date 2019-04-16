#!/bin/sh

# 20190123 - Moved from 2018-10-26 to latest nightly after allocator fixes in nightly
# 20190328 - Locking to agreed upon 2019-03-22
# 20190412 - Moving to Rust stable
rustup default stable

git clone https://github.com/mitls/hacl-c
( cd hacl-c && make -j$(nproc) && cp libhacl.so /usr/lib );
rm -rf hacl-c

git clone https://github.com/libffi/libffi.git
( cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install);
rm -rf libffi

(mkdir -p ~/.stack/global-project/ && echo -e "packages: []\nresolver: $(cat deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml)

( cd deps/internal/crypto/rust-src &&
  LD_LIBRARY_PATH=/usr/local/lib cargo build --release &&
  cp target/release/libec_vrf_ed25519.so /usr/local/lib &&
  cp target/release/libeddsa_ed25519.so /usr/local/lib &&
  cp target/release/libsha_2.so /usr/local/lib && cargo clean)

curl -sSL https://get.haskellstack.org/ | sh
( cd deps/internal/consensus && 
  LD_LIBRARY_PATH=/usr/local/lib stack build --ghc-options '-dynamic -j4' --force-dirty &&
  cp .stack-work/install/x86_64-linux-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so /usr/local/lib &&
  find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSacorn\*.so -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSconcordium-crypto\*.so -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; &&
  find /usr/local/lib -name libHSglobalstate\*.so -exec ln -s {} /usr/local/lib/libHSglobalstate-0.1.so \; &&
  find ~/.stack/programs -name libHS\*-\*.so -exec cp {} /usr/local/lib \; &&
  rm -rf ~/.stack/indices && rm ~/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4.tar.xz
  ) 

ldconfig
