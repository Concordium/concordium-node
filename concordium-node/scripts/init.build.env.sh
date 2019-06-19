#!/bin/sh

( cd deps/internal/crypto/rust-src &&
  LD_LIBRARY_PATH=/usr/local/lib cargo build --release &&
  cp target/release/libec_vrf_ed25519.so /usr/local/lib &&
  cp target/release/libeddsa_ed25519.so /usr/local/lib &&
  cp target/release/libdodis_yampolskiy_prf.so /usr/local/lib &&
  cp target/release/libpedersen_scheme.so /usr/local/lib &&
  cp target/release/libelgamal.so /usr/local/lib &&
  cp target/release/libsha_2.so /usr/local/lib && cargo clean)

CONSENSUS_VERSION=$(cat CONSENSUS_VERSION)

ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

curl -o static-consensus-$CONSENSUS_VERSION.tar.gz https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$CONSENSUS_VERSION.tar.gz
tar -xf static-consensus-$CONSENSUS_VERSION.tar.gz

rm -rf deps/static-libs/linux/*

mv target/* deps/static-libs/linux/

rm -r target static-consensus-$CONSENSUS_VERSION.tar.gz

ldconfig
