#!/bin/sh

( cd deps/internal/crypto/rust-src &&
  LD_LIBRARY_PATH=/usr/local/lib cargo build --release &&
  cp target/release/libec_vrf_ed25519.so /usr/local/lib &&
  cp target/release/libeddsa_ed25519.so /usr/local/lib &&
  cp target/release/libdodis_yampolskiy_prf.so /usr/local/lib &&
  cp target/release/libpedersen_scheme.so /usr/local/lib &&
  cp target/release/libsha_2.so /usr/local/lib && cargo clean)

( cd deps/internal/consensus && 
  LD_LIBRARY_PATH=/usr/local/lib stack build --ghc-options '-dynamic -j4' --force-dirty &&
  cp .stack-work/install/x86_64-linux-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/$(stack ghc -- --version --short | awk '{ print $NF }')/lib/x86_64-linux-ghc-$(stack ghc -- --version --short | awk '{ print $NF }')/libHS*.so /usr/local/lib &&
  find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSacorn\*.so -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSconcordium-crypto\*.so -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; &&
  find /usr/local/lib -name libHSglobalstate-0.1*.so -exec ln -s {} /usr/local/lib/libHSglobalstate-0.1.so \; &&
  find /usr/local/lib -name libHSglobalstate-types-\*.so -exec ln -s {} /usr/local/lib/libHSglobalstate-types-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSscheduler-\*.so -exec ln -s {} /usr/local/lib/libHSscheduler-0.1.0.0.so \; &&
  find ~/.stack/programs/x86_64-linux/ghc-tinfo6-$(stack ghc -- --version --short | awk '{ print $NF }')/lib/ghc-$(stack ghc -- --version --short | awk '{ print $NF }') -name libHS\*-\*.so -exec cp {} /usr/local/lib \; &&
  rm -rf ~/.stack/indices && rm ~/.stack/programs/x86_64-linux/ghc-tinfo6-*.tar.xz
  ) 

ldconfig