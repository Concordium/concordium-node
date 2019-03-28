#!/bin/sh
sudo rm -f /usr/local/lib/libHSConcordium-*.so
sudo rm -f /usr/local/lib/libHSlanguage-glsl-*.so
sudo rm -f /usr/local/lib/libHSoak-*.so
sudo rm -f /usr/local/lib/libHSmonadplus-*.so
sudo rm -f /usr/local/lib/libHSacorn-*.so
sudo rm -f /usr/local/lib/libHSconcordium-crypto-*.so
sudo rm -f /usr/local/lib/libec_vrf_ed25519.so
sudo rm -f /usr/local/lib/libeddsa_ed25519.so
sudo rm -f /usr/local/lib/libsha_2.so

if [ -d ~/.stack/global-project/ ]; then
    if [ -f ~/.stack/global-project/stack.yaml ]; then
        mv ~/.stack/global-project/stack.yaml ~/.stack/global-project/stack.yaml.$$
    fi
else
    mkdir -p ~/.stack/global-project
fi

cp scripts/stack.yaml ~/.stack/global-project/stack.yaml

( cd deps/internal/crypto/rust-src &&
  cargo build --release &&
  sudo cp target/release/libec_vrf_ed25519.so /usr/local/lib &&
  sudo cp target/release/libeddsa_ed25519.so /usr/local/lib &&
  sudo cp target/release/libsha_2.so /usr/local/lib )

sudo rm -rf deps/internal/crypto/build

( cd deps/internal/consensus &&
  stack build --ghc-options '-dynamic' --force-dirty &&
  sudo cp .stack-work/install/x86_64-linux-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so /usr/local/lib &&
  sudo find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  sudo find /usr/local/lib -name libHSacorn\*.so -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.so \; &&
  sudo find /usr/local/lib -name libHSconcordium-crypto\*.so -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; 
  ) 

sudo ldconfig
