#!/usr/bin/env bash
if [[ "$OSTYPE" == "darwin"* ]]; then
	LIBEXTENSION="dylib"
    PLATFORM="osx"
else
	LIBEXTENSION="so"
    PLATFORM="linux"
fi
sudo rm -f /usr/local/lib/libHSConcordium-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSlanguage-glsl-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSoak-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSmonadplus-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSacorn-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSconcordium-crypto-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSglobalstate-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSscheduler-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libconcordium-crypto.$LIBEXTENSION*
sudo rm -f /usr/local/lib/libec_vrf_ed25519.$LIBEXTENSION
sudo rm -f /usr/local/lib/libeddsa_ed25519.$LIBEXTENSION
sudo rm -f /usr/local/lib/libsha_2.$LIBEXTENSION
sudo rm -f /usr/local/lib/libdodis_yampolskiy_prf.$LIBEXTENSION
sudo rm -f /usr/local/lib/libpedersen_scheme.$LIBEXTENSION
sudo rm -f /usr/local/lib/libelgamal.$LIBEXTENSION

if [ -d ~/.stack/global-project/ ]; then
    if [ -f ~/.stack/global-project/stack.yaml ]; then
        mv ~/.stack/global-project/stack.yaml ~/.stack/global-project/stack.yaml.$$
    fi
else
    mkdir -p ~/.stack/global-project
fi

echo -e "packages: []\nresolver: $(cat deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml

( cd deps/internal/crypto/rust-src &&
  LD_LIBRARY_PATH=/usr/local/lib cargo build &&
  sudo cp target/debug/libec_vrf_ed25519.$LIBEXTENSION /usr/local/lib &&
  sudo cp target/debug/libeddsa_ed25519.$LIBEXTENSION /usr/local/lib &&
  sudo cp target/debug/libsha_2.$LIBEXTENSION /usr/local/lib &&
  sudo cp target/debug/libdodis_yampolskiy_prf.$LIBEXTENSION /usr/local/lib &&
  sudo cp target/debug/libpedersen_scheme.$LIBEXTENSION /usr/local/lib &&
  sudo cp target/debug/libelgamal.$LIBEXTENSION /usr/local/lib &&
  rm -rf target/ )

sudo ldconfig

( cd deps/internal/consensus &&
  LD_LIBRARY_PATH=/usr/local/lib stack build --ghc-options '-dynamic' --force-dirty &&
  sudo cp .stack-work/install/x86_64-$PLATFORM-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/$(stack ghc -- --version --short | awk '{ print $NF }')/lib/x86_64-$PLATFORM-ghc-$(stack ghc -- --version --short | awk '{ print $NF }')/libHS*.$LIBEXTENSION /usr/local/lib &&
  sudo find /usr/local/lib -name libHSConcordium\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSacorn\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSglobalstate-0.1\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSglobalstate-0.1.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSglobalstate-types-\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSglobalstate-types-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSscheduler-\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSscheduler-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSconcordium-crypto-\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.$LIBEXTENSION
  rm -rf .stack-work
  ) 

sudo ldconfig

