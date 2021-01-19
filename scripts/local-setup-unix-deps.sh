#!/usr/bin/env bash
set -x

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
sudo rm -f /usr/local/lib/libHSconcordium-base-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSglobalstate-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libHSscheduler-*.$LIBEXTENSION
sudo rm -f /usr/local/lib/libconcordium-base.$LIBEXTENSION*
sudo rm -f /usr/local/lib/libec_vrf_ed25519.$LIBEXTENSION
sudo rm -f /usr/local/lib/libeddsa_ed25519.$LIBEXTENSION
sudo rm -f /usr/local/lib/libsha_2.$LIBEXTENSION
sudo rm -f /usr/local/lib/libdodis_yampolskiy_prf.$LIBEXTENSION
sudo rm -f /usr/local/lib/libpedersen_scheme.$LIBEXTENSION
sudo rm -f /usr/local/lib/libelgamal.$LIBEXTENSION
sudo rm -f /usr/local/lib/libaggregate_sig.$LIBEXTENSION
sudo rm -f /usr/local/lib/libcurve_arithmetic.$LIBEXTENSION
sudo rm -f /usr/local/lib/libec_vrf_ed25519_sha256.$LIBEXTENSION
sudo rm -f /usr/local/lib/libps_sig.$LIBEXTENSION
sudo rm -f /usr/local/lib/libsecret_sharing.$LIBEXTENSION
sudo rm -f /usr/local/lib/libsigma_protocols.$LIBEXTENSION
sudo rm -f /usr/local/lib/libid.$LIBEXTENSION

if [ -d ~/.stack/global-project/ ]; then
    if [ -f ~/.stack/global-project/stack.yaml ]; then
        mv ~/.stack/global-project/stack.yaml ~/.stack/global-project/stack.yaml.$$
    fi
else
    mkdir -p ~/.stack/global-project
fi


echo -e "packages: []\nresolver: $(cat deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml

( cd deps/internal/consensus/crypto/rust-src &&
      LD_LIBRARY_PATH=/usr/local/lib cargo build &&
      cd target/debug &&
      for l in $(find . -maxdepth 1 -type f -name \*.so); do
          sudo cp $l /usr/local/lib;
      done
)

( cd deps/internal/consensus &&
  rm -rf .stack-work &&
  LD_LIBRARY_PATH=/usr/local/lib stack build --ghc-options '-dynamic' --force-dirty &&
  cd .stack-work &&
  for f in $(find . -type f -name libHS\*.so); do
      sudo cp $(pwd)/$f /usr/local/lib
  done
  sudo find /usr/local/lib -name libHSConcordium\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSacorn\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSglobalstate-0.1\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSglobalstate-0.1.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSglobalstate-types-\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSglobalstate-types-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSscheduler-\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSscheduler-0.1.0.0.$LIBEXTENSION \; &&
  sudo find /usr/local/lib -name libHSconcordium-base-0.1\*.$LIBEXTENSION -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.$LIBEXTENSION \; &&
  rm -rf .stack-work
  )

sudo ldconfig
