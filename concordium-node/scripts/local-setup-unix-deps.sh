#!/bin/sh
sudo rm -rf consensus
sudo rm -f /usr/local/lib/libHSConcordium-*.so
sudo rm -f /usr/local/lib/libHSlanguage-glsl-*.so
sudo rm -f /usr/local/lib/libHSoak-*.so
sudo rm -f /usr/local/lib/libHSmonadplus-*.so
git clone git@gitlab.com:Concordium/consensus/prototype.git consensus

if [ -d ~/.stack/global-project/ ]; then
    if [ -f ~/.stack/global-project/stack.yaml ]; then
        mv ~/.stack/global-project/stack.yaml ~/.stack/global-project/stack.yaml.$$
    fi
else
    mkdir -p ~/.stack/global-project
fi

cp scripts/stack.yaml ~/.stack/global-project/stack.yaml

( cd consensus &&
  git checkout oak-integration && 
  git submodule update --init --recursive &&
  stack build --ghc-options '-dynamic' --force-dirty &&
  sudo cp .stack-work/install/x86_64-linux-tinfo6/lts-12.19/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so /usr/local/lib &&
  sudo find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  sudo find /usr/local/lib -name libHSlanguage-glsl-0.3.0-\*.so -exec ln -s {} /usr/local/lib/libHSlanguage-glsl-0.3.0.so \; &&
  sudo find /usr/local/lib -name libHSoak-0.19.0-\*.so -exec ln -s {} /usr/local/lib/libHSoak-0.19.0.so \; &&
  sudo find /usr/local/lib -name libHSmonadplus-1.3\*.so -exec ln -s {} /usr/local/lib/libHSmonadplus-1.3.so \; 
  ) 
