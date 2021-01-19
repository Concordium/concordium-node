#!/usr/bin/env bash

set -e
STACK_VERSION="2.5.1"

###########################################################################################################################
## Get GHC and Stack

ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

curl https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/ghc-$GHC_VERSION-fpic-simple-x86_64-unknown-linux-gnu.tar.gz -o ghc-$GHC_VERSION-fpic-simple-x86_64-unknown-linux-gnu.tar.gz

tar -xf ghc-$GHC_VERSION-fpic-simple-x86_64-unknown-linux-gnu.tar.gz
cp -r bootstrapped_out/* /
rm -r bootstrapped_out ghc-$GHC_VERSION-fpic-simple-x86_64-unknown-linux-gnu.tar.gz

sed -i 's/git-fetch-with-cli = true/git-fetch-with-cli = false/' /build/crypto/rust-src/.cargo/config

wget https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz
tar -xf stack-$STACK_VERSION-linux-x86_64.tar.gz
mkdir -p $HOME/.stack/bin
mv stack-$STACK_VERSION-linux-x86_64/stack $HOME/.stack/bin
export PATH=$PATH:$HOME/.stack/bin
echo "system-ghc: true" > ~/.stack/config.yaml
stack update


#############################################################################################################################
## Copy GHC libs

mkdir -p /target/{profiling,vanilla}/{ghc,cabal,concordium}
mkdir -p /binaries/{lib,bin}
for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*_p.a" ! -name "*_debug_p.a" ! -name "*rts_p.a" ! -name "*ffi_p.a"); do
    cp $lib /target/profiling/ghc/
done

for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*.a" ! -name "*_p.a" ! -name "*_l.a" ! -name "*_debug.a" ! -name "*rts.a" ! -name "*ffi.a"); do
    cp $lib /target/vanilla/ghc/
done


#############################################################################################################################
## Build the project with smart contracts

cd /build

(
    cd /build/crypto/rust-src &&
        cargo update &&
        cargo check
)

stack build --profile --flag "Concordium:-dynamic" --stack-yaml stack.integer-simple.yaml

for lib in $(find .stack-work -type f -name "*.a" ! -name "*_p.a"); do
    cp $(pwd)/$lib /target/vanilla/concordium/;
done

for lib in $(find .stack-work -type f -name "*_p.a"); do
    cp $(pwd)/$lib /target/profiling/concordium/;
done

#############################################################################################################################
## Copy rust binaries

cp $(find .stack-work -type f \( -name genesis -or -name generate-update-keys \)) /binaries/bin/
cp $(pwd)/crypto/rust-src/target/release/*.so /binaries/lib/
cp $(pwd)/smart-contracts/wasm-chain-integration/target/release/*.so /binaries/lib/
(
    cd crypto/rust-bins &&
    cargo build --release
)
cp $(pwd)/crypto/rust-bins/target/release/{client,genesis_tool,generate_testdata} /binaries/bin/


#############################################################################################################################
## Copy dependencies

for lib in $(find ~/.stack/snapshots/x86_64-linux/ -type f -name "*.a" ! -name "*_p.a"); do
    cp $lib /target/vanilla/cabal;
done

for lib in $(find ~/.stack/snapshots/x86_64-linux/ -type f -name "*_p.a"); do
    cp $lib /target/profiling/cabal;
done

mkdir -p /target/rust
cp -r $(pwd)/crypto/rust-src/target/release/*.a /target/rust/

for f in $(find /target /binaries -type f); do
    strip --strip-debug $f;
done

#############################################################################################################################
## Remove ruststd symbols from rust libraries

cd /target/rust

for i in $(ls)
do
    ar x $i;
    rm $i;
done

set +e

for file in $(find . -type f -name "*.o"); do
  if nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)\|\(T __clzsi2\)\|\(T rust_eh_personality\)" >> /dev/null; then
    echo "Removing file:";
    echo $file;
    rm $file;
  fi
done

set -e

ar rcs libRcrypto.a *.o
rm *.o

cp /build/smart-contracts/wasm-chain-integration/target/release/libwasm_chain_integration.a /target/rust/libwasm_chain_integration.a
ar x libwasm_chain_integration.a

set +e

for file in $(find . -type f -name "*.o"); do
  nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)\|\(T __clzsi2\)\|\(T rust_eh_personality\)" >> /dev/null;
  if [ $? -eq 0 ]; then
    echo "Removing file:"
    echo $file
    rm $file;
  fi
done

set -e


rm libwasm_chain_integration.a
ar rcs libwasm_chain_integration.a *.o
rm *.o

cd /build

#############################################################################################################################

tar czf static-consensus-$GHC_VERSION.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION.tar.gz /binaries
rm -rf /target /binaries
