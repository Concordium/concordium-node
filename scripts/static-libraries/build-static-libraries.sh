#!/usr/bin/env bash

set -e
STACK_VERSION="2.5.1"

#############################################################################################################################
## Copy GHC libs

sed -i 's/git-fetch-with-cli = true/git-fetch-with-cli = false/' /build/concordium-base/rust-src/.cargo/config

mkdir -p /target/{profiling,vanilla}/{ghc,dependencies,concordium}
mkdir -p /binaries/{lib,bin}
for lib in $(find `stack ghc -- --print-libdir` -type f -name "*_p.a" ! -name "*_debug_p.a" ! -name "*rts_p.a" ! -name "*ffi_p.a"); do
    cp $lib /target/profiling/ghc/
done

for lib in $(find `stack ghc -- --print-libdir` -type f -name "*.a" ! -name "*_p.a" ! -name "*_l.a" ! -name "*_debug.a" ! -name "*rts.a" ! -name "*ffi.a"); do
    cp $lib /target/vanilla/ghc/
done

cd /build

#############################################################################################################################
## Build the project

cargo update --manifest-path /build/concordium-base/rust-src/Cargo.toml
cargo check --manifest-path /build/concordium-base/rust-src/Cargo.toml

stack build --profile --flag "concordium-consensus:-dynamic" --stack-yaml /build/concordium-consensus/stack.integer-simple.yaml

for lib in $(find /build/concordium-consensus/.stack-work -type f -name "*.a" ! -name "*_p.a"); do
    cp $lib /target/vanilla/concordium/;
done

for lib in $(find /build/concordium-consensus/.stack-work -type f -name "*_p.a"); do
    cp $lib /target/profiling/concordium/;
done

#############################################################################################################################
## Copy rust binaries

# TODO: genesis executable might be gone
cp $(find /build/concordium-base/.stack-work -type f \( -name genesis -or -name generate-update-keys \)) /binaries/bin/
cp /build/concordium-base/rust-src/target/release/*.so /binaries/lib/
cp /build/concordium-consensus/smart-contracts/wasm-chain-integration/target/release/*.so /binaries/lib/
cargo build --release --manifest-path /build/concordium-base/rust-bins/Cargo.toml
cp /build/concordium-base/rust-bins/target/release/{client,genesis_tool,generate_testdata} /binaries/bin/


#############################################################################################################################
## Copy dependencies

for lib in $(find ~/.stack/snapshots/x86_64-linux/ -type f -name "*.a" ! -name "*_p.a"); do
    cp $lib /target/vanilla/dependencies;
done

for lib in $(find ~/.stack/snapshots/x86_64-linux/ -type f -name "*_p.a"); do
    cp $lib /target/profiling/dependencies;
done

mkdir -p /target/rust
cp -r /build/concordium-base/rust-src/target/release/*.a /target/rust/

for f in $(find /target /binaries -type f); do
    strip --strip-debug $f;
done

#############################################################################################################################
## Remove ruststd symbols from rust libraries

(
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

    cp /build/concordium-consensus/smart-contracts/wasm-chain-integration/target/release/libwasm_chain_integration.a /target/rust/libwasm_chain_integration.a

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
)

cd /build

#############################################################################################################################

tar czf static-consensus-$GHC_VERSION.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION.tar.gz /binaries
rm -rf /target /binaries
