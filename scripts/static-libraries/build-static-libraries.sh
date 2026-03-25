#!/usr/bin/env bash

set -ex

#############################################################################################################################
## Copy GHC libs

mkdir -p /target/{profiling,vanilla}/{ghc,dependencies,concordium}
mkdir -p /binaries/{lib,bin}

LIB_DIR=$(stack --stack-yaml /build/concordium-consensus/stack.static.yaml ghc -- --print-libdir)

find "$LIB_DIR" -type f -name "*_p.a" ! -name "*_debug_p.a" ! -name "*rts_p.a" ! -name "*ffi_p.a" -exec cp {} /target/profiling/ghc/ \;
find "$LIB_DIR" -type f -name "*.a" ! -name "*_p.a" ! -name "*_l.a" ! -name "*_debug.a" ! -name "*rts.a" ! -name "*ffi.a" -exec cp {} /target/vanilla/ghc/ \;

cd /build

#############################################################################################################################
## Build the project

stack build --profile --flag "concordium-consensus:-dynamic" --stack-yaml /build/concordium-consensus/stack.static.yaml
find /build/concordium-consensus/.stack-work -type f -name "*.a" ! -name "*_p.a" -exec cp {} /target/vanilla/concordium/ \;
find /build/concordium-consensus/.stack-work -type f -name "*_p.a" -exec cp {} /target/profiling/concordium/ \;

#############################################################################################################################
## Copy rust binaries

LOCAL_INSTALL_ROOT=$(stack --stack-yaml /build/concordium-consensus/stack.static.yaml path --profile --local-install-root)
cp "$LOCAL_INSTALL_ROOT"/bin/{generate-update-keys,genesis,database-exporter} /binaries/bin/
cp /build/concordium-base/rust-src/target/release/*.so /binaries/lib/
cp /build/concordium-consensus/lib/*.so /binaries/lib/

#############################################################################################################################
## Copy dependencies

find ~/.stack/snapshots/x86_64-linux/ -type f -name "*.a" ! -name "*_p.a" -exec cp {} /target/vanilla/dependencies \;
find ~/.stack/snapshots/x86_64-linux/ -type f -name "*_p.a" -exec cp {} /target/profiling/dependencies \;

## Rust files - copy the static libraries to the target directory, and strip debug symbols from them to reduce their size, as 
## they are not needed for the final binaries and can cause linking issues if they contain ruststd symbols
mkdir -p /target/rust
cp -r /build/concordium-base/rust-src/target/release/*.a /target/rust/

find /target /binaries -type f -exec strip --strip-debug {} \;

#############################################################################################################################
## Remove ruststd symbols from rust libraries

(
    cd /target/rust

    ## takes every .a static file and unpacks them
    for i in $(ls)
    do
        ar x $i;
        rm $i;
    done

    set +e

    ## delete rust specific runtime symbols from the .o files, as they are not needed and cause linking issues
    for file in $(find . -type f -name "*.o"); do
        if nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)\|\(T __clzsi2\)\|\(T rust_eh_personality\)" >> /dev/null; then
            echo "Removing file:";
            echo $file;
            rm $file;
        fi
    done

    set -e

    ## creates the librcrypto.a static library from the remaining .o files, which should now be free of ruststd symbols
    ar rcs libRcrypto.a *.o
    rm *.o

    ## copy Rust node library and repeat the process to remove ruststd symbols from it as well, as it is linked into the consensus node and must not contain ruststd symbols
    cp /build/concordium-consensus/lib/libnode_rust_library.a /target/rust/libnode_rust_library.a

    ar x libnode_rust_library.a

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

    rm libnode_rust_library.a
    ar rcs libnode_rust_library.a *.o
    rm *.o
)

cd /build

#############################################################################################################################

tar czf static-consensus-$GHC_VERSION.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION.tar.gz /binaries
rm -rf /target /binaries
