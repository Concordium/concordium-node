#!/usr/bin/env bash

section () {
    echo "


##################################################### $1 ###############################################################


"
}

subsection () {
    echo "


******** $1 *********


"
}

set -e
GHC_BUILDER_VERSION="8.8.3"
CABAL_BUILDER_VERSION="3.0.0.0"
CABAL_VERSION="$CABAL_BUILDER_VERSION"
STACK_VERSION="2.1.3"

echo "We will run the following process:

* Fetch dependencies
* Fetch compiler tools
  - GHC
  - Cabal
  - Stack
* Copy the ghc libraries
* Prepare the project
  - Prepare the flags for the static build
  - Generate freeze file
  - Early cargo-check on crypto
* Build the project in default mode
  - Build the project
  - Copy the libraries
* Build the project with smart contracts
  - Build the project
  - Copy the libraries
* Build the rust utility binaries
* Copy other libraries
  - Cabal libraries
  - Rust libraries
  - Strip debug symbols
* Remove duplicate objects
"

ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

# Compile lmdb
git clone https://github.com/LMDB/lmdb
(
    cd lmdb/libraries/liblmdb
    sed -i 's/CFLAGS  =/CFLAGS  = -DMDB_FDATASYNC_WORKS/g' Makefile
    make install
    mv /usr/local/lib/liblmdb* /usr/lib/
    mv /usr/local/include/lmdb.h /usr/include/
)
rm -rf lmdb

#############################################################################################################################
section "Fetching compiler tools"

wget -q https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/fpic-ghc-$GHC_VERSION.tar.gz

tar -xf fpic-ghc-$GHC_VERSION.tar.gz
cp -r bootstrapped_out/* /
rm -r bootstrapped_out

subsection "GHC: OK"

sed -i 's/git-fetch-with-cli = true/git-fetch-with-cli = false/' /build/crypto/rust-src/.cargo/config

subsection "RUST: OK"

wget -q https://downloads.haskell.org/~cabal/cabal-install-$CABAL_VERSION/cabal-install-$CABAL_VERSION-x86_64-unknown-linux.tar.xz
tar -xf cabal-install-$CABAL_VERSION-x86_64-unknown-linux.tar.xz
mkdir -p $HOME/.cabal/bin
chmod +x cabal
mv cabal $HOME/.cabal/bin/
export PATH=$PATH:$HOME/.cabal/bin
cabal update

subsection "CABAL: OK"

wget -q https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64-static.tar.gz
tar -xf stack-$STACK_VERSION-linux-x86_64-static.tar.gz
mkdir -p $HOME/.stack/bin
mv stack-$STACK_VERSION-linux-x86_64-static/stack $HOME/.stack/bin
export PATH=$PATH:$HOME/.stack/bin
echo "system-ghc: true" > ~/.stack/config.yaml
stack update

subsection "STACK: OK"

#############################################################################################################################
section "Copying ghc libraries"

mkdir -p /target/{profiling,vanilla}/{ghc,cabal,concordium}
mkdir -p /target-sc/{profiling,vanilla}/{ghc,cabal,concordium}
mkdir -p /binaries/{lib,bin}
for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*_p.a"); do
    cp $lib /target/profiling/ghc/
done

for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*[^_p].a"); do
    cp $lib /target/vanilla/ghc/
done

# When copying the profiling libraries we are copying a bunch of libs that are
# in the ghc libs directory but would contain duplicated symbols, namely the
# debug versions of the libraries and the non threaded versions.
for l in /target/profiling/ghc/libCffi_p.a \
             /target/profiling/ghc/libHSrts_p.a \
             $(find /target/profiling/ghc -name "*debug_p*.a") \
             /target/vanilla/ghc/libCffi.a \
             /target/vanilla/ghc/libHSrts.a \
             /target/vanilla/ghc/libHSCabal-3.0.1.0.a \
             /target/vanilla/ghc/libHSghc-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghc-boot-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghc-heap-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghci-$GHC_VERSION.a \
             /target/vanilla/ghc/libHShpc-0.6.0.3.a \
             /target/vanilla/ghc/libHSterminfo-0.4.1.4.a \
             $(find /target/vanilla/ghc -name "libffi*") \
             $(find /target/vanilla/ghc -name "*[debug|l].a"); do
    rm $l;
done
cp -r /target/* /target-sc/

#############################################################################################################################
section "Preparing project"

cp /manifests/cabal.project           /build
cp /manifests/cabal.project.local     /build

for f in $(find /build -type f -name package.yaml); do
   sed -i -e 's/[\s]*ld-options://g' -e 's/[\s]*- -static//g' $f
done

subsection "Prepare flags for static build: OK"

cd /build

stack ls dependencies > /dev/null # to generate the cabal files

cabal freeze --constraint="Concordium -dynamic" \
      --constraint="globalstate-types +disable-smart-contracts"
while IFS= read -r line
do
    p=$(echo "$line" | cut -d' ' -f1)
    c=$(echo "$line" | cut -d' ' -f2)
    sed -i "s/any.$p ==.*,/any.$p ==$c,/g" cabal.project.freeze
done <<< $(stack ls dependencies)

subsection "Generated freeze file: OK"

(
    cd /build/crypto/rust-src &&
        cargo update &&
        cargo check
)

subsection "Pre-check in crypto libs: OK"

#############################################################################################################################
section "Build the project in default mode"

LD_LIBRARY_PATH=$(pwd)/crypto/rust-src/target/release:$(pwd)/smart-contracts/wasmer-interp/target/release cabal build all

subsection "Project built: OK"

for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib /target/vanilla/concordium/;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib /target/profiling/concordium/;
done

subsection "Libraries copied: OK"

#############################################################################################################################
section "Build the project with smart contracts"

sed -i 's/globalstate-types +disable-smart-contracts/globalstate-types -disable-smart-contracts/g' cabal.project.freeze

LD_LIBRARY_PATH=$(pwd)/crypto/rust-src/target/release:$(pwd)/smart-contracts/wasmer-interp/target/release cabal build all

subsection "Project build: OK"

for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib /target-sc/vanilla/concordium/;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib /target-sc/profiling/concordium/;
done

subsection "Libraries copied: OK"

#############################################################################################################################
section "Build the rust utility binaries"

cp dist-newstyle/build/x86_64-linux/ghc-$GHC_BUILDER_VERSION/Concordium-0.1.0.0/x/genesis/build/genesis/genesis /binaries/bin/
cp $(pwd)/crypto/rust-src/target/release/*.so /binaries/lib/
(
    cd crypto/rust-bins &&
    cargo build --release
)
cp $(pwd)/crypto/rust-bins/target/release/{client,genesis_tool,generate_testdata} /binaries/bin/


#############################################################################################################################
section "Copy other libraries"
for lib in $(find ~/.cabal/store/ghc-$GHC_VERSION/ -type f -name "*[^_p].a"); do
    cp $lib /target/vanilla/cabal;
    cp $lib /target-sc/vanilla/cabal;
done

for lib in $(find ~/.cabal/store/ghc-$GHC_VERSION/ -type f -name "*_p.a"); do
    cp $lib /target/profiling/cabal;
    cp $lib /target-sc/profiling/cabal;
done

subsection "Cabal libraries: OK"

mkdir -p /{target,target-sc}/rust
cp -r $(pwd)/crypto/rust-src/target/release/*.a /target/rust/

subsection "Rust libraries: OK"

strip --strip-debug /target/vanilla/cabal/libHS* \
            /target/vanilla/concordium/libHS* \
                /target/profiling/cabal/libHS* \
                /target/profiling/concordium/libHS* \
                /target/profiling/concordium/libHS* \
                /target/vanilla/ghc/lib* \
                /target/profiling/ghc/lib* \
                /target-sc/vanilla/cabal/libHS* \
                /target-sc/vanilla/concordium/libHS* \
                /target-sc/profiling/cabal/libHS* \
                /target-sc/profiling/concordium/libHS* \
                /target-sc/profiling/concordium/libHS* \
                /target-sc/vanilla/ghc/lib* \
                /target-sc/profiling/ghc/lib*

strip --strip-debug /binaries/bin/* \
            /binaries/lib/*

subsection "Strip debug symbols: OK"

#############################################################################################################################
section "Remove duplicate objects"
cd /target/rust
for i in $(ls)
do
    ar x $i
done

mkdir crypto
mv *.o crypto
rm *a

set +e

for file in $(find . -type f -name "*.o"); do
  nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)|\(T __clzsi2\)" >> /dev/null;
  if [ $? -eq 0 ]; then
    echo "Removing file:"
    echo $file
    rm $file;
  fi
done

set -e

ar rcs libRcommon.a
ar rcs libRcrypto.a crypto/*.o
rm -r crypto

cp /build/smart-contracts/wasmer-interp/target/release/libwasmer_interp.a /target/rust/libwasmer_interp.a
ar x libwasmer_interp.a

set +e

for file in $(find . -type f -name "*.o"); do
  nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)|\(T __clzsi2\)" >> /dev/null;
  if [ $? -eq 0 ]; then
    echo "Removing file:"
    echo $file
    rm $file;
  fi
done

set -e

rm libwasmer_interp.a
ar rcs libwasmer_interp.a *.o

cp -r /target/rust/* /target-sc/rust/

cd /build

#############################################################################################################################
section "Done!"

tar czf static-consensus-$GHC_VERSION.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION.tar.gz /binaries
rm -rf /target
mv /target-sc /target
tar czf static-consensus-$GHC_VERSION-sc.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION-sc.tar.gz /binaries

rm -rf /target /binaries
