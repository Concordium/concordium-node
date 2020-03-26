#!/usr/bin/env bash

set -e
GHC_BUILDER_VERSION="8.6.5"
CABAL_BUILDER_VERSION="3.0.0.0"
GHC_VERSION="8.6.5"
STACK_VERSION="2.1.3"
pacman -Sy
pacman -S reflector --noconfirm
reflector --latest 20 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist
pacman -S wget tar make m4 pkgconf autoconf automake grep python clang libtool ncurses which rustup binutils git postgresql-libs --noconfirm
ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

# Compile lmdb
git clone https://github.com/LMDB/lmdb
(
    cd lmdb/libraries/liblmdb
    make install
    mv /usr/local/lib/liblmdb* /usr/lib/
    mv /usr/local/include/lmdb.h /usr/include/
)


wget https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/fpic-ghc-$GHC_VERSION.tar.gz

tar -xf fpic-ghc-$GHC_VERSION.tar.gz
cp -r bootstrapped_out/* /
rm -r bootstrapped_out

cp /manifests/cabal.project           /build
cp /manifests/cabal.project.local     /build

rustup set profile minimal
rustup default 1.42.0

rm -rf $HOME/.cargo

sed -i 's/git-fetch-with-cli = true/git-fetch-with-cli = false/' /build/crypto/rust-src/.cargo/config

(
    cd /build/crypto/rust-src &&
        cargo update &&
        cargo check
)

mkdir -p /target/{profiling,vanilla}/{ghc,cabal,concordium}
mkdir -p /binaries/{lib,bin}
for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*_p.a"); do
    cp $lib /target/profiling/ghc/
done

for lib in $(find /usr/local/lib/ghc-$GHC_VERSION -type f -name "*[^_p].a"); do
    cp $lib /target/vanilla/ghc/
done

for l in /target/profiling/ghc/libHSrts_p.a \
             /target/profiling/ghc/libCffi_p.a \
             /target/vanilla/ghc/libCffi.a \
             /target/vanilla/ghc/libHSrts.a \
             /target/vanilla/ghc/libHSCabal-2.4.0.1.a \
             /target/vanilla/ghc/libHSghc-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghc-boot-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghc-heap-$GHC_VERSION.a \
             /target/vanilla/ghc/libHSghci-$GHC_VERSION.a \
             /target/vanilla/ghc/libHShpc-0.6.0.3.a \
             /target/vanilla/ghc/libHSterminfo-0.4.1.2.a \
             $(find /target/vanilla/ghc -name "libffi*") \
             $(find /target/vanilla/ghc -name "*[debug|l].a"); do
    rm $l;
done

wget https://downloads.haskell.org/~cabal/cabal-install-$CABAL_BUILDER_VERSION/cabal-install-$CABAL_BUILDER_VERSION-x86_64-unknown-linux.tar.xz
tar -xf cabal-install-$CABAL_BUILDER_VERSION-x86_64-unknown-linux.tar.xz
mkdir -p $HOME/.cabal/bin
chmod +x cabal
mv cabal $HOME/.cabal/bin/
export PATH=$PATH:$HOME/.cabal/bin
cabal update

wget https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64-static.tar.gz
tar -xf stack-$STACK_VERSION-linux-x86_64-static.tar.gz
mkdir -p $HOME/.stack/bin
mv stack-$STACK_VERSION-linux-x86_64-static/stack $HOME/.stack/bin
export PATH=$PATH:$HOME/.stack/bin
echo "system-ghc: true" > ~/.stack/config.yaml
stack update

for f in $(find /build -type f -name package.yaml); do
   sed -i -e 's/[\s]*ld-options://g' -e 's/[\s]*- -static//g' $f
done

cd /build

stack ls dependencies > /dev/null # to generate the cabal files

rm *.freeze
cabal freeze
while IFS= read -r line
do
    p=$(echo "$line" | cut -d' ' -f1)
    c=$(echo "$line" | cut -d' ' -f2)
    sed -i "s/any.$p ==.*,/any.$p ==$c,/g" cabal.project.freeze
done <<< $(stack ls dependencies)
sed -i 's/Concordium +dynamic/Concordium -dynamic/g' cabal.project.freeze


LD_LIBRARY_PATH=$(pwd)/crypto/rust-src/target/release cabal build all

echo "Let's copy the binaries and their dependent libraries"
cp dist-newstyle/build/x86_64-linux/ghc-$GHC_BUILDER_VERSION/Concordium-0.1.0.0/x/genesis/build/genesis/genesis /binaries/bin/
cp $(pwd)/crypto/rust-src/target/release/*.so /binaries/lib/

echo "Build the rust utility binaries"
(
    cd crypto/rust-bins &&
    cargo build --release
)
cp $(pwd)/crypto/rust-bins/target/release/{client,genesis_tool,generate_testdata,server,wallet_server} /binaries/bin/

echo "Let's copy the needed concordium libraries"
for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib /target/vanilla/concordium;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib /target/profiling/concordium;
done

echo "Let's copy the needed cabal libraries"
for lib in $(find ~/.cabal/store/ghc-$GHC_VERSION/ -type f -name "*[^_p].a"); do
    cp $lib /target/vanilla/cabal;
done

for lib in $(find ~/.cabal/store/ghc-$GHC_VERSION/ -type f -name "*_p.a"); do
    cp $lib /target/profiling/cabal;
done

mkdir -p /target/rust
cp -r $(pwd)/crypto/rust-src/target/release/*.a /target/rust/

echo "Removing debug symbols because certain distros can't update their stuff to be compliant with the spec"
strip --strip-debug /target/vanilla/cabal/libHS* \
            /target/vanilla/concordium/libHS* \
                /target/profiling/cabal/libHS* \
                /target/profiling/concordium/libHS* \
                /target/vanilla/ghc/lib* \
                /target/profiling/ghc/lib*

strip --strip-debug /binaries/bin/* \
            /binaries/lib/*

echo "Removing object files"
echo "Expanding libraries"
cd /target/rust
for i in $(ls)
do
    ar x $i
done

mkdir crypto
mv *.o crypto

rm *a

set +e

echo "Removing objects with standard symbols that collide with any other rust instance"
for file in $(find . -type f -name "*.o"); do
  nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)|\(T __clzsi2\)" >> /dev/null;
  if [ $? -eq 0 ]; then
    echo "Removing file:"
    echo $file
    rm $file;
  fi
done

set -e

echo "Unifying duplicated objects that collide between both libraries"
ar rcs libRcommon.a

echo "Recreating the libraries"
ar rcs libRcrypto.a crypto/*.o

rm -r crypto

cd /build

echo "Done!"

tar czf static-consensus-$GHC_VERSION.tar.gz /target
tar czf static-consensus-binaries-$GHC_VERSION.tar.gz /binaries

rm -rf /target /binaries
