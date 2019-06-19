#!/bin/bash
pacman -Sy
pacman -Syyu --noconfirm
pacman -S wget tar make m4 pkgconf autoconf automake grep python clang libtool ncurses which rustup binutils --noconfirm
ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5

wget https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/fpic-ghc-$GHC_VERSION.tar.gz

tar -xf fpic-ghc-$GHC_VERSION.tar.gz
cp -r bootstrapped_out/* /
rm -r bootstrapped_out

cp /manifests/cabal.project           /build
cp /manifests/cabal.project.local     /build
cp /manifests/acorn.cabal             /build/acorn
cp /manifests/Concordium.cabal        /build/Concordium
cp /manifests/concordium-crypto.cabal /build/concordium-crypto
cp /manifests/globalstate-types.cabal /build/globalstate-mockup/globalstate-types
cp /manifests/globalstate.cabal       /build/globalstate-mockup/globalstate
cp /manifests/scheduler.cabal         /build/scheduler

rustup default stable

mkdir -p /target/{profiling,vanilla}/{ghc,cabal,concordium}
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
             /target/vanilla/ghc/libHSghc-8.6.5.a \
             /target/vanilla/ghc/libHSghc-boot-8.6.5.a \
             /target/vanilla/ghc/libHSghc-heap-8.6.5.a \
             /target/vanilla/ghc/libHSghci-8.6.5.a \
             /target/vanilla/ghc/libHShpc-0.6.0.3.a \
             /target/vanilla/ghc/libHSterminfo-0.4.1.2.a \
             $(find /target/vanilla/ghc -name "libffi*") \
             $(find /target/vanilla/ghc -name "*[debug|l].a"); do
    rm $l;
done

wget http://hackage.haskell.org/package/cabal-install-2.4.1.0/cabal-install-2.4.1.0.tar.gz
tar -xf cabal-install-2.4.1.0.tar.gz
cd cabal-install-2.4.1.0

./bootstrap.sh --no-doc
export PATH=$PATH:$HOME/.cabal/bin


cd ..
rm -rf cabal-install-2.4.1.0 cabal-install-2.4.1.0.tar.gz
cabal new-update

cd /build

LD_LIBRARY_PATH=$(pwd)/crypto/rust-src/target/release cabal new-build all

echo "Let's copy the needed concordium libraries"
for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib /target/vanilla/concordium;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib /target/profiling/concordium;
done

echo "Let's copy the needed cabal libraries"
for lib in $(find ~/.cabal/store/ghc-8.6.5/ -type f -name "*[^_p].a"); do
    cp $lib /target/vanilla/cabal;
done

for lib in $(find ~/.cabal/store/ghc-8.6.5/ -type f -name "*_p.a"); do
    cp $lib /target/profiling/cabal;
done

mkdir -p /target/rust
cp -r $(pwd)/crypto/rust-src/target/release/*.a /target/rust/

cd /target/rust
for i in $(ls)
do
ar x $i
done

echo "Removing duplicated symbols"
for file in $(find . -type f -name "*.o"); do
  nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)|\(T __clzsi2\)" >> /dev/null;
  if [ $? -eq 0 ]; then
    echo "Removing file:"
    echo $file
    rm $file;
  fi
done


ar rcs libRcrypto.a *.o

rm *.o

cd /build

echo "Done!"

tar czf static-consensus-$GHC_VERSION.tar.gz /target
mv static-consensus-$GHC_VERSION.tar.gz /out 
