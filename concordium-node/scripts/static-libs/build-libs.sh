#!/bin/bash

GHCVER=8.6.5

# This script assumes you have:
# - GHC with fPIC (see build-deps.sh)
# - Cabal with the path to binaries already in your $PATH (see build-deps.sh)
# - Cargo
# - hpack 0.31.2 (cabal new-install hpack & move the bin somewhere else)

rootdir=$(pwd)
consensus_dir=$rootdir/../../deps/internal/consensus

## Prepare our stuff for cabal
cp cabal.project           $consensus_dir
cp cabal.project.local     $consensus_dir
(cd $consensus_dir/acorn
 hpack
 cd $consensus_dir/Concordium
 hpack
 cd $consensus_dir/globalstate-mockup/globalstate
 hpack
 cd $consensus_dir/globalstate-mockup/globalstate-types
 hpack
 cd $consensus_dir/scheduler
 hpack)

(cd
cabal new-update)

# Build our stuff
(rm -rf ~/.cabal/store/ghc-$GHCVER/
    cd $consensus_dir
LD_LIBRARY_PATH=$rootdir/../../deps/internal/consensus/crypto/rust-src/target/release cabal new-build all --flags="-dynamic"

rm -rf $rootdir/target
mkdir -p $rootdir/target/{profiling,vanilla}/{cabal,concordium}
mkdir -p $rootdir/target/rust

# Copy our libraries
echo "Let's copy the needed concordium libraries"
for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib $rootdir/target/vanilla/concordium;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib $rootdir/target/profiling/concordium;
done

echo "Let's copy the needed vendor libraries"
for lib in $(find ~/.cabal/store/ghc-$GHCVER/ -type f -name "*[^_p].a"); do
    cp $lib $rootdir/target/vanilla/cabal;
done

for lib in $(find ~/.cabal/store/ghc-$GHCVER/ -type f -name "*_p.a"); do
    cp $lib $rootdir/target/profiling/cabal;
done)

# Copy the rust libraries
(echo "Let's copy the needed rust libraries"
 cd $rootdir/../../deps/internal/consensus/crypto/rust-src/target/release
 cp *.a $rootdir/target/rust
 cd $rootdir/target/rust

 rustlibs=$(find . -type f)
 for lib in $rustlibs; do
     ar x $lib;
 done

 echo "Removing duplicated symbols"
 for file in $(find . -type f -name "*.o"); do
     nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)\|\(T __clzsi2\)" >> /dev/null;
     if [ $? -eq 0 ]; then
         echo "Removing file:"
         echo $file
         rm $file;
     fi
 done

 ar rcs libRcrypto.a *.o
 for lib in $rustlibs; do
     rm $lib;
 done
 rm *.o)

strip --strip-debug $rootdir/target/vanilla/cabal/libHS* $rootdir/target/vanilla/concordium/libHS* $rootdir/target/profiling/cabal/libHS* $rootdir/target/profiling/concordium/libHS*

echo "Done! In the target directory you will find all the libraries. Copy/mv all the subdirs to p2p-client/deps/static-libs/"
