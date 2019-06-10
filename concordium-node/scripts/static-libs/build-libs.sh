#!/bin/bash

rootdir=$(pwd)

## Prepare our stuff for cabal
cp cabal.project           ../../deps/internal/consensus
cp cabal.project.local     ../../deps/internal/consensus
cp acorn.cabal             ../../deps/internal/consensus/acorn
cp Concordium.cabal        ../../deps/internal/consensus/Concordium
cp concordium-crypto.cabal ../../deps/internal/consensus/concordium-crypto
cp globalstate-types.cabal ../../deps/internal/consensus/globalstate-mockup/globalstate-types
cp globalstate.cabal       ../../deps/internal/consensus/globalstate-mockup/globalstate
cp scheduler.cabal         ../../deps/internal/consensus/scheduler

if [ -f "/etc/arch-release" ]; then
    echo "It seems you are running Arch, I will install some things"
    ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5
    pacman -Sy
    pacman -S base-devel python3 wget tar cmake protobuf unbound numactl --needed
fi

command -v cargo
if [ $? -eq 0 ]; then
    echo "OK, so you already have one cargo"
else
    echo "No cargo found. Installing..."
    curl -f https://sh.rustup.rs | sh
    source $HOME/.cargo/env
fi

## Get the ghc source code
wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-src.tar.xz
tar xf ghc-8.6.5-src.tar.xz
rm ghc-8.6.5-src.tar.xz

## Do you already have a GHC?
command -v ghc
if [ $? -eq 0 ]; then
    echo "OK, so you already have one GHC"
else
    echo "GHC not installed, going to install (This will take a while)"
    mkdir aux
    cd aux
    wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb9-linux.tar.xz
    tar xf ghc-8.6.5-x86_64-deb9-linux.tar.xz
    cd ghc-8.6.5/
    ./configure
    make -j8
    sudo make install
    ghc --version
    if [ $? -eq 0 ]; then
        echo "OK, GHC was installed"
    else
        echo "Couldn't install ghc, please check the log and rerun"
        exit 1
    fi
    cd ../../
    rm -rf aux
fi

## Build the fPIC ghc
echo "Build the boot libraries and runtime (This will take some time (a lot))"
cp $rootdir/build.mk $rootdir/ghc-8.6.5/mk/build.mk
cd $rootdir/ghc-8.6.5
./boot
./configure
sed -i 's/CFLAGS="/&-fPIC -g -fstack-protector-all /' $rootdir/ghc-8.6.5/libffi/ghc.mk
make -j8
sudo make install

# Copy the needed libraries
echo "Let's copy the needed boot libraries"
(rm -rf $rootdir/target
 mkdir -p $rootdir/target/{profiling,vanilla}/{ghc,cabal,concordium}

for lib in $(find $rootdir/ghc-8.6.5 -type f -name "*_p.a"); do
    cp $lib $rootdir/target/profiling/ghc
done

for lib in $rootdir/ghc-8.6.5/rts/dist/build/libCffi_thr.a \
               $rootdir/ghc-8.6.5/rts/dist/build/libHSrts_thr.a \
               $(find $rootdir/ghc-8.6.5/libraries -type f -name "*[^_p].a" | grep dist-install); do
    cp $lib $rootdir/target/vanilla/ghc
done

for l in $rootdir/target/profiling/ghc/libHSrts_p.a \
             $rootdir/target/profiling/ghc/libCffi_p.a \
             $rootdir/target/vanilla/ghc/libCffi.a \
             $rootdir/target/vanilla/ghc/libHSrts.a \
             $rootdir/target/vanilla/ghc/libHSCabal-2.4.0.1.a \
             $rootdir/target/vanilla/ghc/libHSghc-8.6.5.a \
             $rootdir/target/vanilla/ghc/libHSghc-boot-8.6.5.a \
             $rootdir/target/vanilla/ghc/libHSghc-heap-8.6.5.a \
             $rootdir/target/vanilla/ghc/libHSghci-8.6.5.a \
             $rootdir/target/vanilla/ghc/libHShpc-0.6.0.3.a \
             $rootdir/target/vanilla/ghc/libHSterminfo-0.4.1.2.a \
             $(find $rootdir/target/vanilla/ghc -name "libffi*") \
             $(find $rootdir/target/vanilla/ghc -name "*[debug|l].a"); do
    rm $l;
done)

# Install cabal if needed

(command -v cabal
if [ $? -eq 0 ]; then
    echo "OK, Cabal already installed"
else
    echo "Cabal not installed"
    cd $rootdir/ghc-8.6.5/libraries/Cabal/cabal-install
    ./bootstrap.sh --no-doc
    if [ $(whoami) == "root" ]; then
        export PATH=$PATH:/root/.cabal/bin
    else
        export PATH=$PATH:/home/$(whoami)/.cabal/bin
    fi
    cabal --version
    if [ $? -eq 0 ]; then
        echo "OK, Cabal has been installed"
    else
        echo "Couldn't install Cabal, please check the log and rerun"
        exit 1
    fi
fi
rm -rf $rootdir/ghc-8.6.5)
cabal new-update

# Build our stuff
(rm -rf ~/.cabal/store/ghc-8.6.5/
    cd $rootdir/../../deps/internal/consensus
LD_LIBRARY_PATH=$rootdir/../../deps/internal/consensus/crypto/rust-src/target/release cabal new-build all

# Copy our libraries
echo "Let's copy the needed concordium libraries"
for lib in $(find . -type f -name "*inplace.a"); do
    cp $(pwd)/$lib $rootdir/target/vanilla/concordium;
done

for lib in $(find . -type f -name "*_p.a"); do
    cp $(pwd)/$lib $rootdir/target/profiling/concordium;
done

echo "Let's copy the needed cabal libraries"
for lib in $(find ~/.cabal/store/ghc-8.6.5/ -type f -name "*[^_p].a"); do
    cp $lib $rootdir/target/vanilla/cabal;
done

for lib in $(find ~/.cabal/store/ghc-8.6.5/ -type f -name "*_p.a"); do
    cp $lib $rootdir/target/profiling/cabal;
done)

# Copy the rust libraries
(echo "Let's copy the needed rust libraries"
 cd $rootdir/../../deps/internal/consensus/crypto/rust-src/target/release
 mkdir $rootdir/target/rust
 cp *.a $rootdir/target/rust
 cd $rootdir/target/rust

 rustlibs=$(find . -type f)
 for lib in $rustlibs; do
     ar x $lib;
 done

 echo "Removing duplicated symbols"
 for file in $(find . -type f -name "*.o"); do
     nm $file | grep "\(T __rust_alloc\)\|\(T __rdl_alloc\)" >> /dev/null;
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

echo "Done! In the target directory you will find all the libraries. Copy mv all the subdirs to p2p-client/deps/static-libs/"
