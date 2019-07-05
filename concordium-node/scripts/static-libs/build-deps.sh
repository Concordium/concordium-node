#!/bin/bash

GHCVER=8.6.5

## Prepare our stuff for cabal
if [ -f "/etc/arch-release" ]; then
    echo "It seems you are running Arch, I will install some things"
    ln -s /usr/lib/libtinfo.so.6 /usr/lib/libtinfo.so.5
    pacman -Sy
    pacman -S base-devel python3 wget tar cmake protobuf unbound numactl --needed
fi

command -v cargo
if [ $? -eq 0 ]; then
    echo "OK, so you already have Cargo"
else
    echo "Cargo not found. Installing..."
    curl -f https://sh.rustup.rs | sh
    source $HOME/.cargo/env
fi

## Get the ghc source code
wget https://downloads.haskell.org/~ghc/$GHCVER/ghc-$GHCVER-src.tar.xz
tar xf ghc-$GHCVER-src.tar.xz
rm ghc-$GHCVER-src.tar.xz

## Do you already have a GHC?
command -v ghc
if [ $? -eq 0 ]; then
    echo "OK, so you already have one GHC"
else
    echo "GHC not installed, going to install (This will take a while)"
    mkdir aux
    cd aux
    wget https://downloads.haskell.org/~ghc/$GHCVER/ghc-$GHCVER-x86_64-deb9-linux.tar.xz
    tar xf ghc-$GHCVER-x86_64-deb9-linux.tar.xz
    cd ghc-$GHCVER/
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
cp $rootdir/build.mk $rootdir/ghc-$GHCVER/mk/build.mk
cd $rootdir/ghc-$GHCVER
./boot
./configure
sed -i 's/CFLAGS="/&-fPIC -g -fstack-protector-all /' $rootdir/ghc-$GHCVER/libffi/ghc.mk
make -j8
sudo make install

# Copy the needed libraries
echo "Let's copy the needed boot libraries"
(rm -rf $rootdir/target
 mkdir -p $rootdir/target/{profiling,vanilla}/{ghc,cabal,concordium}

for lib in $(find $rootdir/ghc-$GHCVER -type f -name "*_p.a"); do
    cp $lib $rootdir/target/profiling/ghc
done

for lib in $rootdir/ghc-$GHCVER/rts/dist/build/libCffi_thr.a \
               $rootdir/ghc-$GHCVER/rts/dist/build/libHSrts_thr.a \
               $(find $rootdir/ghc-$GHCVER/libraries -type f -name "*[^_p].a" | grep dist-install); do
    cp $lib $rootdir/target/vanilla/ghc
done

for l in $rootdir/target/profiling/ghc/libHSrts_p.a \
             $rootdir/target/profiling/ghc/libCffi_p.a \
             $rootdir/target/vanilla/ghc/libCffi.a \
             $rootdir/target/vanilla/ghc/libHSrts.a \
             $rootdir/target/vanilla/ghc/libHSCabal-2.4.0.1.a \
             $rootdir/target/vanilla/ghc/libHSghc-$GHCVER.a \
             $rootdir/target/vanilla/ghc/libHSghc-boot-$GHCVER.a \
             $rootdir/target/vanilla/ghc/libHSghc-heap-$GHCVER.a \
             $rootdir/target/vanilla/ghc/libHSghci-$GHCVER.a \
             $rootdir/target/vanilla/ghc/libHShpc-0.6.0.3.a \
             $rootdir/target/vanilla/ghc/libHSterminfo-0.4.1.2.a \
             $(find $rootdir/target/vanilla/ghc -name "libffi*") \
             $(find $rootdir/target/vanilla/ghc -name "*[debug|l].a"); do
    rm $l;
done

strip --strip-debug $rootdir/target/vanilla/ghc/lib* $rootdir/target/profiling/ghc/lib*
)

# Install cabal if needed

(command -v cabal
if [ $? -eq 0 ]; then
    echo "OK, Cabal already installed"
else
    echo "Cabal not installed"
    cd $rootdir/ghc-$GHCVER/libraries/Cabal/cabal-install
    ./bootstrap.sh --no-doc
    export PATH=$HOME/.cabal/bin
    cabal --version
    if [ $? -eq 0 ]; then
        echo "OK, Cabal has been installed"
    else
        echo "Couldn't install Cabal, please check the log and rerun"
        exit 1
    fi
fi
rm -rf $rootdir/ghc-$GHCVER)
cabal new-update
