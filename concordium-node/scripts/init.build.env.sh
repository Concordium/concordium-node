#!/bin/sh
git clone https://github.com/mitls/hacl-c
( cd hacl-c && make -j$(nproc) && cp libhacl.so /usr/lib );
rm -rf hacl-c

git clone https://github.com/libffi/libffi.git
( cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install);
rm -rf libffi

(mkdir -p ~/.stack/global-project/ && echo -e "packages: []\nresolver: $(cat deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml)

curl -sSL https://get.haskellstack.org/ | sh
( cd deps/internal/consensus && stack build --ghc-options '-dynamic -j4' --force-dirty &&
  cp .stack-work/install/x86_64-linux-tinfo6/$(cat stack.yaml | grep ^resolver: | awk '{ print $NF }')/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so /usr/local/lib &&
  find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSacorn\*.so -exec ln -s {} /usr/local/lib/libHSacorn-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSconcordium-crypto\*.so -exec ln -s {} /usr/local/lib/libHSconcordium-crypto-0.1.so \; &&
  find ~/.stack/programs -name libHS\*-\*.so -exec cp {} /usr/local/lib \; 
  ) 

git clone https://github.com/KDE/heaptrack.git
(cd heaptrack && patch src/track/heaptrack.sh.cmake ../scripts/include-date-in-name.patch && mkdir build && cd build && cmake -DCMAKE_BUILD_TYPE=release .. && make -j$(nproc) && make install);
rm -rf heaptrack

(mkdir -p deps/internal/crypto/build && 
    cd deps/internal/crypto/build && 
    cmake -DCMAKE_BUILD_TYPE=Release .. && 
    cmake --build . && 
    cmake --build . --target install
)

ldconfig
# 20190123 - Moved from 2018-10-26 to latest nightly after allocator fixes in nightly
rustup default nightly

