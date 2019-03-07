#!/bin/sh
git clone https://github.com/mitls/hacl-c
( cd hacl-c && make -j$(nproc) && cp libhacl.so /usr/lib );
rm -rf hacl-c

git clone https://github.com/libffi/libffi.git
( cd libffi && ./autogen.sh && ./configure && make -j$(nproc) && make install);
rm -rf libffi

(mkdir -p ~/.stack/global-project/ && cp scripts/stack.yaml ~/.stack/global-project/stack.yaml)

curl -sSL https://get.haskellstack.org/ | sh
( cd consensus && stack build --ghc-options '-dynamic -j4' --force-dirty &&
  cp .stack-work/install/x86_64-linux-tinfo6/lts-12.20/8.4.4/lib/x86_64-linux-ghc-8.4.4/libHS*.so /usr/local/lib &&
  find /usr/local/lib -name libHSConcordium\*.so -exec ln -s {} /usr/local/lib/libHSConcordium-0.1.0.0.so \; &&
  find /usr/local/lib -name libHSlanguage-glsl-0.3.0-\*.so -exec ln -s {} /usr/local/lib/libHSlanguage-glsl-0.3.0.so \; &&
  find /usr/local/lib -name libHSoak-0.19.0-\*.so -exec ln -s {} /usr/local/lib/libHSoak-0.19.0.so \; &&
  find /usr/local/lib -name libHSmonadplus-1.3\*.so -exec ln -s {} /usr/local/lib/libHSmonadplus-1.3.so \; &&
  find ~/.stack/programs -name libHS\*-\*.so -exec cp {} /usr/local/lib \; &&
  ls /usr/local/lib &&
  ls ~/.stack/programs/x86_64-linux/
  ) 

(cp -r consensus/workdir consensus-sys/)

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

