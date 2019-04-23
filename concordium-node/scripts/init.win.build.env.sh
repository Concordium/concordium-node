#!/bin/sh

# Checkout grpc source
(cd deps/windows/grpc-rs-0.4.0/grpc-sys && git clone https://github.com/pingcap/grpc.git && cd grpc && git checkout 1de249c && git submodule update --init --recursive && cd third_party/boringssl && git checkout master && git pull)

cp scripts/CXXFeatureCheck.cmake deps/windows/grpc-rs-0.4.0/grpc-sys/grpc/third_party/benchmark/cmake/
cp scripts/CMakeLists.txt deps/windows/grpc-rs-0.4.0/grpc-sys/grpc/
cp deps/windows/HSdll.dll .
cp deps/windows/libHSdll.dll .

(mkdir -p ~/.stack/global-project/ && echo -e "packages: []\nresolver: $(cat deps/internal/consensus/stack.yaml | grep ^resolver: | awk '{ print $NF }')" > ~/.stack/global-project/stack.yaml)

stack exec -- ghc --print-libdir
