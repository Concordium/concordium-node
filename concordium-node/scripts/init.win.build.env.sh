#!/bin/sh

# Checkout grpc source
(cd deps/windows/grpc-rs-0.4.0/grpc-sys && git clone https://github.com/pingcap/grpc.git && cd grpc && git checkout 1de249c && git submodule update --init --recursive && cd third_party/boringssl && git checkout master && git pull)

cp scripts/CXXFeatureCheck.cmake deps/windows/grpc-rs-0.4.0/grpc-sys/grpc/third_party/benchmark/cmake/
cp scripts/CMakeLists.txt deps/windows/grpc-rs-0.4.0/grpc-sys/grpc/

(mkdir -p ~/.stack/global-project/ && cp scripts/stack.yaml ~/.stack/global-project/stack.yaml)

stack exec -- ghc --print-libdir
cp -r consensus-sys/lib/* /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4/
cabal exec -- ghc --print-libdir
ls -lah /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4
