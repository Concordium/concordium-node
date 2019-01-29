#!/bin/sh
# TODO - @mkj, adapt to be in deps/windows/grpc-rs-0.4.0 directly applied already in git
patch grpc-rs-0.4.0/grpc-sys/build.rs scripts/zlib-mingw.patch

(mkdir -p ~/.stack/global-project/ && cp scripts/stack.yaml ~/.stack/global-project/stack.yaml)

stack exec -- ghc --print-libdir
cp -r consensus-sys/lib/* /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4/
cabal exec -- ghc --print-libdir
ls -lah /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4
