#!/bin/sh
patch grpc-rs-0.4.0/grpc-sys/build.rs scripts/zlib-mingw.patch

rm consensus-sys/Cargo.toml
cp consensus-sys/Cargo.toml.win consensus-sys/Cargo.toml
rm consensus-sys/build.rs
cp consensus-sys/build.rs.win consensus-sys/build.rs
rm consensus-sys/curryrs/build.rs
cp consensus-sys/curryrs/build.rs.win consensus-sys/curryrs/build.rs

stack exec -- ghc --print-libdir
cp -r consensus-sys/lib/* /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4/
cabal exec -- ghc --print-libdir
ls -lah /root/.stack/programs/x86_64-linux/ghc-8.4.4/lib/ghc-8.4.4
