{ pkgs ? import <nixpkgs> { } }:

let
  moz_overlay = import (builtins.fetchTarball
  "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustStableChannel =
  (nixpkgs.rustChannelOf { channel = "1.36.0"; }).rust.override {
    extensions =
    [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
  };
in with nixpkgs;
stdenv.mkDerivation {
  name = "concordium_shell";
  hardeningDisable = [ "all" ];
  buildInputs = [
    rustStableChannel
    openssl
    protobuf
    pkgconfig
    unbound
    numactl
    gmp
    cmake
    curl
    gnutar
    capnproto
  ];
  shellHook = ''
    export OPENSSL_DIR="${openssl.dev}"
    export OPENSSL_LIB_DIR="${openssl.out}/lib"
    scripts/download-static-libs.sh
  '';
}
