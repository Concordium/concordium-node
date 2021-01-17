{ ghc }:
with (import <nixpkgs> { });

let
  moz_overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustStableChannel =
    (nixpkgs.rustChannelOf { channel = "1.45.2"; }).rust.override {
      extensions =
        [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
    };
in with nixpkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "concordium_shell";
  hardeningDisable = [ "all" ];
  buildInputs = [
    rustStableChannel
    protobuf
    pkgconfig
    unbound
    numactl
    gmp
    cmake
    curl
    gnutar
    postgresql
    zlib
    lmdb
  ];
  PROTOC = "${pkgs.protobuf}/bin/protoc";
}
