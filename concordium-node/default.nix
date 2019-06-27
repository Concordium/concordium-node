{ pkgs ? import <nixpkgs> { } }:

let
  moz_overlay = import (builtins.fetchTarball
  "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustStableChannel =
  (nixpkgs.rustChannelOf { channel = "stable"; }).rust.override {
    extensions =
    [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
  };
in with pkgs;

let
  rustPlatform = pkgs.makeRustPlatform {
    cargo = rustStableChannel;
    rustc = rustStableChannel;
  };

in rustPlatform.buildRustPackage rec {
  name = "concordium-p2p-client-${version}";
  version = "0.1.35.1";
  src = ./.;
  RUST_BACKTRACE = 1;
  hardeningDisable = [ "all" ];
  cargoBuildFlags = [ "--features=static" ];
  buildInputs = with pkgs; [
    pkgconfig
    openssl
    cmake
    protobuf
    gmp
    numactl
    perl
    unbound
    gcc
  ];
  cargoSha256 = "06wzhgsb9k0q54x0myqzbjg1lhpfy5vgj0l61szmpy7z3p7z9mf7";
  meta = with pkgs.stdenv.lib; {
    description = "Concordium AG";
    homepage = "https://www.concordium.com";
    license = licenses.mit;
  };
  doCheck = false;
}
