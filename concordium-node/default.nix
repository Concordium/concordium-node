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
  version = "0.1.35.2";
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
  cargoSha256 = "10m0xsidhr4qlqyj34xbf7476vblgrrmgjnxk8rh21zp69a8fqhj";
  meta = with pkgs.stdenv.lib; {
    description = "Concordium AG";
    homepage = "https://www.concordium.com";
    license = licenses.mit;
  };
  doCheck = false;
}
