{ pkgs ? import <nixpkgs> { } }:

let
  moz_overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  pkgs_overlay = (self: super: {
    flatbuffers = super.flatbuffers.overrideDerivation (old: {
      pname = "flatbuffers";
      version = "1.11.0";
      name = "flatbuffers-1.11.0";
      src = super.fetchFromGitHub {
        owner = "google";
        repo = "flatbuffers";
        rev = "v1.11.0";
        sha256 = "1gl8pnykzifh7pnnvl80f5prmj5ga60dp44inpv9az2k9zaqx3qr";
      };
    });
  });
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay pkgs_overlay ]; };
  rustStableChannel =
    (nixpkgs.rustChannelOf { channel = "1.39.0"; }).rust.override {
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
  version = "0.1.67.0";
  src = ./.;
  RUST_BACKTRACE = 1;
  hardeningDisable = [ "all" ];
  cargoBuildFlags = [ "--features=static" ];
  buildInputs = with pkgs; [
    pkgconfig
    cmake
    protobuf
    gmp
    numactl
    perl
    unbound
    gcc
    flatbuffers
  ];
  cargoSha256 = "1lay053m3vk6lzzm9iac6bmnic0qn9xsi9775hv31a1pcf5m7pa0";
  meta = with pkgs.stdenv.lib; {
    description = "Concordium AG";
    homepage = "https://www.concordium.com";
    license = licenses.mit;
  };
  doCheck = false;
}
