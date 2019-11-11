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
  nixpkgs = import <nixpkgs> { overlays = [ pkgs_overlay moz_overlay ]; };
  rustStableChannel =
    (nixpkgs.rustChannelOf { channel = "1.39.0"; }).rust.override {
      extensions =
        [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
    };
in with nixpkgs;
stdenv.mkDerivation {
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
    capnproto
    flatbuffers
  ];
  shellHook = ''
    scripts/download-static-libs.sh
  '';
}
