let
  # 'stack --nix' will look for a GHC in the user channels
  # we don't want a GHC from an unknown user channel,
  # but a GHC from a known, build environment-specific channel
  # thus, please use 'stack --no-nix --system-ghc' instead
  pkgs2105 = import (builtins.fetchTarball "channel:nixos-21.05") {};
  pkgs2111 = import (builtins.fetchTarball "channel:nixos-21.11") {};
  ghc = pkgs2105.haskell.packages.ghc8104.ghcWithPackages (p: with p; [ stack haskell-language-server ]);
in
pkgs2105.mkShell {

  nativeBuildInputs = with pkgs2105; [ ghc cargo ];

  buildInputs = with pkgs2105; [ postgresql unbound lmdb pkgs2111.flatbuffers protobuf zlib ];

  PROTOC = "${pkgs2105.protobuf}/bin/protoc";
  
}
