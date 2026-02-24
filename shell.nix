{ nixpkgs ? import <nixpkgs> {} }:

let
  pkgs = nixpkgs;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # GHC and cabal
    haskell.compiler.ghc910
    cabal-install

    # Dev tools
    ghcid
    haskell-language-server
    hlint

    # Required for building some Haskell packages
    pkg-config
    zlib
  ];

  # Tell cabal where to find zlib
  LD_LIBRARY_PATH = "${pkgs.zlib}/lib";
}
