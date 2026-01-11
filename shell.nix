{ pkgs ? import <nixpkgs> { config.allowUnfree = true; } }:
let
  h = pkgs.haskellPackages;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    h.ghc
    h.cabal-install
    h.haskell-language-server
    ormolu
    hlint
    pkg-config
    zlib
  ];
}
