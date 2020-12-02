{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "haskell-env";
  buildInputs = [
    ghcid ghc
  ];
}
