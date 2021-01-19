{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc883" }:
let
  inherit (nixpkgs) pkgs;
  hpkgs = pkgs.haskell.packages.${compiler};

  ghc = hpkgs.ghcWithPackages (ps: with ps; [
          aeson Chart Chart-diagrams
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-env";
  buildInputs = with hpkgs; [
    ghc
    cabal-install
    ghcid
  ];
}
