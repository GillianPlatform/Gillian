{ pkgs ? import <nixpkgs> {} }:

let
  # Gillian-C2 needs the CBMC version CI installs (6.9.0, see .github/workflows/ci.yml)
  cbmcPkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/a033e66ab94a6bedf2c3d58d93a36319d3cbbbaf.tar.gz";
    sha256 = "140gms89455qvs29g7zwnx66z0h9ycf4hvfg7lh84rqi79sh7yfa";
  }) {};
in
pkgs.mkShell rec {
  packages = with pkgs; [
    opam
    gnumake
    z3
    cbmcPkgs.cbmc
  ];
  shellHook = ''
    make githooks > /dev/null
    eval $(opam env)
  '';
}
