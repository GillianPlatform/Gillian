{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  packages = with pkgs; [
    opam
    gnumake
    z3
  ];
  shellHook = ''
    make githooks > /dev/null
    eval $(opam env)
  '';
}
