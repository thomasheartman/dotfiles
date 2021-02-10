{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.nixfmt
    pkgs.rnix-lsp
  ];
}
