{ pkgs ? import <nixpkgs> {
  overlays = [
    (import (builtins.fetchTarball
      "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz"))
  ];
} }:
with pkgs;
let

  rust = pkgs.latest.rustChannels.stable.rust;

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cargo-watch
    rust-analyzer # rust-analyzer must come before `rust` so it gets
                  # put first in the path, thereby overriding the
                  # version that is packaged with `rust`. We do this
                  # because the version that comes with `rust` is old.
    rust
    cargo-edit
  ];
}
