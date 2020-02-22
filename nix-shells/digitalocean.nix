{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  kc = kubectl;
  dc = doctl;

  runKc = writeShellScriptBin "kc" "kubectl $@";
  runDc = writeShellScriptBin "dc" "doctl $@";
in stdenv.mkDerivation {
  name = "digital_ocean_k8s";
  buildInputs = [ kc dc runKc runDc ];
}
