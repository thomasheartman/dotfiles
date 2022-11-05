# initial version based on rebeccaskinner's config:
# https://github.com/rebeccaskinner/nix-config/blob/6c440b2d64cfc1390c8efa457519934d34245c85/collections/games/dungeondraft/default.nix
#
# File system interactions didn't seem to work.
#
# version 2 was adapted to follow ixahedron's config:
# https://github.com/ixahedron/nixos-config/blob/b3875ab548460855616b2c481cdd788b4707d8e9/xserver/dungeondraft/default.nix


{ pkgs, lib, ... }:

let
  name = "dungeondraft";
  version = "1.0.4.6";
  path = lib.makeBinPath [ pkgs.gnome.zenity pkgs.gdb ];

in

pkgs.stdenv.mkDerivation {
  inherit name version;
  src = ./Dungeondraft-${version}-Linux64.zip;

  nativeBuildInputs = [
    pkgs.autoPatchelfHook
    pkgs.unzip
  ];

  buildInputs = with pkgs; [
    alsa-lib
    libGL
    libpulseaudio
    stdenv.cc.cc.lib
    xorg.libX11
    xorg.libXcursor
    xorg.libXi
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXrender
    zlib

    makeWrapper

    krb5
  ];
  unpackCmd = "unzip $curSrc -d ./dungeondraft";
  sourceRoot = "dungeondraft";

  installPhase =
    ''
      name=${name}

      mkdir -p $out/bin
      mkdir -p $out/share/applications

      chmod +x Dungeondraft.x86_64

      substituteInPlace ./Dungeondraft.desktop \
        --replace '/opt/Dungeondraft/Dungeondraft.x86_64' "$out/bin/$name" \
        --replace '/opt/Dungeondraft' $out \
        --replace '/opt/Dungeondraft/Dungeondraft.png' "$out/Dungeondraft.png"

      mv ./Dungeondraft.desktop $out/share/applications/
      mv ./Dungeondraft.x86_64 "$out/$name"
      mv ./Dungeondraft.pck $out/$name.pck

      makeWrapper $out/$name $out/bin/$name \
          --prefix PATH : ${path}

      mv ./* $out
    '';
}
