# based on rebeccaskinner's config:
# https://github.com/rebeccaskinner/nix-config/blob/6c440b2d64cfc1390c8efa457519934d34245c85/collections/games/dungeondraft/default.nix
{ pkgs, ... }:

let

  fetchDungeondraft = { inputFile, fileSha, shaAlgo ? "sha256" }:
    pkgs.runCommand "unpackDungeondraft"
      {
        name = "Dungeondraft.zip";
        outputHashMode = "flat";
        outputHashAlgo = shaAlgo;
        outputHash = fileSha;
        nativeBuildInputs = [ pkgs.unzip ];
      } ''
      cp ${inputFile} $out
    '';

  v1_0_4_6 = {
    inputFile = ./Dungeondraft-1.0.4.6-Linux64.zip;
    fileSha = "sha256-DT44ii0rMeXdUoSvKxvR9Mir8dQswQ8f4L3MD8sxFxo=";
  };

  v1_0_3_2 = {
    inputFile = ./Dungeondraft-1.0.3.2-Linux64.zip;
    fileSha = "sha256-1ixZ98E27H8fdbACV0QwjUn7QxI/RSFw1jA+MUssQYs=";
  };


  dungeondraft = pkgs.stdenv.mkDerivation {
    name = "Dungeondraft";
    src = fetchDungeondraft v1_0_4_6;
    nativeBuildInputs = [
      pkgs.unzip
      pkgs.autoPatchelfHook
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

      krb5
    ];
    sourceRoot = ".";
    installPhase =
      ''
        target=$out/opt/Dungeondraft
        mkdir -p $target
        mkdir $out/bin
        mkdir -p $out/share/applications
        chmod +x Dungeondraft.x86_64
        cp -av data_Dungeondraft $target/
        cp -av Dungeondraft.x86_64 $target/
        cp -av Dungeondraft.pck $target/
        ln -sf $target/Dungeondraft.x86_64 $out/bin/dungeondraft
        substitute ./Dungeondraft.desktop $out/share/applications/Dungeondraft.desktop --replace "/opt/Dungeondraft" "$out/opt/Dungeondraft"
      '';
  };

in
dungeondraft
