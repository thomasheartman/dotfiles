{ config, pkgs, ... }:
let

  unstable = import (
    fetchTarball
      "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
  ) {};

in

{
  imports = [ ../home.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    unstable.postgresql

    (
      writeScriptBin "pfox" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -P porterbuddy $@
      ''
    )
  ];

  home.file.".gitconfig.work".source = ~/dotfiles/system-config/porterbuddy/.gitconfig;
}
