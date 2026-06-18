# This module defines a small NixOS installation CD.  It does not
# contain any graphical stuff.
{ config, pkgs, ... }:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>

    # Provide an initial copy of the NixOS channel so that the user
    # doesn't need to run "nix-channel --update" first.
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  environment = {
    systemPackages = with pkgs; [
      acpi
      any-nix-shell
      arandr
      autorandr
      bind
      bitwarden-cli
      cacert
      cachix
      curl
      exfat
      fd
      fish
      git
      home-manager
      ispell
      killall
      libinput
      libinput-gestures
      networkmanager
      notmuch
      pciutils
      powertop
      tmux
      unzip
      vim
      thunderbolt
      tree
      wget
      xcape
      xclip
      xorg.xev
      xorg.xkbcomp
      zip

      (
        writeScriptBin "rebuild" ''
          #!${stdenv.shell}
          sudo nixos-rebuild switch \
            -p ${config.networking.hostName} \
            --flake ~/dotfiles/nixos#${config.networking.hostName} \
            "$@"
        ''
      )

      (
        writeScriptBin "update" ''
          #!${stdenv.shell}
          nix flake update ~/dotfiles/nixos "$@"
        ''
      )

      (
        writeScriptBin "mfx" ''
          #!${stdenv.shell}
          ${pkgs.xorg.xrandr}/bin/xrandr --output DP-1-2 --off
          ${pkgs.autorandr}/bin/autorandr -c
        ''
      )

      (
        writeScriptBin "hib" ''
          #!${stdenv.shell}
          systemctl hibernate "$@"
        ''
      )

      (
        writeScriptBin "sus" ''
          #!${stdenv.shell}
          systemctl suspend "$@"
        ''
      )

      (
        writeScriptBin "jc" ''
          #!${stdenv.shell}
          journalctrl "$@"
        ''
      )

      (
        writeScriptBin "sc" ''
          #!${stdenv.shell}
          systemctl "$@"
        ''
      )

    ];

  };
}
