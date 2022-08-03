# Use this file to create custom ISO images (as detailed in
# https://blog.thomasheartman.com/posts/building-a-custom-nixos-installer)
{ config, pkgs, ... }:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  nixpkgs.config = {
    allowUnfree = true;
  };


  # programs that should be available in the installer
  environment.systemPackages = with pkgs; [
    acpi
    any-nix-shell
    arandr
    autorandr
    bind
    bitwarden-cli
    cacert
    cachix
    cryptsetup
    curl
    exfat
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
    nvtop
    pciutils
    powertop
    thunderbolt
    tmux
    tree
    unzip
    vim
    wget
    xcape
    xclip
    xorg.xev
    xorg.xkbcomp
    zip
  ];
}
