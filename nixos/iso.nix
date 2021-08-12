# Use this file to create custom ISO images (as detailed in
# https://blog.thomasheartman.com/posts/building-a-custom-nixos-installer)
{config, pkgs, ...}:
{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  # programs that should be available in the installer
  environment.systemPackages = with pkgs; [
    fish
    git
    cryptsetup
    home-manager
    curl
  ];
}
