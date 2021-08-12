{ config, pkgs, ... }:

let

  hostname = "phaaze";

in
{
  imports = [ (./. + "/${hostname}.hardware-configuration.nix") ./base.nix ];

  boot.kernelParams = [ "acpi_rev_override" ];

  services.xserver = {
    useGlamor = true;

    displayManager.autoLogin.user = "thomas";
  };

  networking.hostName = hostname;


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?
}
