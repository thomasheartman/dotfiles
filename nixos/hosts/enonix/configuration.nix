{ config, pkgs, ... }:

{
  imports = [
    <nixos-hardware/dell/xps/15-7590>
    ./hardware-configuration.nix
    ./../../base.nix
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    useDHCP = false;
    interfaces.wlp59s0.useDHCP = true;
  };

  environment.systemPackages = with pkgs; [
    ecryptfs
    ecryptfs-helper
    lsof
    rsync
  ];

  programs.xss-lock = {
    enable = true;
    lockerCommand = ''
      ${pkgs.i3lock}/bin/i3lock -c 000000
    '';
  };

  services.logind.extraConfig = ''
    IdleAction=suspend-then-hibernate
    IdleActionSec=1800
  '';

  services.hardware.bolt.enable = true;

  security.pam.enableEcryptfs = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
