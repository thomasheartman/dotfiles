{ config, pkgs, ... }:

let

  hostname = baseNameOf ./.;

in
{
  imports = [
    <nixos-hardware/lenovo/thinkpad>
    <nixos-hardware/lenovo/thinkpad/yoga.nix>
    ./hardware-configuration.nix
    ../base.nix
  ];

  boot = {
    kernelParams = [
      # as recommended by
      # https://wiki.archlinux.org/title/Lenovo_ThinkPad_X1_Carbon_(Gen_6)
      "i915.enable_dc=2"
    ];
    kernelPackages = pkgs.linuxPackages_latest;
  };


  services.xserver = {
    useGlamor = true;
    displayManager.autoLogin.user = "thomas";
    videoDrivers = [ "intel" ];
    deviceSection = ''
      Option "DRI" "2"
      Option "TearFree" "true"
    '';
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
    hostName = hostname;
  };

  services.logind.extraConfig = ''
    IdleAction=suspend-then-hibernate
    IdleActionSec=1800
  '';

  security.pam.enableEcryptfs = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
      preLVM = true;
    };
  };

  services.fprintd.enable = true;
}
