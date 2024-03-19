# Edit this configuration file to define what should beinstalled on
# your system.  Help is available in the cnfiguration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let

  hostname = baseNameOf ./.;

in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../base.nix
    ../steam.nix
    ./gpu.nix
    ../hidpi.nix
  ];

  boot = {
    # Enable real-time kernels (for audio production)
    kernelPackages = pkgs.linuxPackages_latest_rt;
  };

  heartman.hidpi = {
    enable = true;
    scale = 1;
  };

  # # make system esync compatible (https://github.com/lutris/docs/blob/master/HowToEsync.md)
  # systemd.services."user@1000".serviceConfig.LimitNOFILE = fileLimit;
  # systemd.user.extraConfig = "DefaultLimitNOFILE=524288";

  # ChatGPT's suggestion. Also doesn't appear to work.
  # systemd.services."systemd-ulimit".serviceConfig.LimitNOFILE = 524288;

  ## try this if the above doesn't work
  # security.pam.loginLimits = let fileLimit = "524288";
  # in [{
  #   domain = "*";
  #   item = "nofile";
  #   type = "hard";
  #   value = fileLimit;
  # }
  # #   { domain = "*"; item = "memlock"; type = "-"; value = fileLimit; }
  # ];

  # todo: try this!
  # security.pam.loginLimits = [
  #   {
  #     domain = "*";
  #     item = "nofile";
  #     type = "-";
  #     value = "32768";
  #   }
  #   {
  #     domain = "*";
  #     item = "memlock";
  #     type = "-";
  #     value = "32768";
  #   }
  # ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  networking.hostName = hostname;

  # musnix https://github.com/musnix/musnix
  musnix = {
    enable = true;
    kernel = {
      # realtime = true; # <- build fails (kernel 6.1.12-rt7 on 2023-03-18T11:48:50+01:00). Instead, we set the kernel type in the boot section.
    };
  };
}
