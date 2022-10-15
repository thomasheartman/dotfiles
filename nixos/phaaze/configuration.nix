{ config, pkgs, modulesPath, ... }:

let

  hostname = baseNameOf ./.;

in
{
  imports = [ ./hardware-configuration.nix ../base.nix ../hidpi.nix ../steam.nix ];

  boot = {
    kernelParams = [
      "acpi_rev_override"
      "intel_iommu=igfx=off"
      "nvidia-drm.modeset=1"
    ];
    extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];
  };

  environment.systemPackages = [
    pkgs.nvtop
  ];

  services.xserver = {
    displayManager.autoLogin.user = "thomas";
  };

  networking.hostName = hostname;

  services.xserver = {
    videoDrivers = [ "nvidia" ];

    config = ''
      Section "Device"
          Identifier  "Intel Graphics"
          Driver      "intel"
          #Option      "AccelMethod"  "sna" # default
          #Option      "AccelMethod"  "uxa" # fallback
          Option      "TearFree"        "true"
          Option      "SwapbuffersWait" "true"
          BusID       "PCI:0:2:0"
          #Option      "DRI" "2"             # DRI3 is now default
      EndSection

      Section "Device"
          Identifier "nvidia"
          Driver "nvidia"
          BusID "PCI:1:0:0"
          Option "AllowEmptyInitialConfiguration"
      EndSection
    '';

    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';
  };

  hardware.video.hidpi.enable = true;

  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId = "PCI:0:2:0";
  };
  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

  # musnix https://github.com/musnix/musnix
  musnix = {
    enable = true;
    kernel = {
      optimize = true;
      # realtime = true; # seems not to work right with nvidia: https://github.com/musnix/musnix/issues/127
    };
  };
}
