{ config, pkgs, ... }:

let
  # keyboard
  compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${/etc/nixos/layout.xkb} $out
  '';

  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';

  unstableTarball = fetchTarball
    "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz";


in
{
  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    kernelParams = [
      "mem_sleep_default=deep"
    ];

    tmpOnTmpfs = true;
  };

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  system.autoUpgrade.enable = true;

  networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n = { defaultLocale = "en_US.UTF-8"; };

  console = {
    useXkbConfig = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
  };

  fonts = {
    fonts = with pkgs; [
      dejavu_fonts
      fira-code
      fira-code-symbols
      ipafont
      joypixels
      kochi-substitute
      mplus-outline-fonts
      noto-fonts
      powerline-fonts
      symbola
    ];

    fontconfig = {
      defaultFonts = {
        monospace = [ "DejaVu Sans Mono" "IPAGothic" ];
        sansSerif = [ "DejaVu Sans" "IPAPGothic" ];
        serif = [ "DejaVu Serif" "IPAPMincho" ];
        emoji = [ "JoyPixels" ];
      };
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = import unstableTarball { config = config.nixpkgs.config; };
    };

    allowUnfree = true;
  };

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
      exfat-utils
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
      nvidia-offload
      nvtop
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
        sudo nixos-rebuild switch -p ${config.networking.hostName} $@
      ''
    )
    ];

    interactiveShellInit = ''
      alias nixdot='git --git-dir=/etc/nixos/git --work-tree=/etc/nixos/'
    '';

    variables."SSL_CERT_FILE" = "/etc/ssl/certs/ca-bundle.crt";
  };

  # Enable sound.
  sound.enable = true;

  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # uncomment this when moving to unstable or to stable after 21.03/05
  # hardware.keyboard.zsa.enable = true;

  programs = {
    fish = {
      enable = true;
      promptInit = ''
        any-nix-shell fish --info-right | source
      '';
    };
    ssh.startAgent = true;
    tmux = {
      enable = true;
      clock24 = true;
    };
    gnupg = { agent.enable = true; };
  };

  hardware.nvidiaOptimus.disable = true;

  services.thermald.enable = true;

  location.longitude = 10.45;
  location.latitude = 59.54;

  services.redshift = {
    enable = true;
    brightness.night = "0.8";
    extraOptions = [ "-m randr" ];
  };

  # https://github.com/target/lorri/
  services.lorri.enable = true;

  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      naturalScrolling = true;
      disableWhileTyping = true;
    };

    exportConfiguration = true;
    autoRepeatDelay = 250;
    autoRepeatInterval = 150;

    videoDrivers = [ "intel" ];

    windowManager.exwm = {
      enable = true;
      enableDefaultConfig = false;
      extraPackages = epkgs: [
        epkgs.emacsql-sqlite
        epkgs.vterm
        epkgs.magit
        epkgs.pdf-tools
      ];
    };

    displayManager.defaultSession = "none+exwm";
  };

  # users
  users.extraUsers.thomas = {
    name = "thomas";
    group = "users";
    extraGroups = [
      "audio"
      "disk"
      "docker"
      "networkmanager"
      "plugdev"
      "root"
      "systemd-journal"
      "video"
      "wheel"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/thomas";
    shell = pkgs.fish;
  };

  nix.trustedUsers = [ "root" "thomas" ];
  services.emacs.defaultEditor = true;

  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [ "--all" ];
    };
  };

  security.sudo.extraConfig = ''
    %wheel ALL=(ALL:ALL) ${pkgs.systemd}/bin/poweroff
    %wheel ALL=(ALL:ALL) ${pkgs.systemd}/bin/reboot
    %wheel ALL=(ALL:ALL) ${pkgs.systemd}/bin/systemctl suspend
    %wheel ALL=(ALL:ALL) ${pkgs.systemd}/bin/systemctl hibernate
  '';

  systemd.user.services.autorandrize = {
    enable = true;
    description = "Automatically adjust screens when waking up";
    wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
    after = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" ];
    serviceConfig = {
      Type = "oneshot";
      TimeOutSec = "0";
      ExecStart = "${pkgs.autorandr}/bin/autorandr -c";
    };
  };

}
