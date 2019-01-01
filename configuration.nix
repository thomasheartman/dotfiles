# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  compiledLayout = pkgs.runCommand "keyboard-layout" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${/home/thomas/xkbmap} $out
  '';
in
{
    imports =
      [ # Include the results of the hardware scan.
        ./hardware-configuration.nix
      ];

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.kernelParams = [
      # "acpi_osi=!" "acpi_osi=\"Windows 2009\"" "nomodeset"
      "acpi_rev_override" "nomodeset"
    ];

    system.autoUpgrade.enable = true;

    # networking.hostName = "nixos"; # Define your hostname.
    networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    # networking.networkmanager.enable = true;
    networking.wireless.networks = {
        Hartnet = { psk = "0x12FD10B"; };
    };
    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Select internationalisation properties.
    i18n = {
      consoleFont = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
      consoleUseXkbConfig = true;
    };

    fonts.fonts = with pkgs; [
        fira-code
        fira-code-symbols
        mplus-outline-fonts
        powerline-fonts
    ];

    # Set your time zone.
    # time.timeZone = "Europe/Amsterdam";

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = with pkgs; [
      alacritty
      autorandr
      curl
      emacs
      fd
      firefox
      fish
      git
      ispell
      pciutils
      pijul
      ripgrep
      slack
      spotify
      tmux
      vim
      wget
      xcape
      xorg.xev
      xorg.xkbcomp
    ];

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

    # List services that you want to enable:

    # Enable the OpenSSH daemon.
    # services.openssh.enable = true;

    # Open ports in the firewall.
    # networking.firewall.allowedTCPPorts = [ ... ];
    # networking.firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # networking.firewall.enable = false;

    # Enable CUPS to print documents.
    # services.printing.enable = true;

    # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    services.xserver = {
      enable = true;
      libinput.enable = true;
      layout = "us";
      xkbModel = "pc104";
      xkbVariant = "dvp";
      xkbOptions = "ctrl:nocaps";
      exportConfiguration = true;
      displayManager.sddm = {
        enable = true;
        autoLogin.enable = true;
        autoLogin.user = "thomas";
      };
      desktopManager.plasma5.enable = true;


      displayManager.sessionCommands = ''
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${compiledLayout} $DISPLAY
        systemctl --user restart xcape.service
        '';

      # windowManager.xmonad = {
      #   enable = true;
      #   enableContribAndExtras = true;
      #   extraPackages = haskellPackages: [
      #     haskellPackages.xmonad-contrib
      #     haskellPackages.xmonad-extras
      #     haskellPackages.xmonad
      #   ];
      # };
      # windowManager.default = "xmonad";

    };
    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "18.09"; # Did you read the comment?

    # users
    users.extraUsers.thomas = {
      name = "thomas";
      group = "users";
      extraGroups = [
        "wheel" "dis" "audio" "video" "networkmanager" "systemd-journal"
      ];
    createHome = true;
    uid = 1000;
    home = "/home/thomas";
    shell = pkgs.fish;
    };

    services.emacs.enable = true;
    services.emacs.defaultEditor = true;

    systemd.user.services.xcape = {
      enable = true;
      description = "xcape";
      wantedBy = [ "graphical.target" ];
      preStart = ''
        echo "About to set custom kb layout"
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp /home/thomas/layout.xkb $DISPLAY
        echo "Successfully (?) set custom kb layout"
      '';
      restartIfChanged = true;
      serviceConfig = {
        Type = "forking";
        Restart = "always";
        RestartSec = 2;
        ExecStart = "${pkgs.xcape}/bin/xcape -t 250 -e \'Shift_L=dollar;Shift_R=numbersign;Control_L=Escape;Control_R=Return\'";
      };
    };

    services.nixosManual.showManual = true;

}

#  LocalWords:  thomas dvp LocalWords
