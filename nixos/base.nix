{ config, pkgs, ... }:

let
  # keyboard
  compiledLayout = pkgs.runCommand "keyboard-layout" { } ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${/etc/nixos/layout.xkb} $out
  '';


  unstablePkgs = import
    (
      fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
    )
    {
      config = config.nixpkgs.config;
    };

  user = "thomas";
  homeDir = "/home/${user}";

in
{
  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    tmpOnTmpfs = true;
  };

  nix = {
    package = pkgs.nixVersions.stable;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Don't auto-upgrade. From the docs:
  #
  #  If enabled, a systemd timer will run
  #  `nixos-rebuild switch --upgrade`
  #  once a day.
  #
  # Because my upgrade command is different, this causes a whole lot
  # of issues when set to true.
  system.autoUpgrade.enable = false;


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
      font-awesome
      ipafont
      jetbrains-mono
      kochi-substitute
      nerdfonts
      noto-fonts
      noto-fonts-emoji
      open-sans
      powerline-fonts
      siji
      symbola
    ];

    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" "DejaVu Sans Mono" "IPAGothic" ];
        sansSerif = [ "Open Sans" "DejaVu Sans" "IPAPGothic" ];
        serif = [ "DejaVu Serif" "IPAPMincho" ];
        emoji = [ "Noto Color Emoji" ];
      };
    };
  };

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  nixpkgs.config = {
    packageOverrides = pkgs: {
      unstable = unstablePkgs;
    };

    allowUnfree = true;
  };

  # experimental features
  nix.settings.experimental-features = "nix-command flakes";

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
      nushell
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

    interactiveShellInit = ''
      alias nixdot='git --git-dir=/etc/nixos/git --work-tree=/etc/nixos/'
    '';

    pathsToLink = [ "/libexec" ];
  };

  # audio config based on https://nixos.wiki/wiki/PipeWire
  security.rtkit.enable = true;

  hardware.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            # mSBC is not expected to work on all headset + adapter combinations.
            "bluez5.msbc-support" = true;
            # SBC-XQ is not expected to work on all headset + adapter combinations.
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        matches = [
          # Matches all sources
          { "node.name" = "~bluez_input.*"; }
          # Matches all outputs
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = {
          "node.pause-on-idle" = false;
        };
      }
    ];
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  hardware.keyboard.zsa.enable = true;

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
      mouse = {
        naturalScrolling = true;
      };
      touchpad = {
        naturalScrolling = true;
        disableWhileTyping = true;
      };
    };

    exportConfiguration = true;
    autoRepeatDelay = 250;
    autoRepeatInterval = 150;

    displayManager.lightdm = {
      enable = true;
      greeters.enso = {
        enable = true;
        blur = true;
      };
    };

    desktopManager.gnome.enable = true;

    desktopManager.xfce = {
      enable = true;
      enableXfwm = false;
      noDesktop = true;
    };

    displayManager.defaultSession = "xfce";
  };

  # users
  users.extraUsers.${user} = {
    isNormalUser = true;
    name = user;
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
    home = homeDir;
    shell = pkgs.nushell;
  };

  nix.settings.trusted-users = [ "root" user ];

  services.offlineimap = {
    enable = true;
    install = true;
    path = [ pkgs.pass pkgs.notmuch ];
  };

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
