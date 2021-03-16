{ pkgs, config, ... }:

let

  unstable = import (
    fetchTarball
      "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
  ) {
    overlays = [
      (
        import (
          builtins.fetchTarball {
            url =
              "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
          }
        )
      )
    ];
  };

  exwm-load-script = pkgs.writeText "exwm-load.el" ''
    (require 'exwm)
    (exwm-init)
  '';

  mailConfig = { mailBoxName, address, passwordName ? mailBoxName, primary ? false }:
    let
    in
      {
        realName = "Thomas Heartman";
        primary = primary;
        address = address;
        flavor = "gmail.com";

        smtp.tls.useStartTls = true;
        imap.tls.useStartTls = true;

        notmuch.enable = true;

        msmtp = {
          enable = true;
          extraConfig = {
            host = "smtp.gmail.com";
            port = "587";
            from = address;
            user = address;
            passwordeval = ''fish -c "bw get password ${passwordName}"'';
            logfile = "~/.msmtp.${mailBoxName}.log";
          };
        };

        offlineimap = {
          enable = true;
          extraConfig = {
            local = {
              type = "Maildir";
              localfolders = "~/mail/${mailBoxName} ";
            };
            remote = {
              type = "Gmail";
              remoteuser = address;
              remotepasseval = ''mailpasswd("${passwordName}")'';
            };
          };
        };
      };

  gheart = "gheart";
  enonicMail = "enonic";

in
{

  programs.msmtp = { enable = true; };

  programs.notmuch = { enable = true; };

  programs.offlineimap = {
    enable = true;
    extraConfig.general = {
      accounts = "gheart, enonic";
      autorefresh = "5";
    };
    pythonFile = ''
      # implicitly requires fish shell and the bitwarden-cli, and for the bitwarden
      # session env variable ($BW_SESSION) to have been set
      import subprocess

      def mailpasswd(account):
        print "Starting password fetching for account %s" % account
        try:
          return subprocess.check_output('fish -c "bw get password %s" '% account, shell=True)
        except subprocess.CalledProcessError:
          return ""
    '';
  };

  accounts.email.maildirBasePath = "mail";
  accounts.email.accounts.${gheart} = mailConfig {
    primary = true;
    mailBoxName = gheart;
    address = "thomasheartman@gmail.com";
  };
  home.file.".signatures/signature.${gheart}".text = ''
    Thomas Heartman (he/him)
  '';

  accounts.email.accounts.${enonicMail} = mailConfig {
    mailBoxName = enonicMail;
    address = "the@enonic.com";
    passwordName = "enonic-mail";
  };
  home.file.".signatures/signature.${enonicMail}".text = ''
    Thomas Heartman (he/him)
    Developer advocate
    Enonic (https://enonic.com)
  '';

  xsession = {
    enable = true;
    windowManager.command = ''
      ${config.programs.emacs.package}/bin/emacs -l "${exwm-load-script}"
    '';
    initExtra = ''
      xset r rate 200 100
    '';
  };

  home.keyboard = {
    layout = "us,us";
    variant = "altgr-intl,dvp";
    options = [ "grp:shift_caps_toggle" ];
  };

  programs.emacs = {
    enable = true;
    package = unstable.emacsGcc;
    extraPackages = epkgs: [ epkgs.exwm epkgs.emacsql-sqlite epkgs.vterm ];
  };

  home.packages = with pkgs; [
    alacritty
    (
      pkgs.aspellWithDicts
        (dicts: with dicts; [ en en-computers en-science nb ])
    )
    autojump
    bat
    bitwarden-cli
    cacert
    cascadia-code
    chromium
    direnv
    docker
    dropbox-cli
    ffmpeg
    firefox
    gcc # <-this is here to make magit forge work
    ispell
    i3lock
    jetbrains-mono
    jq
    libusb
    mattermost-desktop
    moreutils
    mpv
    msmtp
    mu
    nixfmt
    pavucontrol
    pandoc
    pijul
    playerctl
    powertop
    proselint
    ripgrep
    rnix-lsp
    sdcv
    scrot
    skim
    slack
    spotify
    teensy-loader-cli
    tmux
    victor-mono
    vlc
    unstable.wally-cli
    watchexec
    zoom-us

    (
      writeScriptBin "hms" ''
        #!${stdenv.shell}
        ${home-manager}/bin/home-manager switch
      ''
    )

    (
      writeScriptBin "kbs" ''
        #!${stdenv.shell}
        ${xorg.setxkbmap}/bin/setxkbmap us,us altgr-intl,dvp 'grp:shift-caps-toggle'
        xset r rate 200 100
      ''
    )
  ];

  services.dropbox.enable = true;

  home.file.".config/proselint/config".text = ''
    {
      "checks": {
        "cursing.filth": false,
        "cursing.nfl": false,
        "misc.but": false,
        "typography.symbols": false
      }
    }
  '';

}
