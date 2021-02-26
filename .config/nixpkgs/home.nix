{ pkgs, ... }:

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

  my-emacs = (pkgs.emacsPackagesGen unstable.emacsGcc).emacsWithPackages
    (epkgs: [ epkgs.exwm epkgs.emacsql-sqlite epkgs.vterm ]);

  exwm-load-script = pkgs.writeText "exwm-load.el" ''
    (require 'exwm)
    (exwm-init)
  '';

  mailConfig = { mailBoxName, user, passwordName ? mailBoxName }:
    let
      address = "${user}@gmail.com";
    in
      {
        realName = "Thomas Heartman";
        smtp.tls.useStartTls = true;
        flavor = "gmail.com";
        notmuch.enable = true;
        offlineimap.enable = true;
        msmtp.enable = true;
        signature.showSignature = "append";
        address = address;

        msmtp = {
          extraConfig = {
            host = "smtp.gmail.com";
            port = "587";
            from = address;
            user = user;
            passwordeval = ''fish -c "bw get password ${passwordName}"'';
            logfile = "~/.msmtp.${mailBoxName}.log";
          };
        };

        offlineimap = {
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
      sslcacertfile = "/etc/ssl/certs/ca-certificates.crt";
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
    mailBoxName = gheart;
    user = "thomasheartman";
  } // {
    primary = true;
    signature.text = ''
      --
      :: Thomas Heartman
      :: he/him
    '';
  };

  accounts.email.accounts.${enonicMail} = mailConfig {
    mailBoxName = enonicMail;
    user = "the";
    passwordName = "enonic-mail";
  } // {
    signature.text = ''
      --
      :: Thomas Heartman
      :: Developer advocate
    '';
  };

  xsession = {
    enable = true;
    windowManager.command = ''
      ${my-emacs}/bin/emacs -l "${exwm-load-script}"
    '';
    initExtra = ''
      xset r rate 200 100
    '';
  };

  home.keyboard = {
    layout = "us,us";
    variant = ",dvp";
    options = [ "grp:shift_caps_toggle" ];
  };

  home.packages = with pkgs; [
    my-emacs
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
    wally-cli
    watchexec
    zoom-us

    (
      writeScriptBin "hms" ''
        #!${stdenv.shell}
        ${home-manager}/bin/home-manager switch
      ''
    )
  ];

  services.dropbox.enable = true;

}
