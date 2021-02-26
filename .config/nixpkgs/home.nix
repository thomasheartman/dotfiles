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

  msmtpGmailCommon = {
    host = "smtp.gmail.com";
    port = "587";
  };

in
{

  programs.msmtp = {
    enable = true;
  };

  programs.notmuch = {
    enable = true;
  };

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
  accounts.email.accounts.gheart = {
    primary = true;
    realName = "Thomas Heartman";
    signature = {
      showSignature = "append";
      text = ''
        --
        :: Thomas Heartman
        :: he/him
      '';
    };

    address = "thomasheartman@gmail.com";
    flavor = "gmail.com";
    msmtp = {
      enable = true;
      extraConfig = msmtpGmailCommon // {
        from = "thomasheartman@gmail.com";
        user = "thomasheartman";
        passwordeval = ''fish -c "bw get password gheart"'';
        logfile = "~/.msmtp.gheart.log";
      };
    };
    notmuch.enable = true;
    offlineimap = {
      enable = true;
      extraConfig = {
        local = {
          type = "Maildir";
          localfolders = "~/mail/gheart";
        };
        remote = {
          type = "Gmail";
          remoteuser = "thomasheartman@gmail.com";
          remotepasseval = ''mailpasswd("gheart")'';
        };
      };

    };
  };

  accounts.email.accounts.enonic = {
    realName = "Thomas Heartman";
    signature = {
      showSignature = "append";
      text = ''
        --
        :: Thomas Heartman
        :: Developer advocate
      '';
    };

    address = "the@enonic.com";
    flavor = "gmail.com";
    msmtp = {
      enable = true;
      extraConfig = msmtpGmailCommon // {
        from = "the@enonic.com";
        user = "the";
        passwordeval = ''fish -c "bw get password enonic-mail"'';
        logfile = "~/.msmtp.gheart.log";
      };
    };
    notmuch.enable = true;
    offlineimap = {
      enable = true;
      extraConfig = {
        local = {
          type = "Maildir";
          localfolders = "~/mail/enonic";
        };
        remote = {
          type = "Gmail";
          remoteuser = "the@enonic.com";
          remotepasseval = ''mailpasswd("enonic-mail")'';
        };
      };

    };
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
