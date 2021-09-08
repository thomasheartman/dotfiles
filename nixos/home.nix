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
    (setenv "SUPPRESS_MINI_FRAME" "t")
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
            passwordeval = ''${mailPass passwordName}'';
            logfile = "~/.msmtp.${mailBoxName}.log";
          };
        };

        offlineimap = {
          enable = true;
          postSyncHookCommand = "${pkgs.notmuch}/bin/notmuch new";
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
  porterbuddy = "porterbuddy";

  mailPass = account: "${pkgs.pass}/bin/pass show email/${account}";

in
{

  programs.msmtp = { enable = true; };

  programs.notmuch = { enable = true; };

  programs.feh.enable = true;

  programs.offlineimap = {
    enable = true;
    extraConfig.general = {
      accounts = "${gheart}, ${porterbuddy}, thomasheartman";
      autorefresh = "5";
    };
    pythonFile = ''
      import subprocess

      def mailpasswd(account):
        print "Starting password fetching for account email/%s" % account
        try:
          return subprocess.check_output("${mailPass "%s"}" % account, shell=True)
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

  accounts.email.accounts.${porterbuddy} = mailConfig {
    mailBoxName = porterbuddy;
    address = "heartman@porterbuddy.com";
  };

  home.file.".signatures/${porterbuddy}".source = ~/dotfiles/email/signatures/porterbuddy;
  home.file.".signatures/simple".source = ~/dotfiles/email/signatures/simple;

  accounts.email.accounts."thomasheartman" =
    let
      mailBoxName = "thomasheartman.com";
      primary = false;
      address = "thomas@thomasheartman.com";
      passwordName = "thomasheartman.com";
    in
      {

        realName = "Thomas Heartman";
        primary = primary;
        address = address;

        smtp.tls.useStartTls = true;
        imap.tls.useStartTls = true;
        imap.host = "imappro.zoho.eu";

        notmuch.enable = true;

        msmtp = {
          enable = true;
          extraConfig = {
            host = "smtppro.zoho.eu";
            port = "587";
            from = address;
            user = address;
            passwordeval = ''${mailPass passwordName}'';
            logfile = "~/.msmtp.${mailBoxName}.log";
          };
        };

        offlineimap = {
          enable = true;
          postSyncHookCommand = "${pkgs.notmuch}/bin/notmuch new";
          extraConfig = {
            local = {
              type = "Maildir";
              localfolders = "~/mail/${mailBoxName} ";
            };
            remote = {
              type = "IMAP";
              remotehost = "imappro.zoho.eu";
              remoteuser = address;
              remotepasseval = ''mailpasswd("${passwordName}")'';
            };
          };
        };
      };

  # note: this is how you set up aliases for sending (along with the
  # appropriate config for gnus-aliases). Because it's the same inbox
  # as the one listed above, we don't need to set up more offlineimap
  # stuff or use a different log file.
  accounts.email.accounts."self@thomasheartman" =
    let
      mailBoxName = "thomasheartman.com";
      primary = false;
      address = "self@thomasheartman.com";
      passwordName = "mail@thomasheartman.com";
    in
      {

        realName = "Thomas Heartman";
        primary = primary;
        address = address;

        smtp.tls.useStartTls = true;

        notmuch.enable = false; # set this to true to enable this as a sender in notmuch
        msmtp = {
          enable = true;
          extraConfig = {
            host = "smtppro.zoho.eu";
            port = "587";
            from = address;
            user = address;
            passwordeval = ''${mailPass passwordName}'';
            logfile = "~/.msmtp.${mailBoxName}.log";
          };
        };
      };

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
    extraPackages = epkgs: [ epkgs.exwm epkgs.emacsql-sqlite epkgs.vterm pkgs.python3 pkgs.gcc ];
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
    brightnessctl
    cacert
    cascadia-code
    chromium
    direnv
    discord
    docker
    dropbox-cli
    element-desktop
    ffmpeg
    firefox
    git-lfs
    imagemagick
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
    pdfgrep
    pijul
    playerctl
    powertop
    unstable.procs
    pamixer # <- for Emacs' desktop-environment
    proselint
    ripgrep
    rnix-lsp
    sd
    sdcv
    scrot
    shellcheck
    skim
    unstable.slack
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
        ${home-manager}/bin/home-manager switch -f ~/dotfiles/nixos/${config.home.sessionVariables.HOSTNAME}/home.nix
      ''
    )

    (
      writeScriptBin "kbs" ''
        #!${stdenv.shell}
        ${xorg.setxkbmap}/bin/setxkbmap us,us altgr-intl,dvp 'grp:shift-caps-toggle'
        xset r rate 200 100
      ''
    )

    (
      writeScriptBin "copy" ''
        #!${stdenv.shell}
        ${pkgs.xclip}/bin/xclip -selection clipboard $@
      ''
    )

    (
      writeScriptBin "emq" ''
        #!${stdenv.shell}
        ${config.programs.emacs.package}/bin/emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el $@
      ''
    )

    (
      writeScriptBin "lwo" ''
        #!${stdenv.shell}
        ${pkgs.lorri}/bin/lorri watch --once $@
      ''
    )

    (
      writeScriptBin "ff" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -p default $@
      ''
    )

  ];

  services.dropbox.enable = true;

  services.picom = {
    enable = true;
    shadow = true;
    shadowExclude = [ "!focused" "name ~= '^EXWM workspace'" ];
  };

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

  home.file.".aspell.conf".text = ''
    home-dir /home/thomas/dotfiles/dictionaries
  '';

}
