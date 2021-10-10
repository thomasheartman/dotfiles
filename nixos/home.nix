{ pkgs, config, ... }:

let

  mod = "Mod4";
  terminal = "${pkgs.alacritty}/bin/alacritty";
  rofi = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji ]; };
  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  unstable = import (
    fetchTarball
      "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
  ) {
    overlays = [
      (
        import (
          # use a specific version (as mentioned here: https://github.com/nix-community/emacs-overlay/issues/170)
          # this is to avoid having to suddenly rebuild Emacs when wanting to change other, unrelated config.
          # The list of commits can be found at https://github.com/nix-community/emacs-overlay/commits/master
          builtins.fetchTarball {
            url =
              # "https://github.com/nix-community/emacs-overlay/archive/c51b95cce591f58e0631f6c3c2cdc0c9ff96adab.tar.gz";
              "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
            # sha256 = "07pzqz0kivwi0kv3q9bykx9203jmvs4psiqhrb1if91kvwcll3fp";
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
            passwordeval = mailPass passwordName;
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
              remotepasseval = ''${pythonMailPassFn}("${mailPass passwordName}")'';
            };
          };
        };
      };

  gheart = "gheart";
  porterbuddy = "porterbuddy";

  pythonMailPassFn = "mailpasswd";

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

      def ${pythonMailPassFn}(get_password_command):
        print "Running password command: '%s'" % get_password_command
        try:
          return subprocess.check_output(get_password_command, shell=True)
        except subprocess.CalledProcessError:
          return ""
    '';
  };

  accounts.email.maildirBasePath = "mail";
  accounts.email.accounts.${gheart} = mailConfig {
    mailBoxName = gheart;
    primary = false;
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
      primary = true;
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
            passwordeval = mailPass passwordName;
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
              remotepasseval = ''mailpasswd("${mailPass passwordName}")'';
            };
          };
        };
      };

  # note: this is how you set up aliases for sending (along with the
  # appropriate config for gnus-aliases). Because it's the same inbox
  # as the one listed above, we don't need to set up more offlineimap
  # stuff or use a different log file.

  # accounts.email.accounts."self@thomasheartman" =
  #   let
  #     mailBoxName = "thomasheartman.com";
  #     primary = false;
  #     address = "self@thomasheartman.com";
  #     passwordName = "mail@thomasheartman.com";
  #   in
  #     {

  #       realName = "Thomas Heartman";
  #       primary = primary;
  #       address = address;

  #       smtp.tls.useStartTls = true;

  #       notmuch.enable = false; # set this to true to enable this as a sender in notmuch
  #       msmtp = {
  #         enable = true;
  #         extraConfig = {
  #           host = "smtppro.zoho.eu";
  #           port = "587";
  #           from = address;
  #           user = address;
  #           passwordeval = mailPass passwordName;
  #           logfile = "~/.msmtp.${mailBoxName}.log";
  #         };
  #       };
  #     };

  xsession = {
    enable = true;
    # windowManager.command = ''
    #   ${config.programs.emacs.package}/bin/emacs -l "${exwm-load-script}"
    # '';
    initExtra = ''
      xset r rate 200 100
    '';

    windowManager.i3 = {
      enable = true;
      config = {
        modifier = mod;

        fonts = [ "DejaVu Sans Mono, FontAwesome 12" ];

        focus = {
          followMouse = false;
        };

        terminal = terminal;

        window = {
          titlebar = false;
        };

        keybindings = {
          # rofi: apps, switching, and emoji
          "${mod}+space" = "exec ${rofi}/bin/rofi -show run -show-icons";
          "${mod}+w" = "exec ${rofi}/bin/rofi -show window -show-icons";
          "${mod}+Shift+e" = "exec ${rofi}/bin/rofi -show emoji -show-icons";

          # screenshots
          "${mod}+Print" = "exec sh -c '${pkgs.maim}/bin/maim | xclip -selection clipboard -t image/png'";
          "${mod}+Shift+Print" = "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";

          # move windows and containers
          "${mod}+Next" = "move container to output right";
          "${mod}+Prior" = "move container to output left";
          "${mod}+Shift+Next" = "move workspace to output right";
          "${mod}+Shift+Prior" = "move workspace to output left";

          "${mod}+Return" = "exec ${terminal}";
          "${mod}+Shift+Return" = "exec ${emacsclient}";

          "${mod}+Shift+q" = "kill";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          # cycle workspaces
          "${mod}+Home" = "workspace prev";
          "${mod}+End" = "workspace next";
          "${mod}+Tab" = "workspace back_and_forth";
          "${mod}+Shift+Tab" = "move container to workspace back_and_forth";

          # change v and h because 'split h' means 'when opening a new
          # window, split the current window's width in two and open
          # it to the right', whereas I think of it as 'draw a
          # horizontal line and use that to split it'. One is: 'split
          # along the horizontal axis', the other is: 'make the
          # separator horizontal'
          "${mod}+v" = "split h";
          "${mod}+h" = "split v";
          "${mod}+Shift+f" = "fullscreen toggle";

          "${mod}+Shift+s" = "layout stacking";
          "${mod}+Shift+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";

          "${mod}+z" = "floating toggle";
          "${mod}+Shift+z" = "focus mode_toggle";

          "${mod}+a" = "focus parent";

          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";

          "${mod}+Shift+1" =
            "move container to workspace number 1";
          "${mod}+Shift+2" =
            "move container to workspace number 2";
          "${mod}+Shift+3" =
            "move container to workspace number 3";
          "${mod}+Shift+4" =
            "move container to workspace number 4";
          "${mod}+Shift+5" =
            "move container to workspace number 5";
          "${mod}+Shift+6" =
            "move container to workspace number 6";
          "${mod}+Shift+7" =
            "move container to workspace number 7";
          "${mod}+Shift+8" =
            "move container to workspace number 8";
          "${mod}+Shift+9" =
            "move container to workspace number 9";
          "${mod}+Shift+0" =
            "move container to workspace number 10";

          "${mod}+Shift+c" = "reload";
          "${mod}+Shift+r" = "restart";
          "${mod}+Shift+n" =
            "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          "${mod}+r" = "mode resize";
        };


        bars = [
          {
            position = "top";
            statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${./i3status-rust.toml}";
            fonts = [ "JetBrains Mono, FontAwesome 20" ];
          }
        ];
      };
    };

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

  programs.rofi = {
    enable = true;
    package = rofi;
    cycle = true;
    terminal = terminal;
    extraConfig = {
      modi = "window,run,emoji";
    };
    theme = ../rofi/themes/alter.rasi;
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
    pass
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
    yaml-language-server
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

    (
      writeScriptBin "hmr" ''
        #!${stdenv.shell}
        echo "Activating back to the second most recent home-manager generation"
        cd ${home-manager}/bin/home-manager generations | ${gnused}/bin/sed -n 2p | ${coreutils}/bin/cut -d ' ' -f 7) && ./activate
      ''
    )

    (
      writeScriptBin "emc" ''
        #!${stdenv.shell}
        ${emacsclient}
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
