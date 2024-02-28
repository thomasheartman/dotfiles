{ pkgs, config, lib, ... }:

let

  theme = import ./theme.nix;
  cmds = pkgs.callPackage ./shared-commands.nix { };


  mailConfig =
    { mailBoxName, address, passwordName ? mailBoxName, primary ? false }: {
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
            remotepasseval =
              ''${pythonMailPassFn}("${mailPass passwordName}")'';
          };
        };
      };
    };

  gheart = "gheart";

  pythonMailPassFn = "mailpasswd";

  mailPass = account: "${pkgs.pass}/bin/pass show email/${account} 2>/dev/null";

in
{

  imports = [ ./polybar.nix ./rofi.nix ./picom.nix ./i3.nix ];

  programs.msmtp = { enable = true; };

  programs.notmuch = { enable = true; };

  programs.feh.enable = true;

  programs.offlineimap = {
    enable = true;
    extraConfig.general = {
      accounts = "${gheart}, thomasheartman";
      autorefresh = "5";
    };
    pythonFile = ''
      import subprocess

      def ${pythonMailPassFn}(get_password_command):
        print("Running password command: '%s'" % get_password_command)
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

  home.file.".signatures/simple".source = ./../email/signatures/simple;

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
      imap.host = "imap.fastmail.com";

      notmuch.enable = true;

      msmtp = {
        enable = true;
        extraConfig = {
          host = "smtp.fastmail.com";
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
            remotehost = "imap.fastmail.com";
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
    # package = pkgs.emacsUnstable;
    extraPackages = epkgs: [
      epkgs.emacsql-sqlite
      epkgs.vterm
      pkgs.python3
      pkgs.gcc
    ];
  };

  services.emacs = { enable = true; };

  programs.autorandr = {
    enable = true;
    hooks = {
      postswitch = {
        "notify-i3" = "${pkgs.i3}/bin/i3-msg restart";
        "change-background" = cmds.setBackgroundImage;
        "restart-polybar" = cmds.restartPolybar;
      };
    };
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # programs.nushell = {
  #   enable = true;
  #   configFile.source = ../nushell/config.nu;
  #   envFile.source = ../nushell/env.nu;
  # };

  programs.starship = {
    enable = true;
  };

  home.packages = with pkgs; [
    alacritty
    (pkgs.aspellWithDicts
      (dicts: with dicts; [ en en-computers en-science nb ]))
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
    element-desktop
    ffmpeg
    firefox
    git-lfs
    jetbrains-mono
    imagemagick
    ispell
    i3lock
    jq
    libusb1
    mattermost-desktop
    moreutils
    mpv
    msmtp
    mu
    nodejs # <- for copilot
    nixfmt
    nushell
    pavucontrol
    pandoc
    pass
    pdfgrep
    pijul
    playerctl
    powertop
    procs
    pamixer # <- for Emacs' desktop-environment
    proselint
    ripgrep
    rnix-lsp
    sd
    sdcv
    scrot
    shellcheck
    skim
    slack
    spotify
    teensy-loader-cli
    vscode
    tmux
    victor-mono
    vlc
    wally-cli
    # warp-terminal # <- wait for them to add the linux version of this too
    watchexec
    yaml-language-server
    zoom-us

    (
      let
        hostname = config.home.sessionVariables.HOSTNAME;
        resultsDir = "/tmp/${hostname}-home.results";
      in
      writeScriptBin "hms" ''
        #!${stdenv.shell}
        set -e
        nix build \
        ~/dotfiles/nixos#homeManagerConfigs.${hostname}.activationPackage \
        -o ${resultsDir} "$@";
        ${resultsDir}/activate
      ''
    )

    (writeScriptBin "kbs" ''
      #!${stdenv.shell}
      ${xorg.setxkbmap}/bin/setxkbmap us,us altgr-intl,dvp 'grp:shift-caps-toggle'
      xset r rate 200 100
    '')

    (writeScriptBin "copy" ''
      #!${stdenv.shell}
      ${pkgs.xclip}/bin/xclip -selection clipboard $@
    '')

    (writeScriptBin "emq" ''
      #!${stdenv.shell}
      ${config.programs.emacs.package}/bin/emacs -Q -l ~/.emacs.d/straight/repos/straight.el/bootstrap.el $@
    '')

    (writeScriptBin "lwo" ''
      #!${stdenv.shell}
      ${pkgs.lorri}/bin/lorri watch --once $@
    '')

    (writeScriptBin "ff" ''
      #!${stdenv.shell}
      ${firefox}/bin/firefox -p default $@
    '')

    (writeScriptBin "hmr" ''
      #!${stdenv.shell}
      echo "Activating back to the second most recent home-manager generation"
      cd ${home-manager}/bin/home-manager generations | ${gnused}/bin/sed -n 2p | ${coreutils}/bin/cut -d ' ' -f 7) && ./activate
    '')

    (writeScriptBin "emc" ''
      #!${stdenv.shell}
      ${config.programs.emacs.package}/bin/emacsclient -nc "$@"
    '')


    (
      writeScriptBin "dual" ''
        #!${stdenv.shell}
        ${pkgs.autorandr}/bin/autorandr -c home-laptop-below "$@"
      ''
    )

    (
      writeScriptBin "laptop" ''
        #!${stdenv.shell}
        ${pkgs.autorandr}/bin/autorandr -c laptop "$@"
      ''
    )

    # systemctl --user restart pipewire.service
    (
      writeScriptBin "audio" ''
        #!${stdenv.shell}
        systemctl --user restart pipewire.service
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

  home.file.".aspell.conf".text = ''
    home-dir /home/thomas/dotfiles/dictionaries
  '';

  home.file.".config/skippy-xd/skippy-xd.rc".source = ./skippy-xd.rc;

  home.file.".config/polybar/pipewire.sh" = {
    executable = true;

    text = ''
      #!${pkgs.bash}/bin/bash

      getDefaultSink() {
        defaultSink=$(${pkgs.pulseaudio}/bin/pactl info | ${pkgs.gawk}/bin/awk -F : '/Default Sink:/{print $2}')
        description=$(${pkgs.pulseaudio}/bin/pactl list sinks | ${pkgs.gnused}/bin/sed -n "/''${defaultSink}/,/Description/p; /Description/q" | ${pkgs.gnused}/bin/sed -n 's/^.*Description: \(.*\)$/\1/p')
        echo "''${description}"
      }

      getDefaultSource() {
        defaultSource=$(${pkgs.pulseaudio}/bin/pactl info | ${pkgs.gawk}/bin/awk -F : '/Default Source:/{print $2}')
        description=$(${pkgs.pulseaudio}/bin/pactl list sources | ${pkgs.gnused}/bin/sed -n "/''${defaultSource}/,/Description/p; /Description/q" | ${pkgs.gnused}/bin/sed -n 's/^.*Description: \(.*\)$/\1/p')
        echo "''${description}"
      }

      function main() {
          DEFAULT_SOURCE=$(getDefaultSource)
          DEFAULT_SINK=$(getDefaultSink)
          VOLUME=$(${pkgs.pulseaudio}/bin/pactl list sinks | ${pkgs.gnused}/bin/sed -n "/Sink #''${DEFAULT_SINK_ID}/,/Volume/ s!^[[:space:]]\+Volume:.* \([[:digit:]]\+\)%.*!\1!p" | ${pkgs.coreutils}/bin/head -n1)
          IS_MUTED=$(${pkgs.pulseaudio}/bin/pactl list sinks | ${pkgs.gnused}/bin/sed -n "/Sink #''${DEFAULT_SINK_ID}/,/Mute/ s/Mute: \(yes\)/\1/p")

          action=$1
          if [ "''${action}" == "up" ]; then
              ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%
          elif [ "''${action}" == "down" ]; then
              ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%
          elif [ "''${action}" == "mute" ]; then
              ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle
          else
              if [ "''${IS_MUTED}" != "" ]; then
                  echo " ''${DEFAULT_SOURCE} | ﱝ ''${VOLUME}% ''${DEFAULT_SINK}"
              else
                  echo " ''${DEFAULT_SOURCE} | 墳 ''${VOLUME}% ''${DEFAULT_SINK}"
              fi
          fi
      }

      main "$@"

    '';
  };

  # symlink files to home
  home.activation =
    let
      links = [
        ".alacritty.yml"
        ".aliases"
        ".direnvrc"
        ".editorconfig"
        ".emacs.d"
        ".gitconfig"
        ".gitignore"
        ".notmuch-config"
        ".stardict"
        ".vimrc"
        "dictionaries"
        "email"
        "feeds.org"
        "nix-shells"
      ];

      # the config directory is special. We don't want to end up with
      # a bunch of everchanging files in our dotfiles, so lets link
      # those files differently.
      configLinks =
        lib.attrsets.mapAttrsToList (name: _: name) (builtins.readDir ../.config);

      weirdLinks = [{
        source = "wallpapers/desktop-background.jpg";
        target = ".background-image";
      }];

      mkMiscSymlink = inputFile: outputFile: ''
        $DRY_RUN_CMD ln -nfs $VERBOSE_ARG \
            $HOME/dotfiles/${inputFile} $HOME/${outputFile};
      '';
      mkSymlink = path: mkMiscSymlink path path;
      mkConfigSymlink = path: mkMiscSymlink ".config/${path}" ".config/${path}";

      mkScript = f: links: lib.strings.concatStringsSep "\n" (map f links);

    in
    {
      "link basic home files" = lib.hm.dag.entryAfter [ "writeBoundary" ]
        (mkScript mkSymlink links);

      "link .config files" = lib.hm.dag.entryAfter [ "writeBoundary" ]
        (mkScript mkConfigSymlink configLinks);

      "link other files" = lib.hm.dag.entryAfter [ "writeBoundary" ]
        (mkScript ({ source, target }: mkMiscSymlink source target) weirdLinks);
    };
}
