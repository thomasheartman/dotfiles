{ pkgs, ... }:

let

  my-emacs = pkgs.emacsWithPackagesFromUsePackage {
    config = ~/.emacs.d/init.el;
    package = pkgs.emacsGcc;
    alwaysTangle = true;
    extraEmacsPackages = epkgs: [
      epkgs.exwm
      epkgs.emacsql-sqlite
      epkgs.vterm
      epkgs.pdf-tools
    ];
  };

  exwm-load-script = pkgs.writeText "exwm-load.el" ''
    (require 'exwm)
    (exwm-init)
  '';

in {



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
    autojump
    bat
    bitwarden-cli
    cacert
    cascadia-code
    chromium
    direnv
    docker
    ffmpeg
    firefox
    gcc # <-this is here to make magit forge work
    ispell
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

    (writeScriptBin "hms" ''
      #!${stdenv.shell}
      ${home-manager}/bin/home-manager switch
    '')
  ];

  services.dropbox.enable = true;

  services.xscreensaver = {
    enable = true;
    settings = {
      lock = true;
      lockTimeout = 5;
      mode = "blank";
    };
  };

}
