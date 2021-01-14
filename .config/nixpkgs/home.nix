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
    (configure-ivy-posframe-for-exwm)
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
    moreutils
    mpv
    msmtp
    mu
    nixfmt
    pavucontrol
    pandoc
    pijul
    powertop
    ripgrep
    skim
    slack
    spotify
    teensy-loader-cli
    tmux
    victor-mono
    vlc
    wally-cli
    watchexec

    (writeScriptBin "hms" ''
      #!${stdenv.shell}
      ${home-manager}/bin/home-manager switch
    '')
  ];
}
