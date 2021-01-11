{ pkgs, ... }:

let

  my-emacs = pkgs.emacsWithPackagesFromUsePackage {
    config = /home/thomas/.emacs.d/init.el;
    package = pkgs.emacsGcc;
    alwaysTangle = true;
    # alwaysEnsure = true;
    extraEmacsPackages = epkgs: [ epkgs.exwm ];
  };

  exwm-load-script = ''
    (require 'exwm)
    (exwm-init)
  '';

in {

  # xsession = {
  #   enable = false;
  #   windowManager.command = ''
  #     ${my-emacs}/bin/emacs -l ${exwm-load-script}
  #   '';
  # };

  home.packages = with pkgs; [
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
