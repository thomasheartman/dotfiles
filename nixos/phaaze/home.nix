{ config, pkgs, ... }:

{
  imports = [ ../home.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    ardour
    artyFX
    calf
    distrho
    drumgizmo
    guitarix
    reaper
    surge
    tonelib-gfx
    tap-plugins
    wine
    zrythm
    (callPackage ../scarlett-mixer.nix { })

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
  ];

  home.file.".gitconfig.work".source = ./../../system-config/unleash/.gitconfig;
}
