{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    ardour
    artyFX
    bitwig-studio
    calf
    distrho
    drumgizmo
    guitarix
    reaper
    surge
    tonelib-gfx
    tap-plugins
    zrythm
    (callPackage ./scarlett-mixer.nix { })
    (callPackage ./zebralette.nix { })
  ];
}
