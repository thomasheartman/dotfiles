{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    ardour
    artyFX
    bitwig-studio
    calf
    dexed
    distrho
    drumgizmo
    guitarix
    reaper
    surge
    # tonelib-gfx # <- this one often has problems :/
    tap-plugins
    zrythm
    (callPackage ./scarlett-mixer.nix { })
    (callPackage ./zebralette.nix { })
  ];
}
