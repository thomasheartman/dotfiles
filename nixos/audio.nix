{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    ardour
    artyFX
    bitwig-studio
    calf
    ChowCentaur
    ChowKick
    ChowPhaser
    CHOWTapeModel
    dexed
    distrho
    drumgizmo
    eq10q
    guitarix
    odin2
    reaper
    surge
    # tonelib-gfx # <- this one often has problems due to hash mismatches :/
    tap-plugins
    tunefish
    zrythm
    (callPackage ./scarlett-mixer.nix { })
    (callPackage ./zebralette.nix { })
  ];
}
