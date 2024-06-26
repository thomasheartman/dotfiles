{ config, pkgs, muse-sounds-manager, ... }:

{
  home.packages = with pkgs; [
    ardour
    artyFX
    bitwig-studio
    calf
    carla
    ChowCentaur
    ChowKick
    ChowPhaser
    CHOWTapeModel
    dexed
    distrho
    drumgizmo
    eq10q
    # guitarix
    musescore # the program is "mscore"
    odin2
    playonlinux # for managing wine installations
    reaper
    surge
    # tonelib-gfx # <- this one often has problems due to hash mismatches :/
    tap-plugins
    tunefish
    yabridge
    yabridgectl
    zrythm
    (callPackage ./scarlett-mixer.nix { })
    (callPackage ./podolski.nix { })
    (callPackage ./zebralette.nix { })
    muse-sounds-manager.packages.x86_64-linux.muse-sounds-manager
  ];
}
