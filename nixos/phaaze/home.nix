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
    drumgizmo
    guitarix
    surge
    tonelib-gfx
    tap-plugins
    wine
    (callPackage ../scarlett-mixer.nix { })
    (
      writeScriptBin "pfox" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -P porterbuddy $@
      ''
    )
  ];

  home.file.".gitconfig.work".source = ./../../system-config/unleash/.gitconfig;
}
