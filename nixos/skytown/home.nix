{ config, pkgs, ... }:

{
  imports = [ ../home.nix ../audio.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    wine

  ];
}
