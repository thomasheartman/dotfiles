{ config, pkgs, ... }:

{
  imports = [ ../home.nix ../audio.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    (import ../dungeondraft { inherit pkgs lib; })

    wine
  ];
}
