{ config, pkgs, ... }:

{
  imports = [ ../home.nix ../audio.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    wine

    (import ../dungeondraft { inherit pkgs; })

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
