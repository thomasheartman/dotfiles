{ config, pkgs, ... }:

{
  imports = [ ../home.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    surge
    (
      writeScriptBin "pfox" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -P porterbuddy $@
      ''
    )
  ];

  home.file.".gitconfig.work".source = ./../../system-config/unleash/.gitconfig;
}
