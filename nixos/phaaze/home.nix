{ config, pkgs, ... }:

{
  imports = [ ../home.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    (
      writeScriptBin "pfox" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -P porterbuddy $@
      ''
    )
  ];

  home.file.".gitconfig.work".source = ~/dotfiles/system-config/unleash/.gitconfig;
}
