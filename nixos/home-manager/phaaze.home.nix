{ config, pkgs, ... }:

{
  imports = [ ./home.nix ];

  # note: this is to make the hostname available to home-manager.
  # Ideally I wouldn't have to manually repeat it here after declaring
  # it in the nix configuration, but to share the variable would
  # require some more work. See
  # https://www.lafuente.me/posts/installing-home-manager/ for more
  # info.
  home.sessionVariables = {
    HOSTNAME = "phaaze";
  };

  home.file.".gitconfig.work".source = ~/dotfiles/system-config/porterbuddy/.gitconfig;
}
