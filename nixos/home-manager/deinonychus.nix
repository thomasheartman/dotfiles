{ config, pkgs, ... }:

let
  projectsDir = "projects/work";
  hooksDir = "${projectsDir}/git-hooks";

in
{
  imports = [ ./home.nix ];


  home.file.".gitconfig.work" = {
    text = ''
      [user]
              email = the+github@enonic.com

      [core]
              hooksPath = ~/dotfiles/system-config/enonic/git-hooks

    '';
  };
}
