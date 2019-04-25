/*
Grabbed from here: https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb

Save this file to ~/default.nix or your preferred path (make sure to
change the location in the update-profile script if you choose a
different place).
Install using `nix-env -f ~ --set`, from then on use `update-profile`.
*/
{ pkgs ? import <nixpkgs> {}
, name ? "user-env"
}: with pkgs;
buildEnv {
  inherit name;
  extraOutputsToInstall = ["out" "bin" "lib"];
  paths = with xfce; [
        alacritty
        cargo
        direnv
        docker
        dotnet-sdk
        dotnetPackages.Nuget
        emacs
        firefox
        haskellPackages.apply-refact
        haskellPackages.hlint
        haskellPackages.hindent
        haskellPackages.stylish-haskell
        haskellPackages.hasktags
        haskellPackages.hoogle
        haskellPackages.hakyll
        ispell
        nodejs
        opam
        pandoc
        pijul
        powertop
        python3
        ripgrep
        rustc
        slack
        spotify
        stack
        teensy-loader-cli

    (writeScriptBin "update-profile" ''
      #!${stdenv.shell}
      nix-env --set -f ~ --argstr name "$(whoami)-user-env-$(date -I)"
    '')

    # Manifest to make sure imperative nix-env doesn't work (otherwise it will overwrite the profile, removing all packages other than the newly-installed one).
    (writeTextFile {
      name = "break-nix-env-manifest";
      destination = "/manifest.nix";
      text = ''
        throw ''\''
          Your user environment is a buildEnv which is incompatible with
          nix-env's built-in env builder. Edit your home expression and run
          update-profile instead!
        ''\''
      '';
    })
    # To allow easily seeing which nixpkgs version the profile was built from, place the version string in ~/.nix-profile/nixpkgs-version
    (writeTextFile {
      name = "nixpkgs-version";
      destination = "/nixpkgs-version";
      text = lib.version;
    })
  ];
}