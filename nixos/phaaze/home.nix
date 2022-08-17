{ config, pkgs, ... }:

let
  scarlett-mixer = (pkgs.callPackage
    ({ fetchFromGitHub
    # , glib
    , gobject-introspection
    , meson
    , ninja
    , pkg-config
    , lib
    , stdenv
    , pango
    , cairo
    , lv2
    , alsa-utils
    , mesa
    }:

      stdenv.mkDerivation rec {
        pname = "scarlett-mixer";
        version = "ec3750db5a990d2be9f76aacbc18c6fe96616327";

        strictDeps = true;

        src = fetchFromGitHub {
          owner = "x42";
          repo = pname;
          rev = version;
          hash = "sha256-ioluv/GdWCNGP2jQqsyEbHncCFm8iu69yR8QVKQTJk8=";
        };

        buildInputs = [
          gobject-introspection
          pango
          cairo
          lv2
          alsa-utils
          mesa
        ];

        nativeBuildInputs = [
          meson
          ninja
          pkg-config
        ];

        meta = with lib; {
          homepage = "https://github.com/x42/scarlett-mixer";
          description = "Graphical Mixer Interface for the Scarlett series";
          license = [ licenses.gpl2 ];
          platforms = platforms.linux;
        };
      }) {});

    in

    {
    imports = [ ../home.nix ];

  home.sessionVariables = {
    HOSTNAME = baseNameOf ./.;
  };

  home.packages = with pkgs; [
    surge
    wine
    (callPackage ../scarlett-mixer.nix {})
    (
      writeScriptBin "pfox" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox -P porterbuddy $@
      ''
    )
  ];

  home.file.".gitconfig.work".source = ./../../system-config/unleash/.gitconfig;
  }
