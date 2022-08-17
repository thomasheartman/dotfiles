{ alsa-lib
, cairo
, cmake
# , glib
, gobject-introspection
, lib
, libGLU
, lv2
, mesa
, meson
, ninja
, pango
, pkg-config
, stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation rec {
  pname = "scarlett-mixer";
  version = "35d0ad73d71e1bfc973efe6f66154362f9c1e777";

  strictDeps = true;

  src = fetchFromGitHub {
    owner = "thomasheartman";
    repo = pname;
    rev = version;
    hash = "sha256-bXpZZ/rpVgaZnxcJWE08B0+X002D1HZYfiz0Vgn4yqw=";
    fetchSubmodules = true;
  };

  buildInputs = [
    alsa-lib
    cairo
    gobject-introspection
    libGLU
    lv2
    mesa
    pango
  ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    cmake
  ];

  meta = with lib; {
    homepage = "https://github.com/x42/scarlett-mixer";
    description = "Graphical Mixer Interface for the Scarlett series";
    license = [ licenses.gpl2 ];
    platforms = platforms.linux;
  };
}
