{ stdenv, libuuid, libxcb, expat, gtk3, glib }:

stdenv.mkDerivation {
  pname = "Podolski";
  version = "1.2.3";
  src = builtins.fetchurl {
    url = "https://dl.u-he.com/releases/Podolski_123_12092_Linux.tar.xz";
    sha256 = "sha256:1326hgpmm0124040g7h5km98b2d3pi06xvlsx0r7g694p5pimnc8";
  };
  dontBuild = true;
  sourceRoot = "Podolski-12092";

  installPhase = ''
    mkdir -p $out/lib/vst
    cp -r Podolski/* $out/lib
    rm $out/lib/Data/Scripts/EditorSetup.txt
    rm -rf $out/lib/Data/Images
    rm -rf $out/lib/license.txt
    ln -f $out/lib/Podolski.64.so $out/lib/vst/Podolski.64.so
  '';

  postFixup = ''
    patchelf \
      --set-rpath ${expat}/lib:${libxcb}/lib:${libuuid.out}/lib \
      $out/lib/vst/Podolski.64.so
    patchelf \
      --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      --set-rpath ${gtk3}/lib:${glib.out}/lib \
      $out/lib/dialog.64
  '';
}
