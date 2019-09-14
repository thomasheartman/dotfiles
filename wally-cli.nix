let
   pkgs = import <nixpkgs> {};
in pkgs.stdenv.mkDerivation {
  name = "wally-cli-1.1.0";
  src = pkgs.fetchurl {
      name = "wally-cli";
      url = "https://github.com/zsa/wally/releases/download/1.1.0-linux/wally-cli-uncompressed";
      sha256 = "1prsn3fqi44iwsiyh9nhs82hbd1afw98b0x0c0w248z50dddpr17";
  };
  dontStrip = true;
  unpackPhase = ''
    cp $src ./wally-cli
  '';
  installPhase = ''
    mkdir -p $out/bin
    chmod +wx wally-cli
    cp wally-cli $out/bin
    ${pkgs.patchelf}/bin/patchelf \
      --interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${pkgs.lib.makeLibraryPath [ pkgs.libusb1 ]}" \
      $out/bin/wally-cli
  '';
}
