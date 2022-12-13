# hidpi support config variables
{ pkgs, scale ? 2, ... }:

let dpiScale = 1.0 / scale;

in

{
  services.xserver.dpi = 180;
  console.font =
    "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
  environment.variables = {
    GDK_SCALE = toString scale;
    GDK_DPI_SCALE = pkgs.lib.strings.floatToString dpiScale;
    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=${toString scale}";
  };
}
