# hidpi support config variables
# based on https://nixos.wiki/wiki/Xorg
{ lib, pkgs, config, ... }:
with lib;

{
  options.heartman.hidpi.enable = mkEnableOption "Heartman's HiDPI config";

  options.heartman.hidpi.scale = mkOption {
    default = 1;
    example = 2;
    description = "How much to scale the display by.";
    type = lib.types.float;
  };

  config = let

    scale = config.heartman.hidpi.scale;

    dpiScale = 1.0 / scale;

  in mkIf config.heartman.hidpi.enable {
    services.xserver.dpi = 180;

    environment.variables = {
      GDK_SCALE = toString scale;
      GDK_DPI_SCALE = pkgs.lib.strings.floatToString dpiScale;
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=${toString scale}";
    };
  };
}
