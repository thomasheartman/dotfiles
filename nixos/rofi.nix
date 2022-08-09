{ pkgs, config, ... }:

# Taken from / inspired by https://github.com/tars0x9752/home
# Discovered in this reddit thread: https://www.reddit.com/r/NixOS/comments/wih19c/ive_been_using_nix_for_a_little_over_a_month_and/



let
  colors = rec {
    # general
    background = "#231F2A";
    background-alt = "#3C3B54";
    foreground = "#EEFAF2";
    primary = "#C216B6";
    disabled = "#707880";

    # rofi
    bg0 = "${colors.background}E6";
    bg1 = "${colors.background-alt}80";
    bg2 = "${colors.primary}E6";
    fg0 = "#DEDEDE";
    fg1 = "${colors.foreground}";
    fg2 = "${colors.disabled}80";
  };

in
{

  programs.rofi = {
    enable = true;
    package = pkgs.rofi.override { plugins = [ pkgs.rofi-emoji pkgs.rofi-calc ]; };
    cycle = true;
    terminal = config.xsession.windowManager.i3.config.terminal;
    extraConfig = { modi = "window,run,emoji,calc"; };
    font = "Open Sans 20";
    theme =
      let
        mkL = config.lib.formats.rasi.mkLiteral;
      in
      {
        "*" = {
          bg0 = mkL colors.bg0;
          bg1 = mkL colors.bg1;
          bg2 = mkL colors.bg2;
          fg0 = mkL colors.fg0;
          fg1 = mkL colors.fg1;
          fg2 = mkL colors.fg2;

          background-color = mkL "transparent";
          text-color = mkL "@fg0";

          margin = 0;
          padding = 0;
          spacing = 0;
        };

        window = {
          background-color = mkL "@bg0";
          location = mkL "center";
          width = 1000;
          y-offset = -400;
          border-radius = 8;
        };

        inputbar = {
          padding = mkL "12px";
          spacing = mkL "12px";
          children = map mkL [ "icon-search" "entry" ];
        };

        icon-search = {
          expand = false;
          filename = "search";
          size = mkL "28px";
          vertical-align = mkL "0.5";
        };

        entry = {
          placeholder = "Search";
          placeholder-color = mkL "@fg2";
          vertical-align = mkL "0.5";
        };

        message = {
          border = mkL "2px 0 0";
          border-color = mkL "@bg1";
          background-color = mkL "@bg1";
        };

        textbox = {
          padding = mkL "8px 24px";
        };

        listview = {
          lines = 10;
          columns = 1;
          fixed-height = false;
          border = mkL "1px 0 0";
          border-color = mkL "@bg1";
        };

        element = {
          padding = mkL "8px 16px";
          spacing = mkL "16px";
          background-color = mkL "transparent";
        };

        element-icon = {
          size = mkL "1em";
          vertical-align = mkL "0.5";
        };

        element-text = {
          text-color = mkL "inherit";
          vertical-align = mkL "0.5";
        };

        "element normal active" = {
          text-color = mkL "@bg2";
        };

        "element selected normal" = {
          background-color = mkL "@bg2";
          text-color = mkL "@fg1";
        };

        "element selected active" = {
          background-color = mkL "@bg2";
          text-color = mkL "@fg1";
        };
      };
  };
}
