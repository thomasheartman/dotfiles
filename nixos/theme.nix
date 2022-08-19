let theme = rec {
  # general
  background = "#252034";
  background-alt = "#3C3B54";
  foreground = "#EEFAF2";
  primary = "#01fdfe";
  primary-contrast = theme.background;
  disabled = "#707880";
  danger = "#e74c3c";

  # rofi
  bg0 = "${theme.background}E6";
  bg1 = "${theme.background-alt}80";
  bg2 = "${theme.primary}CC";
  fg0 = "#DEDEDE";
  fg1 = "${theme.foreground}";
  fg2 = "${theme.primary-contrast}";
  fg3 = "${theme.disabled}80";

  transparent = argb: color: opacity:
    if argb then
      "#${builtins.toString opacity}${builtins.substring 1 6 color}"
    else
      "${color}${builtins.toString opacity}";
};
in theme
