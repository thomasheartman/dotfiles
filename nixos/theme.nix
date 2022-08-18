let colors = rec {
  # general
  background = "#231F2A";
  background-alt = "#3C3B54";
  foreground = "#EEFAF2";
  primary = "#C216B6";
  disabled = "#707880";
  danger = "#e74c3c";

  # rofi
  bg0 = "${colors.background}E6";
  bg1 = "${colors.background-alt}80";
  bg2 = "${colors.primary}CC";
  fg0 = "#DEDEDE";
  fg1 = "${colors.foreground}";
  fg2 = "${colors.disabled}80";

  transparent = argb: color: opacity:
    if argb then
      "#${builtins.toString opacity}${builtins.substring 1 6 color}"
    else
      "${color}${builtins.toString opacity}";
};
in colors
