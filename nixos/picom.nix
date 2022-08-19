{ pkgs, config, ... }:


let
  theme = import ./theme.nix;

in
{
  services.picom = let shadowRadius = 8; in
    {
      enable = true;
      shadow = true;

      # use shadows to highlight the active window
      shadowExclude = [
        "!focused"
        # to find a class, use `xprop` and check the
        # WM_CLASS(STRING) property
        "class_g = 'dmenu'"
        "window_type = 'dock'"
      ];
      shadowOpacity = 1.0;

      inactiveOpacity = 0.95;

      settings = {
        # only matters if picom makes windows transparent
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };

        inactive-dim = "0.1";

        # detectTransient = true;

        shadow-color = theme.primary; #"#2deefc"; # "#01fdfe"; # <- is also good
        shadow-radius = shadowRadius;
        shadow-offset-x = -shadowRadius;
        shadow-offset-y = -shadowRadius;


        # Sets the radius of rounded window corners. When > 0, the
        # compositor will round the corners of windows. Does not
        # interact well with `transparent-clipping`.
        corner-radius = 15;

        # Exclude conditions for rounded corners.
        rounded-corners-exclude = [
          "window_type = 'dock'"
          "window_type = 'desktop'"
          "window_type = 'menu'"
          # to find a class, use `xprop` and check the
          # WM_CLASS(STRING) property
          "class_g = 'i3-frame'"
        ];
      };
    };
}
