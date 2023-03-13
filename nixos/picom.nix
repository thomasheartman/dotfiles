{ pkgs, config, ... }:

let theme = import ./theme.nix;

in {
  services.picom = let shadowRadius = 8;
  in {
    enable = true;
    shadow = true;

    settings = {

      inactive-dim = "0.1";

      shadow-exclude = [ "window_type = 'dock'" ];

      wintypes = {
        dropdown_menu = { shadow = false; };
        popup_menu = { shadow = false; };
        utility = { shadow = false; };
      };

      # Rounding corners doesn't work well with i3's child borders,
      # so let's leave that out for now.

      # Sets the radius of rounded window corners. When > 0, the
      # compositor will round the corners of windows. Does not
      # interact well with `transparent-clipping`.
      # corner-radius = 15;

      # Exclude conditions for rounded corners.
      # rounded-corners-exclude = [
      #   "window_type = 'dock'"
      #   "window_type = 'desktop'"
      #   "window_type = 'menu'"
      #   "window_type = 'dnd'"
      #   # to find a class, use `xprop` and check the
      #   # WM_CLASS(STRING) property
      #   "class_g = 'i3-frame'"
      # ];

    };

  };
}
