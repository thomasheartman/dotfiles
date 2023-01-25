{ pkgs, config, lib, ... }:

let
  cmds = pkgs.callPackage ./shared-commands.nix { };

  theme = import ./theme.nix;

  mod = "Mod4";

  terminal = "${pkgs.alacritty}/bin/alacritty";

  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  openMailClient =
    "${emacsclient} --eval '(notmuch-search heartman/notmuch-unread-mail-query)'";

in
{
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = mod;

      fonts = {
        names = [ "JetBrains Mono, FontAwesome 12" ];
        size = 12.0;
      };


      colors =
        let
          defaults = {
            # only shows if the window doesn't cover the whole container
            background = "#0000";
            # this is the border around the container title bar
            border = theme.background;
            # this is drawn around the window
            childBorder = theme.disabled;
            # the indicator shows where the window will split
            indicator = theme.secondary;
            # the text on the title bar
            text = theme.foreground;

          };
          override = base: args: base // args;
        in
         rec {
          focused = override defaults {
            border = theme.primary;
            childBorder = theme.primary;
          };
          unfocused = defaults;
          focusedInactive = override unfocused {
            border = theme.disabled;
          };
          urgent = override defaults {
            border = theme.danger;
            childBorder = theme.danger;
          };
          # placeholder
        };

      focus = { followMouse = false; };

      terminal = terminal;

      window = { titlebar = false; border = 3; };

      gaps = {
        inner = 15;
        # smartGaps = true;
      };

      keybindings =
        let
          rofi = config.programs.rofi.package;
          window-switcher = (pkgs.writeShellScriptBin "window-switcher" ''
            PATH=${lib.makeBinPath [ rofi ]}
            rofi -show window \
            -kb -accept-entry "!Super-Tab,!Super+Super_L" \
            -kb-row-down "Super+Tab" \
            -kb-row-up "Super+Shift+Tab" \
            -selected-row 1 \
            -show-icons
          ''
          );
        in
        {
          # rofi: apps, switching, and emoji
          "${mod}+space" = ''
            exec ${rofi}/bin/rofi -show combi -show-icons
          '';
          "${mod}+w" = "exec ${rofi}/bin/rofi -show window -show-icons";
          "${mod}+Shift+e" = "exec ${rofi}/bin/rofi -show emoji -show-icons";
          "${mod}+Control+space" = "exec ${rofi}/bin/rofi -show window -show-icons";

          # screenshots
          "${mod}+Print" =
            "exec sh -c '${pkgs.maim}/bin/maim | xclip -selection clipboard -t image/png'";
          "${mod}+Shift+Print" =
            "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";

          # mail
          "${mod}+Shift+m" = "exec ${openMailClient}";

          "${mod}+Return" = "exec ${emacsclient}";
          "${mod}+Shift+Return" = "exec ${terminal}";

          "${mod}+Shift+q" = "kill";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          # move windows and containers
          "${mod}+Control+Right" = "move container to output right; focus output right";
          "${mod}+Control+Left" = "move container to output left; focus output left";
          "${mod}+Control+Up" = "move container to output up; focus output up";
          "${mod}+Control+Down" = "move container to output down; focus output down";
          "${mod}+Shift+Control+Right" = "move workspace to output right";
          "${mod}+Shift+Control+Left" = "move workspace to output left";
          "${mod}+Shift+Control+Up" = "move workspace to output up";
          "${mod}+Shift+Control+Down" = "move workspace to output down";

          # cycle workspaces
          "${mod}+Home" = "workspace prev";
          "${mod}+End" = "workspace next";
          "${mod}+Tab" = ''exec ${window-switcher}/bin/window-switcher'';
          "${mod}+Shift+Tab" = "move container to workspace back_and_forth";

          # change v and h because 'split h' means 'when opening a new
          # window, split the current window's width in two and open
          # it to the right', whereas I think of it as 'draw a
          # horizontal line and use that to split it'. One is: 'split
          # along the horizontal axis', the other is: 'make the
          # separator horizontal'
          "${mod}+v" = "split h";
          "${mod}+h" = "split v";
          "${mod}+Shift+f" = "fullscreen toggle";

          "${mod}+Shift+s" = "layout stacking";
          "${mod}+Shift+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";
          "${mod}+x" = "layout toggle all";
          "${mod}+Shift+space" = "layout toggle all";

          "${mod}+z" = "floating toggle";
          "${mod}+Shift+z" = "focus mode_toggle";

          "${mod}+a" = "focus parent";
          "${mod}+Shift+a" = "focus child";

          "${mod}+1" = "workspace number 1";
          "${mod}+2" = "workspace number 2";
          "${mod}+3" = "workspace number 3";
          "${mod}+4" = "workspace number 4";
          "${mod}+5" = "workspace number 5";
          "${mod}+6" = "workspace number 6";
          "${mod}+7" = "workspace number 7";
          "${mod}+8" = "workspace number 8";
          "${mod}+9" = "workspace number 9";
          "${mod}+0" = "workspace number 10";

          "${mod}+Shift+1" = "move container to workspace number 1; workspace 1";
          "${mod}+Shift+2" = "move container to workspace number 2; workspace 2";
          "${mod}+Shift+3" = "move container to workspace number 3; workspace 3";
          "${mod}+Shift+4" = "move container to workspace number 4; workspace 4";
          "${mod}+Shift+5" = "move container to workspace number 5; workspace 5";
          "${mod}+Shift+6" = "move container to workspace number 6; workspace 6";
          "${mod}+Shift+7" = "move container to workspace number 7; workspace 7";
          "${mod}+Shift+8" = "move container to workspace number 8; workspace 8";
          "${mod}+Shift+9" = "move container to workspace number 9; workspace 9";
          "${mod}+Shift+0" = "move container to workspace number 10; workspace 10";

          "${mod}+Shift+c" = "reload";
          "${mod}+Shift+r" = "restart";
          "${mod}+Shift+n" =
            "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          "${mod}+r" = "mode resize";

          # "${mod}+Control+Up" = "exec ${pkgs.skippy-xd}/bin/skippy-xd";

          # media keys

          # Pulse Audio controls
          "XF86AudioRaiseVolume" =
            "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 +5%"; # increase sound volume
          "XF86AudioLowerVolume" =
            "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-volume 0 -5%"; # decrease sound volume
          "XF86AudioMute" =
            "exec --no-startup-id ${pkgs.pulseaudio}/bin/pactl set-sink-mute 0 toggle"; # mute sound

          # Sreen brightness controls
          "XF86MonBrightnessUp" =
            "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -inc 20"; # increase screen brightness
          "XF86MonBrightnessDown" =
            "exec ${pkgs.xorg.xbacklight}/bin/xbacklight -dec 20"; # decrease screen brightness

          # Media player controls
          "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        };

      bars = [ ];

      startup = [
        {
          command = "i3-msg workspace 1";
          always = true;
          notification = false;
        }
        {
          command = ''${pkgs.alttab}/bin/alttab -fg "${theme.foreground}" -bg "${theme.background}" -frame "${theme.primary}" -t 120x120 -i 120x120'';
          always = true;
          notification = false;
        }
        {
          command = cmds.adjustScreens;
          always = true;
          notification = false;
        }
        {
          command = cmds.restartPolybar;
          always = true;
          notification = false;
        }
        {
          command = cmds.setBackgroundImage;
          always = true;
          notification = false;
        }
        {
          command = "dropbox start";
          notification = false;
        }
      ];
    };
  };
}
