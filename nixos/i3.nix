{ pkgs, config, lib, ... }:

let
  cmds = pkgs.callPackage ./shared-commands.nix { };

  theme = import ./theme.nix;

  super = "Mod4";
  alt = "Mod1";

  terminal = "${pkgs.alacritty}/bin/alacritty";

  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  openMailClient =
    "${emacsclient} --eval '(notmuch-search heartman/notmuch-unread-mail-query)'";

  python = "${pkgs.python3.withPackages (ps: with ps; [ i3ipc ])}/bin/python";


  # todo: package window switching like this?
  # window-switcher = pkgs.writeShellApplication {
  #   name = "window-switcher";
  #   runtimeInputs = [ pkgs.python3.withPackages (ps: [ ps.i3ipc ]) ];
  #   text = ''python ${./i3-cycle-focus.py} "$@"'';
  # };


in
{
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      modifier = super;

      fonts = {
        names = [ "JetBrains Mono, FontAwesome 12" ];
        size = 12.0;
      };


      colors =
        let
          defaults = {
            # only shows if the window doesn't cover the whole container
            background = "#00000000";
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
          unfocused = override defaults {
            childBorder = "#00000000";
          };
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
        in
        {
          # rofi: apps, switching, and emoji
          "${super}+space" = ''
            exec ${rofi}/bin/rofi -show combi -show-icons
          '';
          "${super}+w" = "exec ${rofi}/bin/rofi -show window -show-icons";
          "${super}+Shift+e" = "exec ${rofi}/bin/rofi -show emoji -show-icons";
          "${super}+Control+space" = "exec ${rofi}/bin/rofi -show window -show-icons";

          # screenshots
          "${super}+Print" =
            "exec sh -c '${pkgs.maim}/bin/maim | xclip -selection clipboard -t image/png'";
          "${super}+Shift+Print" =
            "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";

          # mail
          "${super}+Shift+m" = "exec ${openMailClient}";

          "${super}+Return" = "exec ${emacsclient}";
          "${super}+Shift+Return" = "exec ${terminal}";

          "${super}+Shift+q" = "kill";

          "${super}+Left" = "focus left";
          "${super}+Down" = "focus down";
          "${super}+Up" = "focus up";
          "${super}+Right" = "focus right";

          "${super}+Shift+Left" = "move left";
          "${super}+Shift+Down" = "move down";
          "${super}+Shift+Up" = "move up";
          "${super}+Shift+Right" = "move right";

          # move windows and containers
          "${super}+Control+Right" = "move container to output right; focus output right";
          "${super}+Control+Left" = "move container to output left; focus output left";
          "${super}+Control+Up" = "move container to output up; focus output up";
          "${super}+Control+Down" = "move container to output down; focus output down";
          "${super}+Shift+Control+Right" = "move workspace to output right";
          "${super}+Shift+Control+Left" = "move workspace to output left";
          "${super}+Shift+Control+Up" = "move workspace to output up";
          "${super}+Shift+Control+Down" = "move workspace to output down";

          # cycle workspaces
          "${super}+Home" = "workspace prev";
          "${super}+End" = "workspace next";
          "${alt}+Tab" = ''exec --no-startup-id  "${python} ${./i3-cycle-focus.py} --switch"'';

          # change v and h because 'split h' means 'when opening a new
          # window, split the current window's width in two and open
          # it to the right', whereas I think of it as 'draw a
          # horizontal line and use that to split it'. One is: 'split
          # along the horizontal axis', the other is: 'make the
          # separator horizontal'
          "${super}+v" = "split h";
          "${super}+h" = "split v";
          "${super}+Shift+f" = "fullscreen toggle";

          "${super}+Shift+s" = "layout stacking";
          "${super}+Shift+w" = "layout tabbed";
          "${super}+e" = "layout toggle split";
          "${super}+x" = "layout toggle all";
          "${super}+Shift+space" = "layout toggle all";

          "${super}+z" = "floating toggle";
          "${super}+Shift+z" = "focus mode_toggle";

          "${super}+a" = "focus parent";
          "${super}+Shift+a" = "focus child";

          "${super}+1" = "workspace number 1";
          "${super}+2" = "workspace number 2";
          "${super}+3" = "workspace number 3";
          "${super}+4" = "workspace number 4";
          "${super}+5" = "workspace number 5";
          "${super}+6" = "workspace number 6";
          "${super}+7" = "workspace number 7";
          "${super}+8" = "workspace number 8";
          "${super}+9" = "workspace number 9";
          "${super}+0" = "workspace number 10";

          "${super}+Shift+1" = "move container to workspace number 1; workspace 1";
          "${super}+Shift+2" = "move container to workspace number 2; workspace 2";
          "${super}+Shift+3" = "move container to workspace number 3; workspace 3";
          "${super}+Shift+4" = "move container to workspace number 4; workspace 4";
          "${super}+Shift+5" = "move container to workspace number 5; workspace 5";
          "${super}+Shift+6" = "move container to workspace number 6; workspace 6";
          "${super}+Shift+7" = "move container to workspace number 7; workspace 7";
          "${super}+Shift+8" = "move container to workspace number 8; workspace 8";
          "${super}+Shift+9" = "move container to workspace number 9; workspace 9";
          "${super}+Shift+0" = "move container to workspace number 10; workspace 10";

          "${super}+Shift+c" = "reload";
          "${super}+Shift+r" = "restart";
          "${super}+Shift+n" =
            "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          "${super}+r" = "mode resize";

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
          command = "dropbox start";
          notification = false;
        }
        {
          command = "${python} ${./i3-cycle-focus.py} --delay 0.1";
          always = true;
          notification = false;
        }
        {
          command = "i3-msg workspace 1";
          always = true;
          notification = false;
        }
        {
          command = cmds.setBackgroundImage;
          always = true;
          notification = false;
        }
        {
          command = cmds.restartPolybar;
          always = true;
          notification = false;
        }
      ];
    };
  };
}
