{ pkgs, config, lib, ... }:

let
  cmds = pkgs.callPackage ./shared-commands.nix {};

  mod = "Mod4";

  terminal = "${pkgs.alacritty}/bin/alacritty";

  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  openMailClient =
    "${emacsclient} --eval '(notmuch-search heartman/notmuch-unread-mail-query)'";

in
{
  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = {
      modifier = mod;

      fonts = {
        names = [ "JetBrains Mono, FontAwesome 12" ];
        size = 12.0;
      };

      focus = { followMouse = false; };

      terminal = terminal;

      window = { titlebar = false; };

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

          # move windows and containers
          "${mod}+Next" = "move container to output right";
          "${mod}+Prior" = "move container to output left";
          "${mod}+Shift+Next" = "move workspace to output right";
          "${mod}+Shift+Prior" = "move workspace to output left";

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

          "${mod}+Shift+1" = "move container to workspace number 1";
          "${mod}+Shift+2" = "move container to workspace number 2";
          "${mod}+Shift+3" = "move container to workspace number 3";
          "${mod}+Shift+4" = "move container to workspace number 4";
          "${mod}+Shift+5" = "move container to workspace number 5";
          "${mod}+Shift+6" = "move container to workspace number 6";
          "${mod}+Shift+7" = "move container to workspace number 7";
          "${mod}+Shift+8" = "move container to workspace number 8";
          "${mod}+Shift+9" = "move container to workspace number 9";
          "${mod}+Shift+0" = "move container to workspace number 10";

          "${mod}+Shift+c" = "reload";
          "${mod}+Shift+r" = "restart";
          "${mod}+Shift+n" =
            "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

          "${mod}+r" = "mode resize";

          "${mod}+Control+Up" = "exec ${pkgs.skippy-xd}/bin/skippy-xd";

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
          command = "exec i3-msg workspace 1";
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
      ];
    };
  };
}
