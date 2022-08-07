{ pkgs, config, ... }:

# Created By @icanwalkonwater
# Edited and ported to Nix by Th0rgal

# Look at this setup for potential use on multiple monitors:
# https://github.com/polybar/polybar/issues/763#issuecomment-331604987

# See also this for autorandr-integration:
# https://github.com/polybar/polybar/issues/763#issuecomment-887425778

let
  ac = "#1E88E5";
  mf = "#383838";

  bg = "#00000000";
  fg = "#FFFFFF";

  # Colored
  primary = "#91ddff";

  # Dark
  secondary = "#141228";

  # Colored (light)
  tertiary = "#65b2ff";

  # white
  quaternary = "#ecf0f1";

  # middle gray
  quinternary = "#20203d";

  # Red
  urgency = "#e74c3c";

  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  openMailClient = "${emacsclient} --eval '(notmuch-search heartman/notmuch-unread-mail-query)'";

  polyscript = scriptName: "~/.config/polybar/${scriptName}";

  pkg = pkgs.polybar.override {
    i3GapsSupport = true;
    alsaSupport = true;
  };


in
{
  services.polybar = {
    enable = true;

    package = pkg;

    # load the top bar on all monitors, bottom bar only on main monitor.
    script = let
      pb = "${pkg}/bin/polybar";
    in
      ''
        for m in $(${pb} --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
            MONITOR=$m ${pb} --reload top &
        done

        ${pb} -q -r bottom &
      '';

    config = {
      "global/wm" = {
        margin-bottom = 0;
        margin-top = 0;
      };

      "bar/shared" = {
        monitor = "\${env:MONITOR:}";
        fixed-center = true;
        width = "100%";
        height = 19;
        offset-x = "1%";

        module-margin = "1";
        radius = 0;

        font-0 = "FuraCode Nerd Font:size=12;3";
        font-1 = "FuraCode Nerd Font:style=Bold:size=12;3";
        font-2 = "Noto Color Emoji:style=Regular:size=12";

        locale = "en_US.UTF-8";
      };

      #====================BARS====================#

      "bar/top" = {
        "inherit" = "bar/shared";
        bottom = false;

        # scroll-up = "i3wm-wsnext";
        # scroll-down = "i3wm-wsprev";

        background = bg;
        foreground = fg;

        modules-left = "i3";
        modules-center = "title";
        modules-right = "mail wired-network wifi battery date";
      };

      "bar/bottom" = {
        "inherit" = "bar/shared";
        bottom = true;


        # background = "#88000000";
        background = bg;
        foreground = fg;

        radius-top = 0;


        tray-position = "left";
        tray-detached = false;
        tray-maxsize = 15;
        # tray-background = primary;
        # tray-offset-x = -19;
        # tray-offset-y = 0;
        # tray-padding = 5;
        tray-scale = 1;
        padding = 0;

        # modules-left = "audio";

        # modules-center = "title";

        modules-right = "bluetooth audio keyboard cpu memory";
      };

      "settings" = {
        throttle-output = 5;
        throttle-output-for = 10;

        screenchange-reload = true;

        compositing-background = "source";
        compositing-foreground = "over";
        compositing-overline = "over";
        comppositing-underline = "over";
        compositing-border = "over";

        pseudo-transparency = "false";
      };

      #--------------------MODULES--------------------"

      "module/distro-icon" = {
        type = "custom/script";
        exec =
          "${pkgs.coreutils}/bin/uname -r | ${pkgs.coreutils}/bin/cut -d- -f1";
        interval = 999999999;

        format = " <label>";
        format-foreground = quaternary;
        format-background = secondary;
        format-padding = 1;
        label = "%output%";
        label-font = 2;
      };

      "module/audio" = let
        script = polyscript "pipewire.sh";
      in
        {
          type = "custom/script";

          label = "%output%";

          label-font = 2;

          interval = "2.0";

          exec = script;

          click-right = "exec ${pkgs.pavucontrol}/bin/pavucontrol &";
          click-left = "exec ${script} mute &";
          scroll-up = "exec ${script} down &";
          scroll-down = "exec ${script} up &";
        };

      "module/battery" = {
        type = "internal/battery";
        full-at = 101; # to disable it
        battery = "BAT0"; # TODO: Better way to fill this
        adapter = "AC";

        poll-interval = 5;

        label-full = " 100%";
        # format-full-padding = 1;

        format-charging = " <animation-charging> <label-charging>";
        # format-charging-padding = 1;
        label-charging = "%percentage%% +%consumption%W";
        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";
        animation-charging-framerate = 500;

        format-discharging = "<ramp-capacity> <label-discharging>";
        # format-discharging-padding = 1;
        label-discharging = "%percentage%% -%consumption%W";
        ramp-capacity-0 = "";
        ramp-capacity-0-foreground = urgency;
        ramp-capacity-1 = "";
        ramp-capacity-1-foreground = urgency;
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
      };

      "module/cpu" = {
        type = "internal/cpu";

        interval = "0.5";

        format = " CPU <label>";
        format-padding = 1;

        label = "%percentage:3:3%%";
      };

      "module/date" = {
        type = "internal/date";

        interval = "1.0";

        time = "%H:%M";
        date = "%V %a %b %d";

        format = "<label>";
        # format-padding = 4;
        format-foreground = fg;

        label = " %date%  %time%";
      };

      "module/i3" = {
        type = "internal/i3";
        pin-workspaces = true;

        format = " <label-state> <label-mode>";

        label-mode = "%mode%";
        label-mode-padding = 1;
        label-mode-focused-font = 2;
        label-mode-foreground = secondary;
        label-mode-background = fg;

        label-unfocused = "%name%";
        label-unfocused-padding = 1;

        label-focused = "%name%";
        label-focused-font = 2;
        label-focused-foreground = secondary;
        label-focused-background = fg;
        label-focused-padding = 1;

        label-visible = "%name%";
        label-visible-padding = 1;

        label-urgent = "%name%";
        label-urgent-foreground = urgency;
        label-urgent-padding = 1;

        label-separator = "|";
      };

      "module/title" = {
        type = "internal/xwindow";
        format = "<label>";
        label = "%title%";
        label-maxlen = 70;
      };

      "module/memory" = {
        type = "internal/memory";

        interval = 3;

        format = " RAM <label>";
        format-padding = 1;

        label = "%percentage_used:3:3%%";
      };

      "module/network" = {
        type = "internal/network";
        interval = "1.0";

        format-connected = "<label-connected>";
        format-connected-underline = bg;
        format-connected-overline = bg;
        format-connected-padding = 0;
        format-connected-margin = 0;

        format-disconnected = "<label-disconnected>";
        format-disconnected-underline = bg;
        format-disconnected-overline = bg;
        format-disconnected-padding = 1;
        format-disconnected-margin = 0;
      };

      "module/wifi" = {
        "inherit" = "module/network";
        interface = "wlp59s0";

        label-connected = "直";
        label-disconnected = "睊";
      };

      "module/wired-network" = {
        "inherit" = "module/network";
        interface = "enp10s0";

        label-connected = "";
        label-disconnected = "";
      };

      "module/keyboard" = {
        format-padding = 1;
        type = "internal/xkeyboard";
        label-layout = " %name:0:13:)%";
        label-indicator-on-capslock = "CAPS";
      };


      "module/mail" = let
        search = "${pkgs.notmuch}/bin/notmuch count 'tag:unread +is:inbox -is:draft -is:sent'";
      in
        {
          type = "custom/script";

          # only show mail icon if there are any unread mails
          exec = ''count=$(${search}); if [ $count -gt 0 ]; then echo " $count"; else echo ""; fi '';
          label = "%output%";
          click-left = openMailClient;

          interval = 5;
        };

      "module/bluetooth" = let
        scriptPath = polyscript "bluetooth.sh";
      in
        {
          type = "custom/script";
          exec-if = "bluetoothctl -h";
          exec = scriptPath;
          tail = true;
          click-left = "${scriptPath} --toggle &";
        };

    };
  };
}
