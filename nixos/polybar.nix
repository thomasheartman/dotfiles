{ pkgs, config, ... }:

# Created By @icanwalkonwater
# Edited and ported to Nix by Th0rgal

# Look at this setup for potential use on multiple monitors:
# https://github.com/polybar/polybar/issues/763#issuecomment-331604987

# See also this for autorandr-integration:
# https://github.com/polybar/polybar/issues/763#issuecomment-887425778

let
  theme = import ./theme.nix;

  transparent = theme.transparent true;

  bg = theme.background;
  fg = theme.foreground;

  primary = theme.primary;

  # Red
  urgency = theme.danger;

  emacsclient = ''${config.programs.emacs.package}/bin/emacsclient -nc "$@"'';

  openMailClient = "${emacsclient} --eval '(notmuch-search heartman/notmuch-unread-mail-query)'";

  polyscript = scriptName: "~/.config/polybar/${scriptName}";

  pkg = pkgs.polybar.override {
    i3GapsSupport = true;
    alsaSupport = true;
    pulseSupport = true;
    iwSupport = true;
  };


in
{
  services.polybar = {
    enable = true;

    package = pkg;

    # load the top bar on all monitors, bottom bar only on main monitor.
    script =
      let
        pb = "${pkg}/bin/polybar";
      in
      ''
        for m in $(${pb} --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
            MONITOR=$m ${pb} --reload top &
        done

        ${pb} -q -r bottom &
      '';

    config =
      let
        inline-padding = 1;
        spacing = "10px";
        pulseaudio-control = "${pkgs.callPackage ./pulseaudio-control.nix { } }/bin/pulseaudio-control";
      in
      {
        "global/wm" = {
          margin-bottom = 0;
          margin-top = 0;
        };

        "bar/shared" = {
          monitor = "\${env:MONITOR:}";
          fixed-center = true;
          width = "100%";
          height = 30;
          offset-x = "1%";
          padding-left = spacing;
          padding-right = spacing;

          module-margin = spacing;
          radius = 0;

          font-0 = "Open Sans:size=12;3";
          font-1 = "JetBrainsMono:style=Regular:size=12;3";
          font-2 = "FuraCode Nerd Font:size=12;3";
          font-3 = "Noto Color Emoji:style=Book:scale=6";

          locale = "en_US.UTF-8";
        };

        #====================BARS====================#

        "bar/top" = {
          "inherit" = "bar/shared";
          bottom = false;

          background = transparent theme.background "DD";
          foreground = fg;

          modules-left = "i3 title";
          modules-right = "mail bluetooth pulseaudio-control-output keyboard wired-network wifi battery date time";
        };

        "bar/bottom" = {
            "inherit" = "bar/shared";
            bottom = true;


          #   # background = "#88000000";
          #   background = bg;
          #   foreground = fg;

          #   radius-top = 0;


          tray-position = "left";
          tray-detached = false;
          tray-maxsize = 15;
          tray-background = primary;
          tray-offset-x = -19;
          tray-offset-y = 0;
          tray-padding = 5;
          tray-scale = 1;
          padding = 0;

          #   # modules-left = "audio";

          #   # modules-center = "title";

          #   modules-right = "bluetooth audio keyboard cpu memory";
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

        "module/date" = {
          type = "internal/date";

          interval = "1.0";

          date = "W%V %a %b %d";

          format = "<label>";
          format-foreground = fg;

          label = " %date%";
        };

        "module/time" = {
          type = "internal/date";
          interval = "1.0";

          time = "%H:%M";
          format = "<label>";

          label = " %time%";
          label-font = 2;
        };

        "module/i3" = {
          type = "internal/i3";
          pin-workspaces = true;

          format = " <label-state> <label-mode>";

          label-mode = "%mode%";
          label-mode-font = 2;
          label-mode-foreground = theme.primary-contrast;
          label-mode-background = theme.primary;
          label-mode-padding = spacing;

          label-unfocused = "%name%";
          label-unfocused-padding = spacing;
          label-unfocused-font = 2;

          label-focused = "%name%";
          label-focused-foreground = bg;
          label-focused-background = fg;
          label-focused-padding = spacing;
          label-focused-font = 2;

          label-visible = "%name%";
          label-visible-padding = spacing;
          label-visible-font = 2;

          label-urgent = "%name%";
          label-urgent-foreground = theme.primary-contrast;
          label-urgent-background = theme.primary;
          label-urgent-padding = spacing;
          label-urgent-font = 2;

          label-separator = "|";
        };

        "module/title" = {
          type = "internal/xwindow";
          format = "<label>";
          label = "%title%";
          label-padding = 1;
          label-maxlen = 70;
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
          label-indicator-on-capslock-background = theme.primary;
          label-indicator-on-capslock-foreground = theme.primary-contrast;
          label-indicator-on-capslock-padding = spacing;
        };


        "module/mail" =
          let
            search = "${pkgs.notmuch}/bin/notmuch count 'tag:unread +is:inbox -is:draft -is:sent'";
          in
          {
            type = "custom/script";

            # only show mail icon if there are any unread mails
            exec = ''count=$(${search}); if [ $count -gt 0 ]; then echo " $count"; else echo ""; fi '';
            label = "%output%";
            label-forgeround = theme.foreground;
            label-padding = spacing;
            label-background = theme.primary;
            click-left = openMailClient;
            label-font = 3;

            interval = 5;
          };

        "module/bluetooth" =
          let
            scriptPath = polyscript "bluetooth.sh";
          in
          {
            type = "custom/script";
            exec-if = "bluetoothctl -h";
            exec = scriptPath;
            tail = true;
            click-left = "${scriptPath} --toggle &";
          };

        "module/pulseaudio-control-output" = {
          type = "custom/script";
          tail = true;

          # 必要に応じて nickname および sink や source 名(node名)を変更すること
          # --color-muted は # なしの rrggbb のため # を取り除く
          exec = ''${pulseaudio-control} --format '$VOL_ICON $VOL_LEVEL $NODE_NICKNAME' --color-muted "${builtins.replaceStrings ["#"] [""] theme.disabled}" --icons-volume " , " --icon-muted "ﱝ " --node-nicknames-from "device.profile.name" --node-nickname "alsa_output.pci-0000_00_1f.3.analog-stereo:built-in" listen'';
          click-right = "exec ${pkgs.pavucontrol}/bin/pavucontrol &";
          click-left = "${pulseaudio-control} togmute";
          click-middle = "${pulseaudio-control} next-node";
          scroll-up = "${pulseaudio-control} --volume-max 130 down";
          scroll-down = "${pulseaudio-control} --volume-max 130 up";
          label-foreground = "${fg}";
        };

      };
  };
}
