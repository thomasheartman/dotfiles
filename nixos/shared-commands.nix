{pkgs, ...}:
{
  restartPolybar = "systemctl --user restart polybar.service";
  setBackgroundImage = "${pkgs.feh}/bin/feh --bg-fill ~/.background-image";
}
