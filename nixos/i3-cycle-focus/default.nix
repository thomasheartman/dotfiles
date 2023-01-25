with import <nixpkgs> { };
with pkgs.python3Packages;

buildPythonPackage rec {
  name = "i3-cycle-focus";
  src = ./i3-cycle-focus.py;
  propagatedBuildInputs = [ i3ipc ];
}
