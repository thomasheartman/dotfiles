{
  description = "My Nixos system config";

  inputs =
    # let
    #   systemVersion = "22.05";
    #   # if system version is "unstable" use "master"; otherwise use "release-${systemVersion}"
    #   # homeManagerVersion = if systemVersion == "unstable" then "master" else "release-${systemVersion}";
    # in
    {
      # nixpkgs.url = "nixpkgs/nixos-${systemVersion}";
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
      home-manager.url = "github:nix-community/home-manager/master";
      home-manager.inputs.nixpkgs.follows = "nixpkgs";
      musnix.url = "github:musnix/musnix";
      emacsOverlay.url = "github:nix-community/emacs-overlay";
    };

  outputs = { nixpkgs, home-manager, musnix, emacsOverlay, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };

      lib = nixpkgs.lib;

    in
    {
      homeManagerConfigs = {
        phaaze = home-manager.lib.homeManagerConfiguration {
          # pkgs = nixpkgs.legacyPackages.${system};
          pkgs = import nixpkgs {
            overlays = [emacsOverlay.overlay];
            inherit system;
          };

          modules = [
            # ({ pkgs, ... }: {
            #   nixpkgs.overlays = [ emacsOverlay.overlay ];
            # })
            ./phaaze/home.nix
            # {
            #   home = {
            #     username = "thomas";
            #     homeDirectory = "/home/thomas";
            #   };
            # }
          ];
        };
      };

      nixosConfigurations = {
        phaaze = lib.nixosSystem {
          inherit system;

          modules = [
            musnix.nixosModules.musnix
            ./phaaze/configuration.nix
          ];
        };
      };
    };
}
