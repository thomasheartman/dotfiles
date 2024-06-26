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
      # muse sounds manager might just be temporary. Refer to
      # https://github.com/NixOS/nixpkgs/issues/216432#issuecomment-1942809581
      muse-sounds-manager.url = "github:thilobillerbeck/muse-sounds-manager-nix";
    };

  outputs = { nixpkgs, home-manager, musnix, muse-sounds-manager, ... }@inputs:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowUnfree = true;
        };
      };

      lib = nixpkgs.lib;

      hmConfig = hostname: home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};

        extraSpecialArgs = inputs;

        modules = [
          ({
            nixpkgs.config.allowUnfreePredicate = (pkg: true);
          })
          ./${hostname}/home.nix
          {
            home = {
              username = "thomas";
              homeDirectory = "/home/thomas";
              stateVersion = "22.11";
            };
          }
        ];
      };

      nixosConfig = hostname: lib.nixosSystem {
        inherit system;

        modules = [
          musnix.nixosModules.musnix
          ./${hostname}/configuration.nix
        ];
      };

    in
    {
      homeManagerConfigs = lib.attrsets.genAttrs [ "phaaze" "skytown" ] hmConfig;

      nixosConfigurations = lib.attrsets.genAttrs [ "phaaze" "skytown" ] nixosConfig;
    };
}
