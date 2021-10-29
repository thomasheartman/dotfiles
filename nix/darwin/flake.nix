{
  description = "Minimal DarwinSystem example";

  inputs = {
    # in real world use: ""
    mk-darwin-system.url = "github:vic/mk-darwin-system/main";
  };

  outputs = { mk-darwin-system, ... }@inputs:
    let
      flake-utils = mk-darwin-system.inputs.flake-utils;
      hostName = "termina";
      systems = [ "aarch64-darwin" ];
    in
      flake-utils.lib.eachSystem systems (
        system:
          mk-darwin-system.mkDarwinSystem {
            inherit hostName system;

            nixosModules = [
              (
                { pkgs, ... }: {
                  system.stateVersion = 4;
                  services.nix-daemon.enable = true;
                  nix.package = pkgs.nixFlakes;
                  nix.extraOptions = ''
                    system = aarch64-darwin
                    extra-platforms = aarch64-darwin x86_64-darwin
                    experimental-features = nix-command flakes
                    build-users-group = nixbld
                  '';

                  networking.hostName = hostName;

                  fonts = {
                    fonts = with pkgs; [
                      dejavu_fonts
                      fira-code
                      fira-code-symbols
                      font-awesome
                      ipafont
                      kochi-substitute
                      mplus-outline-fonts
                      nerdfonts
                      noto-fonts
                      # noto-fonts-emoji # <- doesn't work because of scipy
                      open-sans
                      powerline-fonts
                      siji
                      symbola
                    ];
                  };

                  nixpkgs.config = {
                    allowUnfree = true;
                  };

                  environment.systemPackages = with pkgs; [
                    alacritty
                    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
                    autojump
                    bat
                    bitwarden-cli
                    cachix
                    direnv
                    emacs
                    fish
                    git
                    git-lfs
                    home-manager
                    ispell
                    jetbrains-mono
                    jq
                    nixFlakes
                    nixfmt
                    rnix-lsp
                    ripgrep
                    sd
                    victor-mono
                    vim
                    wally-cli
                    watchexec
                    yaml-language-server
                    # yabai <— fails for some reason

                  ];
                }
              )
            ];

            flakeOutputs = { pkgs, ... }@outputs:
              outputs // (with pkgs; { packages = { inherit hello; }; });

          }
      );
}
