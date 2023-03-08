{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flatparse = {
      url = "github:AndrasKovacs/flatparse";
      flake = false;
    };
    strongweak = {
      url = "github:raehik/strongweak/refined1";
      flake = false;
    };
    refined = {
      url = "github:raehik/refined/refined1";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        packages.default = self'.packages.binrep;
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc94;
          overrides = self: super: with pkgs.haskell.lib; {
            flatparse = self.callCabal2nix "flatparse" inputs.flatparse {};
            strongweak = self.callCabal2nix "strongweak" inputs.strongweak {};
            refined = self.callCabal2nix "refined" inputs.refined {};
          };
          devShell = {
            tools = hp: {
              ghcid = null; # ghcid broken on Nix for GHC 9.4
              haskell-language-server = null; # needs a massive system to build
            };
          };
        };
      };
    };
}
