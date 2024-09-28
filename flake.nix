{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    rerefined.url   = "github:raehik/rerefined";
    rerefined.flake = false;
    bytezap.url   = "github:raehik/bytezap";
    bytezap.flake = false;
    flatparse.url   = "github:AndrasKovacs/flatparse";
    flatparse.flake = false;
    strongweak.url   = "github:raehik/strongweak";
    strongweak.flake = false;
    generic-data-functions.url   = "github:raehik/generic-data-functions";
    generic-data-functions.flake = false;
    symparsec.url   = "github:raehik/symparsec";
    symparsec.flake = false;
    singleraeh.url   = "github:raehik/singleraeh";
    singleraeh.flake = false;
    type-level-show.url   = "github:raehik/type-level-show";
    type-level-show.flake = false;
    generic-type-asserts.url   = "github:raehik/generic-type-asserts";
    generic-type-asserts.flake = false;
    generic-type-functions.url   = "github:raehik/generic-type-functions";
    generic-type-functions.flake = false;
    type-level-bytestrings.url   = "github:raehik/type-level-bytestrings";
    type-level-bytestrings.flake = false;
  };
  outputs = inputs:
  let
    defDevShell = compiler: {
      mkShellArgs.name = "${compiler}";
      hoogle = false;
      tools = _: {
        haskell-language-server = null;
        hlint = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc98-binrep;
        devShells.default = self'.devShells.ghc98;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          settings.strongweak.broken = false;
          settings.text-icu.check = false; # 2025-09-25: one test fails???
          packages.bytezap.source = inputs.bytezap;
          devShell = defDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          settings.strongweak.broken = false;
          packages.bytezap.source = inputs.bytezap;
          devShell = defDevShell "ghc96";
        };
      };
    };
}
