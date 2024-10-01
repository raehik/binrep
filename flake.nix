{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    rerefined.url   = "github:raehik/rerefined";
    rerefined.flake = false;
    bytezap.url   = "github:raehik/bytezap";
    bytezap.flake = false;
    strongweak.url   = "github:raehik/strongweak";
    strongweak.flake = false;
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
        haskellProjects.ghc910 = {
          basePackages = pkgs.haskell.packages.ghc910;
          devShell = defDevShell "ghc910";

          #settings.text-icu.check = false; # 2025-09-25: one test fails???

          packages.finite-typelits.source = "0.2.1.0";
          packages.vector-sized.source = "1.6.1";

          # https://github.com/phadej/defun/pull/5
          settings.defun-core.jailbreak = true;

          # waiting on nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
          packages.bytezap.source = inputs.bytezap;
        };

        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = defDevShell "ghc98";

          # waiting on nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
          packages.bytezap.source = inputs.bytezap;
        };

        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = defDevShell "ghc96";

          # waiting on nixpkgs update
          packages.rerefined.source = inputs.rerefined;
          settings.strongweak.broken = false;
          packages.strongweak.source = inputs.strongweak;
          packages.bytezap.source = inputs.bytezap;
        };
      };
    };
}
