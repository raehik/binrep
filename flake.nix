{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    bytezap.url = "github:raehik/bytezap";
    bytezap.flake = false;
    flatparse.url = "github:AndrasKovacs/flatparse";
    flatparse.flake = false;
  };
  outputs = inputs:
  let
    # simple devshell for non-dev compilers: really just want `cabal repl`
    nondevDevShell = compiler: {
      mkShellArgs.name = "${compiler}-binrep";
      hoogle = false;
      tools = _: {
        hlint = null;
        haskell-language-server = null;
        ghcid = null;
      };
    };
  in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default  = self'.packages.ghc96-binrep;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          devShell.mkShellArgs.name = "ghc96-binrep";
          devShell.tools = _: {
            haskell-language-server = null; # 2024-03-06: broken
          };
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
