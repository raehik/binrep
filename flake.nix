{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
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
        haskellProjects.ghc96 = {
          # 2023-11-14: GHC 9.6 base package set is borked
          # PR: https://github.com/NixOS/nixpkgs/pull/267477
          basePackages = pkgs.haskell.packages.ghc96.override {
            overrides = self: super: {
              fgl = self.fgl_5_8_2_0;
              th-desugar = self.th-desugar_1_16;
            };
          };
          devShell.mkShellArgs.name = "ghc96-binrep";
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
