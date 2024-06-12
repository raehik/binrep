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
          packages.rerefined.source = inputs.rerefined;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.symparsec.source = inputs.symparsec;
          packages.singleraeh.source = inputs.singleraeh;
          packages.type-level-show.source = inputs.type-level-show;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          packages.rerefined.source = inputs.rerefined;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.symparsec.source = inputs.symparsec;
          packages.singleraeh.source = inputs.singleraeh;
          packages.type-level-show.source = inputs.type-level-show;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell.mkShellArgs.name = "ghc96-binrep";
          devShell.tools = _: {
            haskell-language-server = null; # 2024-03-06: broken
          };
        };
        haskellProjects.ghc94 = {
          basePackages = pkgs.haskell.packages.ghc94;
          packages.rerefined.source = inputs.rerefined;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.symparsec.source = inputs.symparsec;
          packages.singleraeh.source = inputs.singleraeh;
          packages.type-level-show.source = inputs.type-level-show;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc94";
        };
        haskellProjects.ghc92 = {
          basePackages = pkgs.haskell.packages.ghc92;
          packages.rerefined.source = inputs.rerefined;
          packages.bytezap.source = inputs.bytezap;
          packages.flatparse.source = inputs.flatparse;
          packages.strongweak.source = inputs.strongweak;
          packages.generic-data-functions.source = inputs.generic-data-functions;
          packages.symparsec.source = inputs.symparsec;
          packages.singleraeh.source = inputs.singleraeh;
          packages.type-level-show.source = inputs.type-level-show;
          packages.generic-type-asserts.source = inputs.generic-type-asserts;
          packages.generic-type-functions.source = inputs.generic-type-functions;
          packages.type-level-bytestrings.source = inputs.type-level-bytestrings;
          devShell = nondevDevShell "ghc92";
        };
      };
    };
}
