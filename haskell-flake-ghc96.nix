pkgs:

{
  # 2023-05-10 raehik: always use these for package sets. otherwise things get
  # weird (overrides might not work)
  packages = {};
  devShell.enable = false;
  autoWire = [];

  basePackages = pkgs.haskell.packages.ghc96;

  overrides = self: super: with pkgs.haskell.lib; {
    # 2023-04-17 raehik: hourglass tests broken from GHC 9.2.5
    # PR: https://github.com/vincenthz/hs-hourglass/pull/56
    hourglass = dontCheck super.hourglass;

    # 2023-04-17 raehik: need hedgehog 1.2 for GHC 9.6
    hedgehog = super.hedgehog_1_2;
    tasty-hedgehog = super.tasty-hedgehog_1_4_0_1;

    # 2023-04-17 raehik: retry: tests broken
    # PR: https://github.com/Soostone/retry/pull/82
    retry = dontCheck super.retry;

    # 2023-04-17 raehik: warp: need new for GHC 9.6 (unix-2.8)
    # also has 3 test failures. idk why. disabling
    # also has friends that need swapping out. heck on earth
    warp = dontCheck super.warp_3_3_25;
    recv = super.recv_0_1_0;
    warp-tls = super.warp-tls_3_3_6;

    # 2023-04-17 raehik: bsb-http-chunked: tests broken
    # maybe problematic type wildcard usage...?
    bsb-http-chunked = dontCheck super.bsb-http-chunked;

    # 2023-04-17 raehik: finite-typelits needs base upper bound bump
    # idk how to do a Cabal file patch. so jailbreak it is.
    # PR: https://github.com/mniip/finite-typelits/pull/22
    # 2023-04-26 raehik: 0.1.6.0 rev 1 works. waiting on nixpkgs bump.
    finite-typelits = doJailbreak super.finite-typelits;

    # 2023-04-26 raehik: weird bug. bad test.
    doctest-exitcode-stdio = doJailbreak super.doctest-exitcode-stdio;
  };

}
