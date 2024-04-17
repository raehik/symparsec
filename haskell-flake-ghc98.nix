pkgs:

{
  # disable local project options (always do this for package sets)
  defaults.packages = {};
  devShell.enable = false;
  autoWire = [];

  basePackages = pkgs.haskell.packages.ghc98;

  #packages = { #self: super: with pkgs.haskell.lib; {
    #splitmix.source = "0.1.0.5";
    #splitmix = pkgs.haskell.packages.splitmix_0_1_0_5;
    # 2023-04-17 raehik: need hedgehog 1.2 for GHC 9.6
    #hedgehog.source = "1.2";
    #tasty-hedgehog.source = "1.4.0.1";

    # 2023-04-17 raehik: warp: need new for GHC 9.6 (unix-2.8)
    # also has 3 test failures. idk why. disabling
    # also has friends that need swapping out. heck on earth
    #warp.source = "3.3.25";
    #recv.source = "0.1.0";
    #warp-tls.source = "3.3.6";
  #};

  settings = {
    #ed25519 = {
      # TODO: We need revision 8 for GHC 9.8.
      # I manually bumped the Hackage package set in my GHC 9.8 branch of
      # someone else's branch.
      # But `jailbreak = true` should work... but it doesn't? Hrm.
      #jailbreak = true;
    #};

    splitmix = {super, ...}: {
      # TODO: this should work. but it doesn't. splitmix 0.1.0.4 is still used.
      custom = _: super.splitmix_0_1_0_5;
      # 2023-10-19: requires system lib testu01
      #check = false;
      # 2023-11-02: idk try it
      check = false;
    };

    #hourglass = {
      # 2023-04-17 raehik: hourglass tests broken from GHC 9.2.5
      # PR: https://github.com/vincenthz/hs-hourglass/pull/56
      #check = false;
    #};

    #bsb-http-chunked = {
      # 2023-04-17 raehik: bsb-http-chunked: tests broken
      # maybe problematic type wildcard usage...?
      #check = false;
    #};

    #doctest-exitcode-stdio = {
      # 2023-04-26 raehik: weird bug. bad test.
      #jailbreak = true;
    #};

    #warp = {
      #check = false;
    #};

  };

}
