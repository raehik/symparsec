{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
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
        packages.default  = self'.packages.ghc98-symparsec;
        devShells.default = self'.devShells.ghc98;
        haskellProjects.ghc910 = {
          basePackages = pkgs.haskell.packages.ghc910;
          devShell = defDevShell "ghc910";

          # https://github.com/phadej/defun/pull/5
          settings.defun-core.jailbreak = true;
        };
        haskellProjects.ghc98 = {
          basePackages = pkgs.haskell.packages.ghc98;
          devShell = defDevShell "ghc98";
        };
        haskellProjects.ghc96 = {
          basePackages = pkgs.haskell.packages.ghc96;
          devShell = defDevShell "ghc96";
        };
      };
    };
}
