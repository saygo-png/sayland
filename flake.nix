{
  inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "";
      };
    };
    niceHaskell = {
      url = "github:saygo-png/nice-nixpkgs-haskell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
    systems = {
      url = "path:./systems.nix";
      flake = false;
    };
  };

  outputs = {
    nixpkgs,
    systems,
    niceHaskell,
    treefmt-nix,
    git-hooks,
    self,
    ...
  }: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system: import nixpkgs {inherit system;});
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});
  in {
    packages = eachSystem (system: pkgs: let
      sayland = pkgs.callPackage ./package.nix {niceHaskell = niceHaskell.outputs.niceHaskell.${system};};
    in {
      inherit sayland;
      default = sayland;
    });

    checks = eachSystem (system: pkgs: {
      pre-commit-check = git-hooks.lib.${system}.run {
        package = pkgs.prek;
        src = ./.;
        hooks.custom-treefmt = {
          enable = true;
          entry = "treefmt";
          package = self.formatter.${system};
        };
      };
    });

    formatter = eachSystem (_system: pkgs: (treefmt-nix.lib.evalModule pkgs ./treefmt.nix).config.build.wrapper);

    devShells = eachSystem (system: pkgs: let
      ghcPackages = pkgs.haskell.packages.ghc912;
    in {
      default = pkgs.mkShell {
        shellHook = ''
          ${self.checks.${system}.pre-commit-check.shellHook}
        '';
        packages = [
          self.formatter.${system}
          pkgs.zlib
          pkgs.cabal-install # Uses pkgs as Cabal don't care about ghc and its cached in nixpkgs.
          ghcPackages.ghc
          ghcPackages.haskell-language-server
        ];
      };

      # Minimum for building docs. For CI.
      docs = pkgs.mkShell {
        packages = [
          pkgs.cabal-install
          ghcPackages.ghc
        ];
      };
    });
  };
}
