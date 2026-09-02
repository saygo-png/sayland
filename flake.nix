{
  inputs = {
    treefmt-nix.url = "github:numtide/treefmt-nix";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
    self,
    ...
  }: let
    pkgsFor = nixpkgs.lib.genAttrs (import systems) (system: import nixpkgs {inherit system;});
    eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f system pkgsFor.${system});
  in {
    homeManagerModules.default = self.homeManagerModules.drugtracker2;
    homeManagerModules.drugtracker2 = (import ./home-manager.nix) niceHaskell;

    packages = eachSystem (system: pkgs: let
      program = pkgs.callPackage ./package.nix {niceHaskell = niceHaskell.outputs.niceHaskell.${system};};
    in {
      "sayland" = program;
      default = program;
    });

    formatter = eachSystem (_system: pkgs: (treefmt-nix.lib.evalModule pkgs ./treefmt.nix).config.build.wrapper);

    devShells = eachSystem (_system: pkgs: {
      default = pkgs.mkShell {
        packages = let
          ghcPackages = pkgs.haskell.packages.ghc912;
        in [
          pkgs.zlib
          ghcPackages.cabal-install
          ghcPackages.ghc
          ghcPackages.haskell-language-server
        ];
      };
    });
  };
}
