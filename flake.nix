{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.crane.url = "github:ipetkov/crane";
  outputs = { self, nixpkgs, flake-utils, haskellNix, crane }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        haskell = pkgs.hixProject.flake { };
        rust = crane.mkLib pkgs;
      in
      {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            haskell.devShells.default
            (rust.devShell { })
          ];
        };
        packages = {
          haskell = haskell.packages."aoc:exe:aoc";
          rust = rust.buildPackage { src = rust.cleanCargoSource ./rust; };
        };
      }
    );
}
