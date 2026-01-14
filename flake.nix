{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , haskellNix
    , crane
    , rust-overlay
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [
        haskellNix.overlay
        (final: _prev: {
          hixProject =
            final.haskell-nix.hix.project {
              src = ./.;
              evalSystem = "x86_64-linux";
              name = "aoc";
              compiler-nix-name = "ghc912";
              shell.tools.cabal = "latest";
              shell.tools.hlint = "latest";
              shell.tools.haskell-language-server = "latest";
            };
        })
        (import rust-overlay)
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      haskell = pkgs.hixProject.flake { };
      rust = (crane.mkLib pkgs).overrideToolchain (p: p.rust-bin.selectLatestNightlyWith (
        toolchain: toolchain.default.override {
          extensions = [ "rust-src" ];
          targets = [ "x86_64-unknown-linux-gnu" ];
        }
      ));
    in
    {
      devShells.default = pkgs.mkShell {
        inputsFrom = [
          haskell.devShells.default
          # TODO weirdly, this (sometimes?) causes some HLS /tmp error, only on Fry
          # thankfully, I've so far been able to comment it out, rebuild, run HLS on command line, then bring it back
          (rust.devShell { })
        ];
        packages = with pkgs; [
          bacon
          ghcid
        ];
      };
      packages = {
        haskell = haskell.packages."aoc:exe:aoc";
        rust = rust.buildPackage { src = rust.cleanCargoSource ./rust; };
      };
    }
    );
}
