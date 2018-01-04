let
  overlay = self: super:
          {
            # An attribute which contains the head overrides.
            # Should put some urls here
            patches = super.callPackage ./head.hackage/scripts/overrides.nix
              { patches = ./head.hackage/patches; };


            ghc802 = self.haskell.packages.ghc802;
            ghc822 = self.haskell.packages.ghc822;

            # A modified package set intented to be used with ghcHEAD
            ghcHEAD = super.haskell.packages.ghcHEAD.override
              { overrides = sel: sup:
                  ((super.callPackage self.patches {} sel sup)
                  // { mkDerivation = drv: sup.mkDerivation
                        ( drv // { jailbreak = true; doHaddock = false;});
                        generic-deriving = super.haskell.lib.dontCheck sup.generic-deriving;
                       });
                   };

            all-cabal-hashes = super.fetchurl {
            url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e1089e56e666c2a0fe82f840d3cc7f49b9c9fe9b.tar.gz";
            sha256 = "0qbzdngm4q8cmwydnrg7jvipw39nb1mjxw95vw6f789874002kn1";
            };
          };

  nixpkgs = import ((import <nixpkgs> { }).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "3cfa038870cb2f6017d1a3148aba6c83480eec62";
  sha256 = "0v4gi55bmk70ynml4in765m4zq832g34lb52pgsvfyi4cg9f4vv3";
}) { overlays = [overlay]; };
in
  { pkgSet ? "ghc822" }:
  nixpkgs.${pkgSet}.callCabal2nix "generic-lens" ./. {}


