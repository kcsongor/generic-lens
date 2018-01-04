let
  overlay = self: super:
          {
            # An attribute which contains the head overrides.
            # Should put some urls here
            patches = super.callPackage ./head.hackage/scripts/overrides.nix
              { patches = ./head.hackage/patches; };


            ghc802 = super.haskell.packages.ghc802;
            ghc822 = super.haskell.packages.ghc822;


            # A modified package set intented to be used with ghcHEAD
            ghcHEAD = super.haskell.packages.ghcHEAD.override
              { overrides = sel: sup:
                  ((super.callPackage self.patches {} sel sup)
                  // { mkDerivation = drv: sup.mkDerivation
                        ( drv // { jailbreak = true; doHaddock = false;});
                        generic-deriving = super.haskell.lib.dontCheck sup.generic-deriving;
                        cabal2nix = sup.callHackage "cabal2nix" "2.7.2" {};
                       });
                   };

            all-cabal-hashes = super.fetchurl {
            url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e1089e56e666c2a0fe82f840d3cc7f49b9c9fe9b.tar.gz";
            sha256 = "0qbzdngm4q8cmwydnrg7jvipw39nb1mjxw95vw6f789874002kn1";
            };
          };

  nixpkgs = import <nixpkgs> { overlays = [overlay]; };
in
  { pkgSet ? "ghc802" }:
  nixpkgs.${pkgSet}.callCabal2nix "generic-lens" ./. {}


