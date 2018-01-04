let
  patchRepo = nixpkgs.fetchFromGitHub {
                   owner = "mpickering";
                   repo = "head.hackage";
                   rev = "767dcf11b367ccff4a9fcd55df9c2432cd485fbe";
                   sha256 = "1cdrcw7grpc2dyxnb7a5bg9mi1h7qnblcibi91s348034zf7a0vj";};

  patchDir = "${patchRepo}/patches";
  patchScript = "${patchRepo}/scripts/overrides.nix";

  localOverides = sel: sup:
    { mkDerivation = drv: sup.mkDerivation
                            ( drv // { jailbreak = true; doHaddock = false;});
      generic-deriving = nixpkgs.haskell.lib.dontCheck sup.generic-deriving;
      cabal-doctest = sup.callHackage "cabal-doctest" "1.0.3" {};
      haskell-src-exts = sup.callHackage "haskell-src-exts" "1.20.1" {};
      tasty = nixpkgs.haskell.lib.addBuildDepend sup.tasty sel.semigroups;

    };

  overlay = self: super:
          {

            # An attribute which contains the head overrides.
            # Should put some urls here
            patches = super.callPackage patchScript
              { patches = patchDir; };


            ghc802 = self.haskell.packages.ghc802;
            ghc822 = self.haskell.packages.ghc822;

            # A modified package set intended to be used with ghcHEAD
            ghcHEAD =
            (super.haskell.packages.ghcHEAD.extend
              localOverides).extend (
                (super.callPackage self.patches {}));




            all-cabal-hashes = super.fetchurl {
            url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e1089e56e666c2a0fe82f840d3cc7f49b9c9fe9b.tar.gz";
            sha256 = "0qbzdngm4q8cmwydnrg7jvipw39nb1mjxw95vw6f789874002kn1";
            };
          };

  nixpkgs = import ((import <nixpkgs> { }).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "0a07a5250682a8c24d7c926589f33818475ba3b9";
  sha256 = "02ssdifbi4g8kw3b2a153zmmbihbj14mzw3samnfj37bqvlw0f6s";
}) { overlays = [overlay]; };
in
  { pkgSet ? "ghc822" }:
  nixpkgs.${pkgSet}.callCabal2nix "generic-lens" ./. {}


