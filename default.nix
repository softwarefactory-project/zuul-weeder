{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, algebraic-graphs, base, dhall, lens
      , lib, mtl, tasty, tasty-hunit
      }:
      mkDerivation {
        pname = "zuul-weeder";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson algebraic-graphs base dhall lens mtl
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base tasty tasty-hunit ];
        homepage = "https://github.com/softwarefactory-project/zuul-weeder#readme";
        description = "Detect dead configuration in Zuul";
        license = lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
