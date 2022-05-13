{
  description = "Zuul Weeder";
  nixConfig.bash-prompt = "[nix(zuul-weeder)] ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        packageName = "zuul-weeder";

        haskellOverrides = {
          overrides = hpFinal: hpPrev:
            let
              mk-servant-lib = hpPrev: name:
                let
                  # Use direct source because nixpkgs somehow can't fetch
                  servant-src = builtins.fetchGit {
                    url = "https://github.com/haskell-servant/servant";
                    ref = "master";
                    rev = "c19ed0fb925fbe62365adcaf286c00c497adf8fb";
                  };
                in (hpPrev.callCabal2nix "sevant${name}"
                  "${servant-src}/servant${name}" { });

            in {
              # nixpkgs servant somehow doesn't fetch, use direct src
              servant = mk-servant-lib hpPrev "";
              servant-server = mk-servant-lib hpPrev "-server";
            };
        };

        haskellPackages =
          pkgs.haskell.packages.ghc922.override haskellOverrides;
        zuulWeederPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables zuulWeederPackage;
        defaultPackage = zuulWeederPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ zuulWeederPackage ];

          buildInputs = with haskellPackages; [
            ghcid
            ormolu
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
