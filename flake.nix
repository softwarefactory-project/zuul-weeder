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
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        packageName = "zuul-weeder";
        haskellPackages = pkgs.haskellPackages;
        zuulWeederPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultExe = pkgs.haskell.lib.justStaticExecutables zuulWeederPackage;
        defaultPackage = zuulWeederPackage;

        devShell =  haskellPackages.shellFor {
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
