# Build the container image using:
#   nix build -L .#containerImage
#   TMPDIR=/tmp/podman podman load < result
{
  description = "Zuul Weeder";
  nixConfig.bash-prompt = "[nix(zuul-weeder)] ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    tailwind.url = "github:srid/tailwind-haskell";
    tailwind.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, tailwind }:
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

        python = pkgs.python310.withPackages (ps: with ps; [ kazoo ]);

        haskellPackages =
          pkgs.haskell.packages.ghc922.override haskellOverrides;
        zuulWeederPackage = haskellPackages.callCabal2nix packageName self { };

        distFiles = pkgs.runCommand "copy-dists" { } ''
          mkdir $out
          cp -v ${./dists}/* $out/
        '';

        exe = pkgs.haskell.lib.justStaticExecutables zuulWeederPackage;

      in {
        defaultExe = exe;
        defaultPackage = zuulWeederPackage;

        packages.containerImage = pkgs.dockerTools.buildLayeredImage {
          name = "quay.io/software-factory/zuul-weeder";
          tag = "latest";
          extraCommands = ''
            #!${pkgs.runtimeShell}
            mkdir -p var/tmp/weeder
          '';
          contents = [ exe distFiles python ];
          config = {
            Entrypoint = [ "zuul-weeder" ];
            Env = [ "WEEDER_DIST_PATH=${toString distFiles}" ];
          };
        };

        devShell = haskellPackages.shellFor {
          packages = p: [ zuulWeederPackage ];

          # disable zookeeper in devel mode
          ZUUL_WEEDER_NO_ZK = "1";

          buildInputs = with haskellPackages; [
            python
            ghcid
            ormolu
            cabal-install
            hlint
            pkgs.haskell-language-server
          ];
        };

        apps.tailwind = let
          script = pkgs.writers.writeBash "tailwind-run.sh" ''
            set -xe
            exec ${
              tailwind.defaultPackage."x86_64-linux"
            }/bin/tailwind-run -w 'src/ZuulWeeder/UI.hs' -o dists/tailwind.css;
          '';
        in {
          type = "app";
          program = builtins.toString script;
        };
      });
}
