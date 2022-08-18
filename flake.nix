# Build the container image using:
#   nix build -L .#containerImage
#   TMPDIR=/tmp/podman podman load < result
{
  description = "Zuul Weeder";
  nixConfig.bash-prompt = "[nix(zuul-weeder)]$ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    hspkgs.url =
      "github:podenv/hspkgs/cc4beb99e1c29a6109a1867c0c3f178b2b27c34c";
    flake-utils.url = "github:numtide/flake-utils";
    tailwind.url = "github:srid/tailwind-haskell";
    tailwind.inputs.nixpkgs.follows = "nixpkgs";
    calligraphy.url = "github:jonascarpay/calligraphy";
    calligraphy.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, hspkgs, flake-utils, tailwind, calligraphy }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = hspkgs.pkgs;
        packageName = "zuul-weeder";

        rev = if self ? rev then
          self.rev
        else
          throw "Refusing to build from a dirty Git tree!";

        python_svg =
          pkgs.python310.withPackages (ps: with ps; [ ps.lxml ps.six ]);
        svg_stack_src = pkgs.fetchurl {
          url =
            "https://raw.githubusercontent.com/astraw/svg_stack/de9872c8c933ff96821fd8bff9d887270051b4b7/svg_stack.py";
          sha256 = "sha256-qFxlnWBKJwA+NQYnr3WWFjgVWh8KquKG007wA7pO9W4=";
        };
        svg_stack = pkgs.writeScriptBin "svg_stack" ''
          #!/bin/sh
          exec ${python_svg}/bin/python3 ${svg_stack_src} $*
        '';

        python = pkgs.python310.withPackages (ps: with ps; [ kazoo ]);

        haskellPackages = pkgs.hspkgs;
        zuulWeederPackage = haskellPackages.callCabal2nix packageName self { };
        finalPackage =
          zuulWeederPackage.overrideAttrs (_: { GIT_COMMIT = rev; });

        distFiles = pkgs.runCommand "copy-dists" { } ''
          mkdir $out
          cp -v ${./dists}/* $out/
        '';

        exe = pkgs.haskell.lib.justStaticExecutables finalPackage;

        mkApp = script: {
          type = "app";
          program =
            builtins.toString (pkgs.writers.writeBash "app-wrapper.sh" script);
        };

      in {
        apps.default = exe;
        packages.default = finalPackage;

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
          GIT_COMMIT = self.rev or "dirty";

          buildInputs = with haskellPackages; [
            python
            pkgs.ghcid
            pkgs.ormolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.weeder
            pkgs.haskell-language-server
            pkgs.graphviz
            svg_stack
          ];
        };

        devShells.hoogle = haskellPackages.shellFor {
          packages = p: [ zuulWeederPackage ];
          withHoogle = true;
        };

        apps.calligraphy = mkApp ''
          set -xe
          exec ${
            calligraphy.apps."x86_64-linux".calligraphy-ghc922
          }/bin/calligraphy $*
        '';

        apps.tailwind = mkApp ''
          set -xe
          exec ${
            tailwind.defaultPackage."x86_64-linux"
          }/bin/tailwind-run -w 'src/ZuulWeeder/UI.hs' -o dists/tailwind.css;
        '';
      });
}
