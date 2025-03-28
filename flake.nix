# Build the container image using:
#   $(nix build --no-link --print-out-paths -L .#container) | gzip --fast | podman load
{
  description = "Zuul Weeder";
  nixConfig.bash-prompt = "[nix(zuul-weeder)]$ ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d3780c92e64472e8f9aa54f7bbb0dd4483b98303";
    flake-utils.url = "github:numtide/flake-utils";
    tailwind.url = "github:srid/tailwind-haskell";
    tailwind.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, tailwind }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
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

        haskellPackages = pkgs.haskellPackages.extend (hpFinal: hpPrev: {
          # there is a test failure: resolveGroupController should resolve a direct mount root
          cgroup-rts-threads = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.cgroup-rts-threads {
              broken = false;
            });
        });
        zuulWeederPackage = haskellPackages.callCabal2nix packageName self { };
        finalPackage =
          zuulWeederPackage.overrideAttrs (_: { GIT_COMMIT = rev; });

        distFiles = pkgs.runCommand "copy-dists" { } ''
          mkdir $out
          cp -v ${./dists}/* $out/
        '';

        exe = pkgs.haskell.lib.justStaticExecutables finalPackage;

        container-name = "ghcr.io/softwarefactory-project/zuul-weeder";
        container = pkgs.dockerTools.streamLayeredImage {
          name = container-name;
          tag = "latest";
          created = "now";
          extraCommands = ''
            #!${pkgs.runtimeShell}
            mkdir -p var/tmp/weeder
          '';
          contents = [ exe distFiles python ];
          config = {
            Cmd = [ "zuul-weeder" ];
            Env = [ "WEEDER_DIST_PATH=${toString distFiles}" ];
          };
        };
        publish-container-release =
          pkgs.writeShellScriptBin "container-release" ''
            set -e
            export PATH=$PATH:${pkgs.gzip}/bin:${pkgs.skopeo}/bin
            IMAGE="docker://${container-name}"

            echo "Logging to registry..."
            echo $GH_TOKEN | skopeo login --username $GH_USERNAME --password-stdin ghcr.io

            echo "Building and publishing the image..."
            ${container} | gzip --fast | skopeo copy docker-archive:/dev/stdin $IMAGE:${exe.version}

            echo "Tagging latest"
            skopeo copy $IMAGE:${exe.version} $IMAGE:latest
          '';

        mkApp = script: {
          type = "app";
          program =
            builtins.toString (pkgs.writers.writeBash "app-wrapper.sh" script);
        };

      in {
        apps.default = exe;
        packages.default = finalPackage;

        packages.container = container;
        apps.publish-container-release =
          flake-utils.lib.mkApp { drv = publish-container-release; };
        devShell = haskellPackages.shellFor {
          packages = p: [ zuulWeederPackage ];

          # disable zookeeper in devel mode
          ZUUL_WEEDER_NO_ZK = "1";
          GIT_COMMIT = self.rev or "dirty";

          buildInputs = with haskellPackages; [
            python
            pkgs.ghcid
            pkgs.haskellPackages.fourmolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskellPackages.weeder
            pkgs.haskellPackages.haskell-language-server
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
          exec ${pkgs.haskellPackages.calligraphy}/bin/calligraphy $*
        '';

        apps.tailwind = mkApp ''
          set -xe
          exec ${
            tailwind.defaultPackage."x86_64-linux"
          }/bin/tailwind-run -w 'src/ZuulWeeder/UI.hs' -o dists/tailwind.css;
        '';
      });
}
