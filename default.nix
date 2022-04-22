let
  pkgs = import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/4d60081494259c0785f7e228518fee74e0792c1b.tar.gz";
    sha256 = "sha256:15vxvzy9sxsnnxn53w2n44vklv7irzxvqv8xj9dn78z9zwl17jhq";
  }) { config.allowBroken = true; };

  drv = pkgs.haskellPackages.callCabal2nix "zuul-weeder" ./. { };

  shellDrv = pkgs.haskellPackages.shellFor { packages = p: [ drv ]; };

in if pkgs.lib.inNixShell then shellDrv else drv
