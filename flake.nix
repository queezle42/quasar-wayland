{
  inputs = {
    quasar.url = "github:queezle42/quasar";

    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
      follows = "quasar/nixpkgs";
    };
  };

  outputs = { self, nixpkgs, quasar }:
  with nixpkgs.lib;
  let
    systems = platforms.unix;
    forAllSystems = fn: (genAttrs systems (system:
      fn (import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.default
          quasar.overlays.default
        ];
      })
    ));
    getHaskellPackages = pkgs: pattern: pipe pkgs.haskell.packages [
      attrNames
      (filter (x: !isNull (strings.match pattern x)))
      (sort (x: y: x>y))
      (map (x: pkgs.haskell.packages.${x}))
      head
    ];
  in {
    packages = forAllSystems (pkgs:
    let
      haskellPackages = getHaskellPackages pkgs "ghc94.";
    in rec {
      default = quasar-wayland-examples;
      quasar-wayland = haskellPackages.quasar-wayland;
      quasar-wayland-examples = haskellPackages.quasar-wayland-examples;
      quasar-wayland-gles = haskellPackages.quasar-wayland-gles;
      quasar-wayland-skia = haskellPackages.quasar-wayland-skia;
    }
    );

    overlays = {
      default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
            quasar-wayland = hfinal.callCabal2nix "quasar-wayland" ./quasar-wayland {};
            quasar-wayland-examples = hfinal.callCabal2nix "quasar-wayland-examples" ./examples {};
            quasar-wayland-gles =
              final.haskell.lib.overrideCabal
                (hfinal.callCabal2nix "quasar-wayland-gles" ./quasar-wayland-gles { EGL = null; GLESv2 = null; })
                {
                  librarySystemDepends = [ final.libGL ];
                };
            quasar-wayland-skia = hfinal.callCabal2nix "quasar-wayland-skia" ./quasar-wayland-skia { skia = final.skia_quasar-wayland; };
          };
        };

        skia_quasar-wayland = final.callPackage ./skia {};
      };

      quasar = quasar.overlays.default;
    };

    devShells = forAllSystems (pkgs:
      let
        haskellPackages = getHaskellPackages pkgs "ghc94.";
      in rec {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.quasar-wayland
            hpkgs.quasar-wayland-examples
            hpkgs.quasar-wayland-gles
            hpkgs.quasar-wayland-skia
          ];
          nativeBuildInputs = [
            # On some versions hls requires the same GHC version as the compiled
            # package. Not cached :(
            haskellPackages.haskell-language-server
            pkgs.cabal-install
            pkgs.zsh
            pkgs.entr
            pkgs.ghcid
            pkgs.hlint
          ];
          # Provide libEGL/libGLES2 to ghci (`librarySystemDepends` does not
          # seem to work with `shellFor`. It worked with a shell based on
          # `<package>.env`).
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.libGL ];
        };
      }
    );
  };
}
