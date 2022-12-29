{
  inputs = {
    quasar = {
      url = github:queezle42/quasar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
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
      ghc92 = getHaskellPackages pkgs "ghc92.";
    in rec {
      default = quasar-wayland-examples;
      quasar-wayland = ghc92.quasar-wayland;
      quasar-wayland-examples = ghc92.quasar-wayland-examples;
      quasar-wayland-gles = ghc92.quasar-wayland-gles;
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
            # Due to a ghc bug in 9.4.3 and 9.2.5
            ListLike = final.haskell.lib.dontCheck hprev.ListLike;
          };
        };
      };

      quasar = quasar.overlays.default;
    };

    devShells = forAllSystems (pkgs:
      let
        haskellPackages = getHaskellPackages pkgs "ghc92.";
      in rec {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.quasar-wayland
            hpkgs.quasar-wayland-examples
            hpkgs.quasar-wayland-gles
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
