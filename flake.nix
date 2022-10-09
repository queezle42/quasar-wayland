{
  inputs = {
    quasar = {
      url = github:queezle42/quasar;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  };

  outputs = { self, nixpkgs, quasar }:
  let
    lib = nixpkgs.lib;
    systems = lib.platforms.unix;
    forAllSystems = lib.genAttrs systems;
  in {
    packages = forAllSystems (system:
    let pkgs = import nixpkgs { inherit system; overlays = [
        self.overlays.default
        quasar.overlays.default
      ]; };
    in rec {
      default = quasar-wayland;
      quasar-wayland = pkgs.haskell.packages.ghc924.quasar-wayland;
    }
    );

    overlays = {
      default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
            quasar-wayland = hfinal.callCabal2nix "quasar-wayland" ./. {};
          };
        };
      };

      quasar = quasar.overlay;
    };

    devShell = forAllSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.quasar-wayland.env ];
        packages = [
          pkgs.cabal-install
          pkgs.zsh
          pkgs.entr
          pkgs.ghcid
          pkgs.haskell-language-server
          pkgs.hlint
        ];
      }
    );
  };
}
