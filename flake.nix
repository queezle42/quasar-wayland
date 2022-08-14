{
  inputs = {
    quasar = {
      url = gitlab:jens/quasar?host=git.c3pb.de;
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
        self.overlay
        quasar.overlay
      ]; };
    in rec {
      default = quasar-wayland;
      quasar-wayland = pkgs.haskell.packages.ghc924.quasar-wayland;
    }
    );

    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          quasar-wayland = hfinal.callCabal2nix "quasar-wayland" ./. {};
        };
      };
    };

    overlays = {
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
        ];
      }
    );
  };
}
