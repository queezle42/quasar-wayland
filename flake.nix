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
    forAllSystems = genAttrs systems;
    getHaskellPackages = pkgs: pattern: pipe pkgs.haskell.packages [
      attrNames
      (filter (x: !isNull (strings.match pattern x)))
      (sort (x: y: x>y))
      (map (x: pkgs.haskell.packages.${x}))
      head
    ];
  in {
    packages = forAllSystems (system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [
        self.overlays.default
        quasar.overlays.default
      ]; };
      ghc92 = getHaskellPackages pkgs "ghc92.";
    in rec {
      default = quasar-wayland-examples;
      quasar-wayland = ghc92.quasar-wayland;
      quasar-wayland-examples = ghc92.quasar-wayland-examples;
    }
    );

    overlays = {
      default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
            quasar-wayland-examples = hfinal.callCabal2nix "quasar-wayland-examples" ./examples {};
            quasar-wayland = hfinal.callCabal2nix "quasar-wayland" ./quasar-wayland {};
          };
        };
      };

      quasar = quasar.overlay;
    };

    devShells = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [
          self.overlays.default
          quasar.overlays.default
        ]; };
        haskellPackages = getHaskellPackages pkgs "ghc92.";
      in rec {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.quasar-wayland
            hpkgs.quasar-wayland-examples
          ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.zsh
            pkgs.entr
            pkgs.ghcid
            pkgs.haskell-language-server
            pkgs.hlint
          ];
        };
      }
    );
  };
}
