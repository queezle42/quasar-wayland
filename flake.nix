{
  inputs = {
    quasar-network = {
      url = gitlab:jens/quasar-network?host=git.c3pb.de;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, quasar-network }:
  let
    lib = nixpkgs.lib;
    systems = lib.platforms.unix;
    forAllSystems = lib.genAttrs systems;
  in {
    packages = forAllSystems (system:
    let pkgs = import nixpkgs { inherit system; overlays = [
        self.overlay
        quasar-network.overlay
        quasar-network.overlays.quasar
      ]; };
    in {
      inherit (pkgs.haskellPackages) quasar-wayland;
    }
    );

    overlay = self: super: {
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: super.haskell.packageOverrides hself hsuper // {
          quasar-wayland = import ./. { pkgs = self; haskellPackages = hself; };
        };
      };
    };

    overlays = {
      quasar = quasar-network.overlays.quasar;
      quasar-network = quasar-network.overlay;
    };

    defaultPackage = forAllSystems (system: self.packages.${system}.quasar-wayland);

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
