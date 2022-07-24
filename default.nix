{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, args ? {} }:

let
  #quasar-wayland = haskellPackages.callCabal2nix "quasar-wayland" ./. args;
  quasar-wayland = pkgs.haskell.packages.ghc922.callCabal2nix "quasar-wayland" ./. args;

in
  if pkgs.lib.inNixShell then quasar-wayland.env else quasar-wayland
