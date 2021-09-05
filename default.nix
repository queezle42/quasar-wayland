{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, args ? {} }:

let
  quasar-wayland = haskellPackages.callCabal2nix "quasar-wayland" ./. args;

in
  if pkgs.lib.inNixShell then quasar-wayland.env else quasar-wayland
