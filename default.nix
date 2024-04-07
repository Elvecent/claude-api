{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "claude-api" ./. {}
