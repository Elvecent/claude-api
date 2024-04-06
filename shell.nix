{ nixpkgs ? <nixpkgs>, compiler ? "ghc948", withHoogle ? false}:

let
  claude-api = pkgs.haskellPackages.callCabal2nix "claude-api" ./. {};
  overlay = self: super: {
    haskellPackages = pkgs.haskell.packages.${compiler}.override (old: {
      overrides = self: super: {
      };
    });
  };
  pkgs = import nixpkgs { overlays = [ overlay ]; };
  shellLocal = if builtins.pathExists ./shell-local.nix then import ./shell-local.nix else {};
in
pkgs.haskellPackages.shellFor {
  inherit withHoogle;
  packages = _: [ claude-api ];
  buildInputs = with pkgs.haskellPackages; [ haskell-language-server pkgs.nodejs ];
  inherit (shellLocal) shellHook;
}
