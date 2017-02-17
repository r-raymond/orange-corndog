let
  pkgs = import <nixpkgs> { };
in
  { orange-corndog = pkgs.haskellPackages.callPackage ./default.nix { };
  }
