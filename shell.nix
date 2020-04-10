let compiler = "ghc865";
    pkgs = import ./nixpkgs.nix {};
    hpkgs = pkgs.haskell.packages.${compiler};
 in (hpkgs.callPackage (import ./yesod-middleware-csp.nix) {}).env
