let compiler = "ghc902";
    pkgs = import ./nix/nixpkgs.nix {};
    hpkgs = pkgs.haskell.packages.${compiler};
 in (hpkgs.callPackage (import ./yesod-middleware-csp.nix) {}).env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      hpkgs.hlint
      hpkgs.cabal-install
    ];
  })
