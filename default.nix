{ pkgs ? import ./nix/nixpkgs.nix { }
, compiler ? "ghc865"
}:

let
  inherit (import ./nix/gitignoreSource.nix { inherit (pkgs) lib; }) gitignoreSource;
in
  pkgs.haskell.lib.overrideCabal
    (pkgs.haskell.packages.${compiler}.callPackage ./yesod-middleware-csp.nix {}) (drv: {
      src = gitignoreSource ./.;
      configureFlags = ["-f-library-only"];
      doCheck = false;
      testHaskellDepends = [];
      testToolDepends = [];
      doHaddock = false;
      enableLibraryProfiling = false;
      enableSeparateDataOutput = false;
      enableSharedExecutables = false;
      isLibrary = false;
      postFixup = "rm -rf $out/lib $out/nix-support $out/share/doc";
    })
