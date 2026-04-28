{
  description = "yesod-middleware-csp - A middleware for building CSP headers on the fly";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;
        pkg = hpkgs.callCabal2nix "yesod-middleware-csp" ./. { };
      in {
        packages.default = pkg;

        devShells.default = hpkgs.shellFor {
          packages = p: [ pkg ];
          buildInputs = [
            hpkgs.cabal-install
            hpkgs.hlint
          ];
        };
      });
}
