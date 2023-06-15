# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "yesod-middleware-csp";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskell.packages.ghc944.override {
        overrides = hnew: hold: {
          yesod-middleware-csp = hnew.callCabal2nix "yesod-middleware-csp" ./. { };
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.yesod-middleware-csp;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."yesod-middleware-csp" ];
        withHoogle = true;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
