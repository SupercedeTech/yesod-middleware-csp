{ mkDerivation, base, base64-bytestring, bytestring
, case-insensitive, classy-prelude, classy-prelude-yesod, conduit
, containers, data-default, directory, fast-logger, filepath, hspec
, http-client, http-types, monad-logger, network-uri, stdenv
, template-haskell, text, time, uuid, wai-extra, yesod, yesod-core
, yesod-static, yesod-test
}:
mkDerivation {
  pname = "yesod-middleware-csp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring classy-prelude conduit containers
    data-default directory filepath http-client network-uri
    template-haskell text time uuid yesod yesod-core yesod-static
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring case-insensitive classy-prelude
    classy-prelude-yesod conduit containers data-default directory
    fast-logger filepath hspec http-types monad-logger network-uri
    template-haskell text uuid wai-extra yesod yesod-core yesod-static
    yesod-test
  ];
  description = "A middleware for building CSP headers on the fly";
  license = stdenv.lib.licenses.mit;
}
