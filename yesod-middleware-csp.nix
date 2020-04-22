{ mkDerivation, base, base64-bytestring, bytestring
, case-insensitive, classy-prelude, classy-prelude-yesod
, containers, fast-logger, hspec, http-client, http-conduit
, http-types, monad-logger, network-uri, stdenv, text, time
, ua-parser, uuid, wai-extra, yesod, yesod-core, yesod-static
, yesod-test
}:
mkDerivation {
  pname = "yesod-middleware-csp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring classy-prelude containers
    http-client http-conduit network-uri text time ua-parser uuid yesod
    yesod-core
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring case-insensitive classy-prelude
    classy-prelude-yesod containers fast-logger hspec http-conduit
    http-types monad-logger network-uri text ua-parser uuid wai-extra
    yesod yesod-core yesod-static yesod-test
  ];
  description = "A middleware for building CSP headers on the fly";
  license = stdenv.lib.licenses.mit;
}
