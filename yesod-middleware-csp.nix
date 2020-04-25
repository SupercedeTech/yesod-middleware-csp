{ mkDerivation, base, base64-bytestring, bytestring
, case-insensitive, classy-prelude, classy-prelude-yesod
, containers, fast-logger, hspec, http-client, http-types
, monad-logger, network-uri, stdenv, text, time, uuid, wai-extra
, yesod, yesod-core, yesod-static, yesod-test
}:
mkDerivation {
  pname = "yesod-middleware-csp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64-bytestring bytestring classy-prelude containers
    http-client network-uri text time uuid yesod yesod-core
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring case-insensitive classy-prelude
    classy-prelude-yesod containers fast-logger hspec http-types
    monad-logger network-uri text uuid wai-extra yesod yesod-core
    yesod-static yesod-test
  ];
  description = "A middleware for building CSP headers on the fly";
  license = stdenv.lib.licenses.mit;
}
