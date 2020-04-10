{ mkDerivation, base, classy-prelude, classy-prelude-yesod
, fast-logger, hspec, http-client, monad-logger, network-uri
, stdenv, time, ua-parser, uuid, yesod, yesod-core, yesod-test
}:
mkDerivation {
  pname = "yesod-middleware-csp";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base http-client network-uri time ua-parser uuid yesod yesod-core
  ];
  testHaskellDepends = [
    base classy-prelude classy-prelude-yesod fast-logger hspec
    monad-logger network-uri yesod yesod-core yesod-test
  ];
  description = "A middleware for building CSP headers on the fly";
  license = stdenv.lib.licenses.mit;
}
