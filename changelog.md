# Revision history for `yesod-middleware-csp`

This format is based on [Keep A Changelog](https://keepachangelog.com/en/1.0.0).

## 1.3.0 - 2026-04-28

+ Drop dependency on classy-prelude; use standard Prelude and explicit imports
+ Loosen dependency bounds to support GHC 9.6 through 9.10
+ Switch build tooling from Stack to Nix flakes
+ Fix test suite to depend on the library instead of recompiling sources
+ Bump cabal-version to 2.4

## 1.2.0 - 2023-06-14

+ bump bounds
+ add upperbounds from cabal-gen-bounds
+ add stackage ci

## 1.1.0 - 2022-07-15

+ Add new directive ManifestSrc

## 1.0.2 - 2022-07-12

+ Export the new functions

## 1.0.1 - 2022-07-12

+ Add missing extra source files to cabal file

## 1.0.0 - 2022-07-12

+ Add Attrs variants of add script.
+ Add changelog, bump to version 1, upload to hackage

## 0.0.0 - ???

+ Initial release on github
