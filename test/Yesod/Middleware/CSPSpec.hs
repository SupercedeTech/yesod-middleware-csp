{-# LANGUAGE OverloadedStrings #-}

module Yesod.Middleware.CSPSpec (spec) where

import Data.Maybe (fromJust)
import TestImport

spec :: Spec
spec = withApp $ do

  describe "composing directives" $

    it "groups sources by directive" $ do
      get Example1
      assertCSP "script-src https: 'strict-dynamic'; object-src 'none'"

  describe "with an exclusive directive" $ do

    it "the last applied exclusive directive wins" $ do
      get Example2
      assertCSP "script-src *"

      get Example3
      assertCSP "script-src 'none'"

    it "subsequent non-exlusive directives disable exclusive directives" $ do
      get Example4
      assertCSP "script-src data: https:"

  describe "when the user includes a local JavaScript asset" $

    it "adds the nonce to both the script tag and the header" $ do
      get Example5
      h <- getNonceFromCSP
      a <- getAttrFromResponseMatch "script" "nonce"
      assertCSP $ "script-src 'nonce-" <> fromJust h <> "'"
      assertEq "match" h (encodeUtf8 <$> a)

  describe "when the user includes a remote JavaScript asset" $

    it "adds the nonce to both the script tag and the header" $ do
      get Example6
      h <- getNonceFromCSP
      a <- getAttrFromResponseMatch "script" "nonce"
      assertCSP $ "script-src 'nonce-" <> fromJust h <> "'"
      assertEq "match" h (encodeUtf8 <$> a)

  describe "when no CSP directives are added" $

    it "does not add a CSP header" $ do
      get Example7
      assertNoHeader "Content-Security-Policy"

  describe "when non-exclusive directives have been set" $

    it "they are overwritten by exclusive directives" $ do
      get Example8
      assertCSP "script-src 'none'"
