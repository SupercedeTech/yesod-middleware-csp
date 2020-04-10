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

  describe "choosing CSP version" $ do

    it "picks v2 for Safari" $ do
      request $ useSafari >> setMethod "GET" >> setUrl Example2
      assertHeader "X-Webkit-CSP" "script-src *"

    it "picks v2 for Mobile Safari" $ do
      request $ useMobileSafari >> setMethod "GET" >> setUrl Example2
      assertHeader "X-Webkit-CSP" "script-src *"

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
      assertCSP $ "script-src 'strict-dynamic' 'nonce-" <> fromJust h <> "'"
      assertEq "match" h (encodeUtf8 <$> a)

  describe "when the user includes a remote JavaScript asset" $

    it "adds the nonce to both the script tag and the header" $ do
      get Example6
      h <- getNonceFromCSP
      a <- getAttrFromResponseMatch "script" "nonce"
      assertCSP $ "script-src 'strict-dynamic' 'nonce-" <> fromJust h <> "'"
      assertEq "match" h (encodeUtf8 <$> a)

  describe "when using CSP2" $ do

    it "does not use 'strict-dynamic'" $ do
      request $ useSafari >> setMethod "GET" >> setUrl Example5
      h <- lookupResponseHeader "X-Webkit-CSP"
      liftIO $ fromJust h `shouldNotSatisfy` isInfixOf "'strict-dynamic'"

    it "does not contain a nonce" $ do
      request $ useSafari >> setMethod "GET" >> setUrl Example5
      h <- lookupResponseHeader "X-Webkit-CSP"
      liftIO $ fromJust h `shouldNotSatisfy` isInfixOf "'nonce-"

    it "adds remote script hosts" $ do
      request $ useSafari >> setMethod "GET" >> setUrl Example6
      h <- lookupResponseHeader "X-Webkit-CSP"
      liftIO $ fromJust h `shouldSatisfy` isInfixOf "https://example.com"
