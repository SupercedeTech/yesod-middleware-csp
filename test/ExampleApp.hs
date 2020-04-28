{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ExampleApp where

import ClassyPrelude.Yesod hiding (addScript, addScriptRemote)
import Yesod.Core.Types (Logger)
import Yesod.EmbeddedStatic
import Yesod.Middleware.CSP

data ExampleApp = ExampleApp
  { appLogger :: Logger
  , getStatic :: EmbeddedStatic
  }

mkEmbeddedStatic False "eStatic" [ embedDir "test/static" ]

mkYesod "ExampleApp" [parseRoutes|
/static  StaticR  EmbeddedStatic  getStatic

/1 Example1 GET
/2 Example2 GET
/3 Example3 GET
/4 Example4 GET
/5 Example5 GET
/6 Example6 GET
/7 Example7 GET
/8 Example8 GET
|]

getExample1 :: Handler Html
getExample1 = defaultLayout $ do
  addCSP ScriptSrc Https
  addCSP ScriptSrc StrictDynamic
  addCSP ObjectSrc None
  toWidget $ asText ""

getExample2 :: Handler Html
getExample2 = defaultLayout $ do
  addCSP ScriptSrc None
  addCSP ScriptSrc Wildcard
  toWidget $ asText ""

getExample3 :: Handler Html
getExample3 = defaultLayout $ do
  addCSP ScriptSrc Wildcard
  addCSP ScriptSrc None
  toWidget $ asText ""

getExample4 :: Handler Html
getExample4 = defaultLayout $ do
  addCSP ScriptSrc Wildcard
  addCSP ScriptSrc None
  addCSP ScriptSrc DataScheme
  addCSP ScriptSrc Https
  toWidget $ asText ""

getExample5 :: Handler Html
getExample5 = defaultLayout $ do
  addScript $ StaticR js_test_js
  toWidget $ asText ""

getExample6 :: Handler Html
getExample6 = defaultLayout $ do
  addScriptRemote "https://example.com/test.js"
  toWidget $ asText ""

getExample7 :: Handler Html
getExample7 = defaultLayout $ toWidget $ asText ""

getExample8 :: Handler Html
getExample8 = defaultLayout $ do
  addScript $ StaticR js_test_js
  addCSP ScriptSrc None
  toWidget $ asText ""

instance Yesod ExampleApp where
  yesodMiddleware = addCSPMiddleware
  addStaticContent = embedStaticContent getStatic StaticR Right
