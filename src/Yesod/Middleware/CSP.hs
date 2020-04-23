{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Middleware.CSP
  ( CSPNonce (..)
  , Directive (..)
  , Source (..)
  , addCSP
  , addCSPMiddleware
  , addScript
  , addScriptEither
  , addScriptRemote
  , getRequestNonce
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import ClassyPrelude
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Client.Conduit (host, parseRequest_, secure)
import Web.UAParser (parseUA, uarFamily)
import Yesod.Core hiding (addScript, addScriptEither, addScriptRemote)

type DirSet = Map Directive (Set Source)

newtype CSPNonce = CSPNonce { unCSPNonce :: Text } deriving (Eq, Ord)

newtype CSPHost = CSPHost { unCSPHost :: Text } deriving (Eq, Ord)

data CSPVersion = CSP2 | CSP3 deriving (Eq)

data Source
  = Wildcard
  | None
  | Self
  | DataScheme
  | Host Text
  | Https
  | Http
  | UnsafeInline
  | UnsafeEval
  | StrictDynamic
  | Nonce Text
  deriving (Eq, Ord)

instance IsString Source where
  fromString = Host . pack

instance Show Source where
  show Wildcard = "*"
  show None = "'none'"
  show Self = "'self'"
  show DataScheme = "data:"
  show (Host h) = unpack h
  show Https = "https:"
  show Http = "http:"
  show UnsafeInline = "'unsafe-inline'"
  show UnsafeEval = "'unsafe-eval'"
  show StrictDynamic = "'strict-dynamic'"
  show (Nonce n) = "'nonce-" <> unpack n <> "'"

data Directive
  = DefaultSrc
  | StyleSrc
  | ScriptSrc
  | ObjectSrc
  | ImgSrc
  | FontSrc
  | ConnectSrc
  | MediaSrc
  | FrameSrc
  | FormAction
  | FrameAncestors
  | BaseURI
  | ReportURI
  deriving (Eq, Ord)

instance Show Directive where
  show DefaultSrc = "default-src"
  show StyleSrc = "style-src"
  show ScriptSrc = "script-src"
  show ObjectSrc = "object-src"
  show ImgSrc = "img-src"
  show FontSrc = "font-src"
  show ConnectSrc = "connect-src"
  show MediaSrc = "media-src"
  show FrameSrc = "frame-src"
  show FormAction = "form-action"
  show FrameAncestors = "frame-ancestors"
  show BaseURI = "base-uri"
  show ReportURI = "report-uri"

cachedDirectives :: MonadHandler m => m DirSet
cachedDirectives = fromMaybe M.empty <$> cacheGet

cachedHosts :: MonadHandler m => m (Set CSPHost)
cachedHosts = fromMaybe S.empty <$> cacheGet

-- | Add a directive to the current Content-Security Policy
addCSP :: MonadWidget m => Directive -> Source -> m ()
addCSP d s = cachedDirectives
  >>= cacheSet . M.insertWith insertSource d (S.singleton s)

insertSource :: Set Source -> Set Source -> Set Source
insertSource a b = case S.toList a of
  [ Wildcard ] -> a
  [ None ]     -> a
  _            -> a <> S.filter (`notElem` [Wildcard, None]) b

showSources :: Set Source -> Text
showSources = pack . unwords . map show . S.toList

showDirective :: (Directive, Set Source) -> Text
showDirective (d, s) = tshow d <> " " <> showSources s

showDirectives :: DirSet -> Text
showDirectives = intercalate "; " . map showDirective . M.toList

cspHeaderName :: CSPVersion -> Text
cspHeaderName CSP2 = "X-Webkit-CSP"
cspHeaderName CSP3 = "Content-Security-Policy"

determineCSPVersion :: Maybe ByteString -> CSPVersion
determineCSPVersion mUA = case mUA >>= fmap uarFamily . parseUA of
  Just "Mobile Safari" -> CSP2
  Just "Safari"        -> CSP2
  _                    -> CSP3

augmentCSP2 :: Set CSPHost -> DirSet -> DirSet
augmentCSP2 h d =
  let srcs = S.map (Host . unCSPHost) h
   in if S.null h then d else M.insertWith insertSource ScriptSrc srcs d

augmentCSP3 :: Maybe CSPNonce -> DirSet -> DirSet
augmentCSP3 Nothing d = d
augmentCSP3 (Just (CSPNonce n)) d =
  let srcs = S.fromList [ Nonce n, StrictDynamic ]
   in M.insertWith insertSource ScriptSrc srcs d

addCSPMiddleware :: (HandlerFor m) a -> (HandlerFor m) a
addCSPMiddleware handler = do
  (r, n, h) <- (,,) <$> handler <*> cacheGet <*> cachedHosts
  v <- determineCSPVersion <$> lookupHeader "User-Agent"
  d <- (if v == CSP2 then augmentCSP2 h else augmentCSP3 n) <$> cachedDirectives
  addHeader (cspHeaderName v) (showDirectives d)
  pure r

-- | Get a nonce for the request
--
-- CSP nonces must be unique per request, but they do not need to be unique
-- amongst themselves. This function checks the per-request cache to see if we
-- have already generated a nonce. If we have, we use the cached value. If this
-- is the first call to this function for the request, we generate a new
-- @CSPNonce@ by base64-encoding a UUIDV4 value.
--
-- n.b. It is not important to use a high-quality random value to generate the
-- nonce, but @Data.UUID.V4.nextRandom@ just happens to be faster than
-- @System.Random.randomIO@.
getRequestNonce :: MonadHandler m => m CSPNonce
getRequestNonce = cacheGet >>= maybe mkNonce pure
  where mkNonce = do
          let decode = decodeUtf8 . B64.encode . toASCIIBytes
          nonce <- CSPNonce . decode <$> liftIO nextRandom
          cacheSet nonce
          pure nonce

-- | Add a local JavaScript asset to the widget
--
-- This is intended to a be a drop-in replacement for
-- @Yesod.Core.Widget.addScript@. It takes the nonce generated for the current
-- request and embeds it as an HTML attribute in the script tag.
addScript :: MonadWidget m => Route (HandlerSite m) -> m ()
addScript route = do
  nonce <- getRequestNonce
  addScriptAttrs route [("nonce", unCSPNonce nonce)]

-- | Add a remote JavaScript asset to the widget
--
-- The same notes for @addScript@ apply here.
addScriptRemote :: MonadWidget m => Text -> m ()
addScriptRemote uri = do
  nonce <- getRequestNonce
  hosts <- fromMaybe S.empty <$> cacheGet
  let r = parseRequest_ $ unpack uri
      h = decodeUtf8 . host $ r
      p = if secure r then "https://" else "http://"
  cacheSet $ S.insert (CSPHost (p <> h)) hosts
  addScriptRemoteAttrs uri [("nonce", unCSPNonce nonce)]

addScriptEither :: MonadWidget m => Either (Route (HandlerSite m)) Text -> m ()
addScriptEither = either addScript addScriptRemote
