{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Yesod.Middleware.CSP
  ( CombineSettings (..)
  , CSPNonce (..)
  , Directive (..)
  , Source (..)
  , addCSP
  , addCSPMiddleware
  , addScript
  , addScriptEither
  , addScriptRemote
  , combineScripts'
  , combineStylesheets'
  , getRequestNonce
  ) where

import ClassyPrelude
import Conduit hiding (Source)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.UUID (toASCIIBytes)
import Data.UUID.V4 (nextRandom)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import System.Directory
import System.FilePath (takeDirectory)
import qualified System.FilePath as F
import Yesod.Core hiding (addScript, addScriptEither, addScriptRemote)
import Yesod.Static hiding
       (CombineSettings, combineScripts', combineStylesheets')

type DirSet = Map Directive (Set Source)

newtype CSPNonce = CSPNonce { unCSPNonce :: Text } deriving (Eq, Ord)

data Source
  = Wildcard
  | None
  | Self
  | DataScheme
  | BlobScheme
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
  show BlobScheme = "blob:"
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

-- | Add a directive to the current Content-Security Policy
addCSP :: MonadWidget m => Directive -> Source -> m ()
addCSP d s = cachedDirectives
  >>= cacheSet . M.insertWith insertSource d (S.singleton s)

insertSource :: Set Source -> Set Source -> Set Source
insertSource a b = case S.toList a of
  [ None ]     -> a
  _            -> a <> S.filter (`notElem` [None]) b

showSources :: Set Source -> Text
showSources = pack . unwords . map show . S.toList

showDirective :: (Directive, Set Source) -> Text
showDirective (d, s) = tshow d <> " " <> showSources s

showDirectives :: DirSet -> Text
showDirectives = intercalate "; " . map showDirective . M.toList

cspHeaderName :: Text
cspHeaderName = "Content-Security-Policy"

augment :: Maybe CSPNonce -> DirSet -> DirSet
augment Nothing d = d
augment (Just (CSPNonce n)) d =
  let srcs = S.fromList [ Nonce n ]
      existingScriptSrcs = S.toList (fromMaybe S.empty (lookup ScriptSrc d))
   in if any (`elem` existingScriptSrcs) [ None ]
      then d
      else M.insertWith insertSource ScriptSrc srcs d

addCSPMiddleware :: (HandlerFor m) a -> (HandlerFor m) a
addCSPMiddleware handler = do
  (r, n) <- (,) <$> handler <*> cacheGet
  d <- augment n <$> cachedDirectives
  when (not (null (showDirectives d))) $
    addHeader cspHeaderName (showDirectives d)
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
  addScriptRemoteAttrs uri [("nonce", unCSPNonce nonce)]

addScriptEither :: MonadWidget m => Either (Route (HandlerSite m)) Text -> m ()
addScriptEither = either addScript addScriptRemote

data CombineSettings = CombineSettings
  { csStaticDir :: FilePath
  -- ^ File path containing static files.
  , csCssPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
  -- ^ Post processing to be performed on CSS files.
  , csJsPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
  -- ^ Post processing to be performed on Javascript files.
  , csCssPreProcess :: TL.Text -> IO TL.Text
  -- ^ Pre-processing to be performed on CSS files.
  , csJsPreProcess :: TL.Text -> IO TL.Text
  -- ^ Pre-processing to be performed on Javascript files.
  , csCombinedFolder :: FilePath
  -- ^ Subfolder to put combined files into.
  }

instance Default CombineSettings where
  def = CombineSettings
      { csStaticDir = "static"
      {- Disabled due to: https://github.com/yesodweb/yesod/issues/623
      , csCssPostProcess = \fps ->
            either (error . (errorIntro fps)) (return . TLE.encodeUtf8)
          . flip luciusRTMinified []
          . TLE.decodeUtf8
      -}
      , csCssPostProcess = const return
      , csJsPostProcess = const return
         -- FIXME The following borders on a hack. With combining of files,
         -- the final location of the CSS is no longer fixed, so relative
         -- references will break. Instead, we switched to using /static/
         -- absolute references. However, when served from a separate domain
         -- name, this will break too. The solution is that, during
         -- development, we keep /static/, and in the combining phase, we
         -- replace /static with a relative reference to the parent folder.
      , csCssPreProcess =
            return
          . TL.replace "'/static/" "'../"
          . TL.replace "\"/static/" "\"../"
      , csJsPreProcess = return
      , csCombinedFolder = "combined"
      }

data CombineType = JS | CSS

combineStatics' :: CombineType
                -> CombineSettings
                -> [Route Static] -- ^ files to combine
                -> Q Exp
combineStatics' combineType CombineSettings {..} routes = do
    texts <- qRunIO $ runConduitRes
                    $ yieldMany fps
                   .| awaitForever readUTFFile
                   .| sinkLazy
    ltext <- qRunIO $ preProcess texts
    bs    <- qRunIO $ postProcess fps $ TLE.encodeUtf8 ltext
    let hash' = base64md5 bs
        suffix = csCombinedFolder </> hash' <.> extension
        fp = csStaticDir </> suffix
    qRunIO $ do
        createDirectoryIfMissing True $ takeDirectory fp
        L.writeFile fp bs
    let pieces = map T.unpack $ T.splitOn "/" $ T.pack suffix
    [|StaticRoute (map pack pieces) []|]
  where
    fps :: [FilePath]
    fps = map toFP routes
    toFP (StaticRoute pieces _) = csStaticDir </> F.joinPath (map T.unpack pieces)
    readUTFFile fp = sourceFile fp .| decodeUtf8C
    postProcess =
        case combineType of
            JS -> csJsPostProcess
            CSS -> csCssPostProcess
    preProcess =
        case combineType of
            JS -> csJsPreProcess
            CSS -> csCssPreProcess
    extension =
        case combineType of
            JS -> "js"
            CSS -> "css"

liftRoutes :: [Route Static] -> Q Exp
liftRoutes =
    fmap ListE . mapM go
  where
    go :: Route Static -> Q Exp
    go (StaticRoute x y) = [|StaticRoute $(liftTexts x) $(liftPairs y)|]

    liftTexts = fmap ListE . mapM liftT
    liftT t = [|pack $(TH.lift $ unpack t)|]

    liftPairs = fmap ListE . mapM liftPair
    liftPair (x, y) = [|($(liftT x), $(liftT y))|]

-- | Combine multiple CSS files together
combineStylesheets' :: Bool -- ^ development? if so, perform no combining
                    -> CombineSettings
                    -> Name -- ^ Static route constructor name, e.g. \'StaticR
                    -> [Route Static] -- ^ files to combine
                    -> Q Exp
combineStylesheets' development cs con routes
    | development = [| mapM_ (addStylesheet . $(return $ ConE con)) $(liftRoutes routes) |]
    | otherwise = [| addStylesheet $ $(return $ ConE con) $(combineStatics' CSS cs routes) |]


-- | Combine multiple JS files together
combineScripts' :: Bool -- ^ development? if so, perform no combining
                -> CombineSettings
                -> Name -- ^ Static route constructor name, e.g. \'StaticR
                -> [Route Static] -- ^ files to combine
                -> Q Exp
combineScripts' development cs con routes
    | development = [| mapM_ (addScript . $(return $ ConE con)) $(liftRoutes routes) |]
    | otherwise = [| addScript $ $(return $ ConE con) $(combineStatics' JS cs routes) |]
