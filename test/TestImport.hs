{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestImport
  ( module TestImport
  , module X
  ) where

import qualified Data.Map.Strict as M
import qualified System.Log.FastLogger as F
import qualified Yesod
import qualified Yesod.Default.Config2 as C

import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Control.Monad (when)
import Data.CaseInsensitive (CI)
import ExampleApp as X
import Network.HTTP.Types.Header
import Network.Wai.Test (SResponse(..))
import Test.Hspec as X
import Yesod.Test as X
import Yesod.Test.TransversingCSS (Query, findAttributeBySelector)

withApp :: SpecWith (TestApp ExampleApp) -> Spec
withApp = before $ do
  logF <- F.newStdoutLoggerSet 1 >>= C.makeYesodLogger
  pure (ExampleApp logF eStatic, Yesod.defaultMiddlewaresNoLogging)

safariUA :: ByteString
safariUA = encodeUtf8 . pack . unwords $
  [ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8)"
  , "AppleWebKit/534.59.10 (KHTML, like Gecko) Version/5.1.9"
  , "Safari/534.59.10"
  ]

mobileSafariUA :: ByteString
mobileSafariUA = encodeUtf8 . pack . unwords $
  [ "Mozilla/5.0 (iPhone; CPU iPhone OS 13_1_2 like Mac OS X)"
  , "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.0.1"
  , "Mobile/15E148 Safari/604.1"
  ]

useSafari :: RequestBuilder site ()
useSafari = addRequestHeader (hUserAgent, safariUA)

useMobileSafari :: RequestBuilder site ()
useMobileSafari = addRequestHeader (hUserAgent, mobileSafariUA)

assertCSP :: HasCallStack => ByteString -> YesodExample site ()
assertCSP = assertHeader "Content-Security-Policy"

lookupResponseHeader :: CI ByteString -> YesodExample site (Maybe ByteString)
lookupResponseHeader k = (lookupHeader =<<) <$> getResponse
  where lookupHeader = lookup k . M.fromList . simpleHeaders

getNonceFromCSP :: YesodExample site (Maybe ByteString)
getNonceFromCSP = lookupResponseHeader "Content-Security-Policy"
  >>= \h -> pure $ filter (isPrefixOf "'nonce-") . words . decodeUtf8 <$> h
    >>= fmap (encodeUtf8 . dropEnd 1 . dropPrefix "'nonce-") . headMay

getAttrFromResponseMatch :: Query -> Text -> YesodExample site (Maybe Text)
getAttrFromResponseMatch q a = do
  body <- getAttr . simpleBody <<$>> getResponse
  pure $ body >>= either (const Nothing) headMay >>= headMay
  where getAttr b = findAttributeBySelector b q a

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
