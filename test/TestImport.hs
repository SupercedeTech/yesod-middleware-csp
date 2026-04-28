{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TestImport
  ( module TestImport
  , module X
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import qualified System.Log.FastLogger as F
import qualified Yesod
import qualified Yesod.Default.Config2 as C

import Data.CaseInsensitive (CI)
import ExampleApp as X
import Network.Wai.Test (SResponse(..))
import Test.Hspec as X
import Yesod.Test as X
import Yesod.Test.TransversingCSS (Query, findAttributeBySelector)

withApp :: SpecWith (TestApp ExampleApp) -> Spec
withApp = before $ do
  logF <- F.newStdoutLoggerSet 1 >>= C.makeYesodLogger
  pure (ExampleApp logF eStatic, Yesod.defaultMiddlewaresNoLogging)

assertCSP :: HasCallStack => BS.ByteString -> YesodExample site ()
assertCSP = assertHeader "Content-Security-Policy"

lookupResponseHeader :: CI BS.ByteString -> YesodExample site (Maybe BS.ByteString)
lookupResponseHeader k = (lookupHeader =<<) <$> getResponse
  where lookupHeader = M.lookup k . M.fromList . simpleHeaders

getNonceFromCSP :: YesodExample site (Maybe BS.ByteString)
getNonceFromCSP = lookupResponseHeader "Content-Security-Policy"
  >>= \h -> pure $ filter (T.isPrefixOf "'nonce-") . T.words . decodeUtf8 <$> h
    >>= fmap (encodeUtf8 . T.dropEnd 1 . dropPrefix "'nonce-") . listToMaybe

dropPrefix :: Text -> Text -> Text
dropPrefix prefix t = case T.stripPrefix prefix t of
  Just rest -> rest
  Nothing   -> t

getAttrFromResponseMatch :: Query -> Text -> YesodExample site (Maybe Text)
getAttrFromResponseMatch q a = do
  body <- getAttr . simpleBody <<$>> getResponse
  pure $ body >>= either (const Nothing) listToMaybe >>= listToMaybe
  where getAttr b = findAttributeBySelector b q a

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
