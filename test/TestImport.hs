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
