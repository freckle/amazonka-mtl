{-# LANGUAGE DerivingVia #-}

module Control.Monad.AWSSpec
  ( spec
  ) where

import Prelude

import Amazonka.Data.Text (ToText (..))
import Amazonka.S3.ListBuckets
import Amazonka.S3.Types.Bucket
import Control.Exception (Exception (..), try)
import Control.Lens ((&), (?~), (^.))
import Control.Monad.AWS
import Control.Monad.AWS.Matchers
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Test.Hspec

listBucketsMatching :: (MonadIO m, MonadAWS m) => Text -> m [Bucket]
listBucketsMatching p = do
  resp <- send newListBuckets
  pure $ maybe [] (filter match) $ resp ^. listBucketsResponse_buckets
 where
  match b = p `T.isPrefixOf` toText (b ^. bucket_name)

spec :: Spec
spec = do
  describe "MockT" $ do
    it "can mock send" $ do
      now <- getCurrentTime

      let
        bucketA = newBucket now "a-bucket"
        bucketB = newBucket now "b-bucket"
        bucketC = newBucket now "c-bucket"
        buckets = [bucketA, bucketB, bucketC]
        matcher =
          SendMatcher (== newListBuckets) $
            Right $
              newListBucketsResponse 200 & listBucketsResponse_buckets ?~ buckets

      result <-
        runMockT $ withMatcher matcher $ listBucketsMatching "b-"

      result `shouldBe` [bucketB]

    it "reports useful failures on un-matched requests" $ do
      result <- try @UnmatchedRequestError $ runMockT (listBucketsMatching "b-")

      case result of
        Left ex ->
          displayException ex
            `shouldBe` mconcat
              [ "Unexpected AWS request made within MockT: ListBuckets\n"
              , "Use withMatcher to add a Matcher for this request"
              ]
        Right resp ->
          expectationFailure $
            "UnmatchedRequestError expected, but got response: "
              <> show resp
