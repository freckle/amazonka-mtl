{-# LANGUAGE UndecidableInstances #-}

-- | @DerivingVia@ machinery for mocking AWS interactions in tests
--
-- This module assumes your specs run in a custom transformer that can provide a
-- reader environment. If you define 'HasMatchers' for that environment, you can
-- then derive 'MonadAWS' for this transformer via 'MockAWS'.
--
-- For a more explicit alternative, see "Control.Monad.AWS.MockT".
--
-- === Example:
--
-- Assuming you have some implementation you wanted to test:
--
-- @
-- getBucketsByPrefix :: (MonadIO m, MonadAWS) m => Text -> m [Bucket]
-- getBucketsByPrefix p = do
--   resp <- send newListBuckets
--   pure
--    $ maybe [] (filter matchesPrefix)
--    $ resp ^. listBucketsResponse_buckets
--  where
--   matchesPrefix b = p \`T.isPrefixOf\` toText (b ^. bucket_name)
-- @
--
-- And assuming you've set up your example monad with 'MonadAWS' via 'MockAWS',
-- you can now test it without talking to AWS:
--
-- @
-- describe "getBucketsByPrefix" $ do
--   it "works" $ do
--     now <- getCurrentTime
--
--     let
--       bucketA = newBucket now "a-bucket"
--       bucketB = newBucket now "b-bucket"
--       bucketC = newBucket now "c-bucket"
--       buckets = [bucketA, bucketB, bucketC]
--       matcher =
--         'SendMatcher' (== newListBuckets)
--          $ Right
--          $ newListBucketsResponse 200
--          & listBucketsResponse_buckets ?~ buckets
--
--     'withMatcher' matcher $ do
--       buckets <- getBucketsByPrefix "b-"
--       buckets `shouldBe` [bucketB]
-- @
module Control.Monad.AWS.ViaMock
  ( Matchers
  , HasMatchers (..)
  , Matcher (..)
  , withMatcher
  , withMatchers
  , MockAWS (..)
  ) where

import Prelude

import Amazonka (AuthEnv (..))
import Control.Monad.AWS.Class
import Control.Monad.AWS.Matchers
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..))

-- |
--
-- @since 0.1.0.0
newtype MockAWS m a = MockAWS
  { unMockAWS :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader env
    )

instance (MonadIO m, MonadReader env m, HasMatchers env) => MonadAWS (MockAWS m) where
  sendEither = matchSend
  awaitEither = matchAwait
  withAuth = ($ fakeAuthEnv)
  localEnv _ = id

fakeAuthEnv :: AuthEnv
fakeAuthEnv =
  AuthEnv
    { accessKeyId = "mock-aws-access-key-id"
    , secretAccessKey = "mock-aws-secret-key"
    , sessionToken = Just "mock-aws-sessin-token"
    , expiration = Nothing
    }
