{-# LANGUAGE DerivingVia #-}

-- | Concrete reader monad over 'Matchers'
--
-- Its 'MonadAWS' instance can be used in tests where you don't have or
-- want your own test-app transformer:
--
-- @
-- import "Amazonka.S3"
-- import "Control.Monad.AWS"
--
-- spec :: Spec
-- spec = do
--   describe "someAction" $ do
--     it "works" $ do
--       let matcher =
--             'SendMatcher' (const @_ @ListBuckets True) -- match all calls
--               $ Right
--               $ 'newListBucketsResponse'               -- return no buckets
--               & 'listBucketsResponse_buckets' ?~ []
--
--       names <- 'runMockT' $ 'withMatcher' matcher $ someAction
--       names `shouldBe` []
--
-- someAction :: (MonadIO m, 'MonadAWS') m => m [BucketName]
-- someAction = do
--   resp <- 'send' 'newListBuckets'
--   pure
--     $ maybe [] (map (^. bucket_name))
--     $ resp ^. listBucketsResponse_buckets
-- @
module Control.Monad.AWS.MockT
  ( MockT
  , runMockT

    -- * Setting up 'Matchers'
  , Matcher (..)
  , withMatcher
  , withMatchers
  ) where

import Prelude

import Control.Monad.AWS.Class
import Control.Monad.AWS.Matchers
import Control.Monad.AWS.ViaMock
import Control.Monad.IO.Unlift
import Control.Monad.Reader

-- |
--
-- @since 0.1.0.0
newtype MockT m a = MockT
  { unMockT :: ReaderT Matchers m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader Matchers
    )
  deriving (MonadAWS) via (MockAWS (MockT m))

-- |
--
-- @since 0.1.0.0
runMockT :: MockT m a -> m a
runMockT f = runReaderT (unMockT f) mempty
