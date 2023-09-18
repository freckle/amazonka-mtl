{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.AWS.Matchers
  ( Matchers
  , HasMatchers (..)
  , Matcher (..)
  , withMatcher
  , withMatchers
  , matchSend
  , matchAwait
  , UnmatchedRequestError (..)
  ) where

import Prelude

import Amazonka (AWSRequest, AWSResponse, Error)
import qualified Amazonka.Waiter as Waiter
import Control.Exception (Exception (..), throwIO)
import Control.Lens (Lens', view, (<>~))
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Typeable

-- | Define a response to provide for any matched requests
data Matcher where
  -- | Matches calls to 'send' where the given predicate holds
  --
  -- @since 0.1.0.0
  SendMatcher
    :: forall a
     . (AWSRequest a, Typeable a, Typeable (AWSResponse a))
    => (a -> Bool)
    -> Either Error (AWSResponse a)
    -> Matcher
  -- | Matches calls to 'await' where the given predicate holds
  --
  -- @since 0.1.0.0
  AwaitMatcher
    :: forall a
     . (AWSRequest a, Typeable a)
    => (Waiter.Wait a -> a -> Bool)
    -> Either Error Waiter.Accept
    -> Matcher

-- |
--
-- @since 0.1.0.0
newtype Matchers = Matchers
  { unMatchers :: [Matcher]
  }
  deriving newtype (Semigroup, Monoid)

-- |
--
-- @since 0.1.0.0
class HasMatchers env where
  matchersL :: Lens' env Matchers

-- |
--
-- @since 0.1.0.0
instance HasMatchers Matchers where
  matchersL = id

-- | Add a 'Matcher' for the duration of the block
--
-- @since 0.1.0.0
withMatcher :: (MonadReader env m, HasMatchers env) => Matcher -> m a -> m a
withMatcher = withMatchers . pure

-- | Add multiple 'Matcher's for the duration of the block
--
-- @since 0.1.0.0
withMatchers :: (MonadReader env m, HasMatchers env) => [Matcher] -> m a -> m a
withMatchers ms = local $ matchersL <>~ Matchers ms

-- |
--
-- @since 0.1.0.0
matchSend
  :: forall m env a
   . ( MonadIO m
     , MonadReader env m
     , HasMatchers env
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> m (Either Error (AWSResponse a))
matchSend req = throwUnmatched @a =<< firstMatcher go
 where
  go = \case
    SendMatcher matchReq resp -> do
      guard . matchReq =<< cast req
      cast resp
    AwaitMatcher {} -> Nothing

-- |
--
-- @since 0.1.0.0
matchAwait
  :: forall m env a
   . (MonadIO m, MonadReader env m, HasMatchers env, Typeable a)
  => Waiter.Wait a
  -> a
  -> m (Either Error Waiter.Accept)
matchAwait w req = throwUnmatched @a =<< firstMatcher go
 where
  go = \case
    SendMatcher {} -> Nothing
    AwaitMatcher matchReq acc -> do
      guard =<< matchReq <$> cast w <*> cast req
      cast acc

firstMatcher
  :: (MonadReader env m, HasMatchers env)
  => (Matcher -> Maybe a)
  -> m (Maybe a)
firstMatcher f = do
  matchers <- view matchersL
  pure $ listToMaybe $ mapMaybe f $ unMatchers matchers

-- |
--
-- @since 0.1.0.0
newtype UnmatchedRequestError = UnmatchedRequestError
  { unmatchedRequestType :: String
  }
  deriving anyclass (Exception)

-- Morally-speaking, Show should be reserved for a Haskell-like string
-- representation (derived is best), and displayException is where you make it
-- human-readable. Sadly, too many tools (*cough* hspec) use show instead of
-- displayException, and we want it to look nice there. Sigh.
--
-- https://github.com/hspec/hspec/issues/289
instance Show UnmatchedRequestError where
  show ex =
    "Unexpected AWS request made within MockT: "
      <> unmatchedRequestType ex
      <> "\nUse withMatcher to add a Matcher for this request"

throwUnmatched :: forall req m a. (MonadIO m, Typeable req) => Maybe a -> m a
throwUnmatched =
  maybe
    (liftIO $ throwIO $ UnmatchedRequestError $ show $ typeRep $ Proxy @req)
    pure
