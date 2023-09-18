module Control.Monad.AWS
  ( MonadAWS (..)
  , send
  , paginate
  , paginateEither
  , await

    -- * Concrete transformers
  , EnvT
  , runEnvT
  , MockT
  , runMockT
  ) where

import Amazonka.Prelude

import Amazonka.Core (AWSPager, AWSRequest, AWSResponse, Error)
import qualified Amazonka.Pager as Pager
import qualified Amazonka.Waiter as Waiter
import qualified Control.Exception as Exception
import Control.Monad.AWS.Class
import Control.Monad.AWS.EnvT
import Control.Monad.AWS.MockT
import Data.Conduit (ConduitM)
import qualified Data.Conduit as Conduit
import Data.Typeable (Typeable)

-- | Version of 'Amazonka.send' built on our 'sendEither'
--
-- @since 0.1.0.0
send
  :: ( MonadIO m
     , MonadAWS m
     , AWSRequest a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> m (AWSResponse a)
send = sendEither >=> hoistEither

-- | Version of 'Amazonka.paginateEither' built on our 'sendEither'
--
-- @since 0.1.0.0
paginateEither
  :: (MonadAWS m, AWSPager a, Typeable a, Typeable (AWSResponse a))
  => a
  -> ConduitM () (AWSResponse a) m (Either Error ())
paginateEither = go
 where
  go rq =
    lift (sendEither rq) >>= \case
      Left err -> pure (Left err)
      Right rs -> do
        Conduit.yield rs
        maybe (pure (Right ())) go (Pager.page rq rs)

-- | Version of 'Amazonka.paginate' built on our 'paginateEither'
--
-- @since 0.1.0.0
paginate
  :: ( MonadIO m
     , MonadAWS m
     , AWSPager a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> ConduitM () (AWSResponse a) m ()
paginate =
  paginateEither >=> hoistEither

-- | Version of 'Amazonka.await' built on our 'awaitEither'
--
-- @since 0.1.0.0
await
  :: (MonadIO m, MonadAWS m, AWSRequest a, Typeable a)
  => Waiter.Wait a
  -> a
  -> m Waiter.Accept
await wait = awaitEither wait >=> hoistEither

hoistEither :: MonadIO m => Either Error a -> m a
hoistEither = either (liftIO . Exception.throwIO) pure
