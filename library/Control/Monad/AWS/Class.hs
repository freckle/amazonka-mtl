module Control.Monad.AWS.Class
  ( MonadAWS (..)
  )
where

import Amazonka.Prelude

import Amazonka (AuthEnv)
import Amazonka.Core (AWSRequest, AWSResponse, Error)
import Amazonka.Env (Env)
import qualified Amazonka.Waiter as Waiter
import Data.Typeable (Typeable)

-- | Typeclass for making AWS requests via "Amazonka"
--
-- For out-of-the-box transformers, see:
--
-- - "Control.Monad.AWS.EnvT"
-- - "Control.Monad.AWS.MockT"
--
-- For @DerivingVia@ usage, see:
--
-- - "Control.Monad.AWS.ViaReader"
-- - "Control.Monad.AWS.ViaMock"
class Monad m => MonadAWS m where
  -- |
  --
  -- @since 0.1.0.0
  sendEither
    :: (AWSRequest a, Typeable a, Typeable (AWSResponse a))
    => a
    -> m (Either Error (AWSResponse a))

  -- |
  --
  -- @since 0.1.0.0
  awaitEither
    :: (AWSRequest a, Typeable a)
    => Waiter.Wait a
    -> a
    -> m (Either Error Waiter.Accept)

  -- |
  --
  -- @since 0.1.1.0
  withAuth :: (AuthEnv -> m a) -> m a

  -- |
  --
  -- @since 0.1.1.0
  modified :: (Env -> Env) -> m a -> m a
