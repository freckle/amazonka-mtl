{-# LANGUAGE DerivingVia #-}

-- | Concrete reader monad over 'Env'
--
-- Its 'MonadAWS' instance can be used in situations where you don't have or
-- want your own app transformer:
--
-- @
-- import qualified "Amazonka"
-- import "Amazonka.S3"
-- import "Control.Monad.AWS"
--
-- main :: IO ()
-- main = do
--   env <- 'Amazonka.newEnv' 'Amazonka.discover'
--   'runEnvT' someAction env
--
-- someAction :: (MonadIO m, 'MonadAWS' m) => m ()
-- someAction = do
--   resp <- 'send' 'newListBuckets'
--   liftIO $ print resp
-- @
module Control.Monad.AWS.EnvT
  ( EnvT
  , runEnvT
  )
where

import Prelude

import Control.Monad.AWS.Class
import Control.Monad.AWS.ViaReader
import Control.Monad.Reader
import Control.Monad.Trans.Resource

-- |
--
-- @since 0.1.0.0
newtype EnvT m a = EnvT
  { unEnvT :: ReaderT Env (ResourceT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadResource
    , MonadReader Env
    )
  deriving (MonadAWS) via (ReaderAWS (EnvT m))

-- |
--
-- @since 0.1.0.0
runEnvT :: MonadUnliftIO m => EnvT m a -> Env -> m a
runEnvT f = runResourceT . runReaderT (unEnvT f)
