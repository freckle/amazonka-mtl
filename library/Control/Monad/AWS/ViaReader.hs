{-# LANGUAGE UndecidableInstances #-}

-- | @DerivingVia@ machinery for adding Amazonka functionality to a reader-like
-- transformer
--
-- This is useful when you have a /ReaderT-IO/-like stack implemented with an
-- @AppT@ wrapper to which you will add the 'MonadAWS' instance:
--
-- @
-- newtype AppT m a = AppT
--   -- ...
--   deriving MonadAWS via (ReaderAWS (AppT m))
-- @
--
-- Complete example:
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- module Main (main) where
--
-- import qualified "Amazonka"
-- import "Control.Lens"
-- import "Control.Monad.AWS"
-- import "Control.Monad.AWS.ViaReader"
-- import "Control.Monad.Reader"
-- import "Control.Monad.Trans.Resource"
--
-- data App = App
--   { -- ...
--   , appAWS :: 'Amazonka.Env'
--   }
--
-- instance HasEnv App where
--   envL = lens appAWS $ \x y -> x { appAWS = y }
--
-- newtype AppT m a = AppT
--   { unAppT :: ReaderT App (ResourceT m) a
--   }
--   deriving newtype
--     ( Functor
--     , Applicative
--     , Monad
--     , MonadIO
--     , MonadUnliftIO
--     , MonadResource
--     , MonadReader App
--     )
--   deriving 'MonadAWS' via ('ReaderAWS' (AppT m))
--
-- runAppT :: MonadUnliftIO m => AppT m a -> App -> m a
-- runAppT f app = runResourceT $ runReaderT (unAppT f) app
--
-- main :: IO ()
-- main = do
--   app <- undefined
--   runAppT someAction app
--
-- someAction :: (MonadIO m, 'MonadAWS' m) => m ()
-- someAction = do
--   resp <- 'send' 'newListBuckets'
--   liftIO $ print resp
-- @
module Control.Monad.AWS.ViaReader
  ( Env
  , HasEnv (..)
  , ReaderAWS (..)
  ) where

import Prelude

import qualified Amazonka.Auth as Amazonka
import Amazonka.Env
import qualified Amazonka.Send as Amazonka
import Control.Lens (Lens', to, view, (%~))
import Control.Monad.AWS.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadResource)
import Data.Functor.Identity (runIdentity)

-- |
--
-- @since 0.1.0.0
class HasEnv env where
  envL :: Lens' env Env

-- |
--
-- @since 0.1.0.0
instance HasEnv Env where
  envL = id

-- |
--
-- @since 0.1.0.0
newtype ReaderAWS m a = ReaderAWS
  { unReaderAWS :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadResource
    , MonadReader env
    )

instance (MonadResource m, MonadReader env m, HasEnv env) => MonadAWS (ReaderAWS m) where
  sendEither req = do
    env <- view envL
    Amazonka.sendEither env req

  awaitEither w a = do
    env <- view envL
    Amazonka.awaitEither env w a

  withAuth f = do
    auth <- view $ envL . env_auth . to runIdentity
    Amazonka.withAuth auth f

  modified f = local $ envL %~ f
