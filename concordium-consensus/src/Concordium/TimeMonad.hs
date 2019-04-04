{-# LANGUAGE DefaultSignatures #-}
module Concordium.TimeMonad where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.Time

import Concordium.Logger

class Monad m => TimeMonad m where
    currentTime :: m UTCTime
    default currentTime :: MonadIO m => m UTCTime
    currentTime = liftIO (getCurrentTime)

instance TimeMonad IO

instance TimeMonad m => TimeMonad (StateT s m) where
    currentTime = lift currentTime

instance (TimeMonad m, Monoid w) => TimeMonad (RWST r w s m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (MaybeT m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (LoggerT m) where
    currentTime = lift currentTime