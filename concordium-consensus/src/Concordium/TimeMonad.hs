{-# LANGUAGE DefaultSignatures #-}
module Concordium.TimeMonad where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import qualified Control.Monad.RWS.Strict as Strict
import Data.Time

import Concordium.Logger

import Concordium.GlobalState.BlockState (BSMTrans)

class Monad m => TimeMonad m where
    currentTime :: m UTCTime
    default currentTime :: MonadIO m => m UTCTime
    currentTime = liftIO (getCurrentTime)

instance TimeMonad IO

instance TimeMonad m => TimeMonad (StateT s m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (Strict.StateT s m) where
    currentTime = lift currentTime

instance (TimeMonad m, Monoid w) => TimeMonad (RWST r w s m) where
    currentTime = lift currentTime

instance (TimeMonad m, Monoid w) => TimeMonad (Strict.RWST r w s m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (MaybeT m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (ExceptT e m) where
    currentTime = lift currentTime

instance TimeMonad m => TimeMonad (LoggerT m) where
    currentTime = lift currentTime

instance (Monad (t m), MonadTrans t, TimeMonad m) => TimeMonad (BSMTrans t m) where
    currentTime = lift currentTime